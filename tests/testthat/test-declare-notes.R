test_that("a note is computed at its own position and outlives its data", {
  local({
    design <- declare_model(N = 100, Y = 1:100) +
      declare_sampling(S = complete_rs(N, n = 50)) +
      declare_notes(after_first = max(Y)) +
      declare_sampling(S = complete_rs(N, n = 5)) +
      declare_inquiry(note_value = after_first, current = max(Y),
                      n_rows = length(Y))
    for (i in 1:5) {
      out <- draw_estimands(design)
      value <- function(nm) out$estimand[out$inquiry == nm]
      expect_equal(value("n_rows"), 5)
      # The note is the first sample's maximum: at least the second sample's,
      # and drawn from a pool that still had 50 rows in it.
      expect_gte(value("note_value"), value("current"))
      expect_gte(value("note_value"), 51)
    }
  })
})

test_that("a note is fixed within a run and redrawn between runs", {
  local({
    design <- declare_model(N = 5, Y = rnorm(N)) +
      declare_notes(m = mean(Y)) +
      declare_inquiry(a = m, b = m)
    one <- draw_estimands(design)
    expect_equal(one$estimand[1], one$estimand[2])
    two <- draw_estimands(design)
    expect_false(isTRUE(all.equal(one$estimand[1], two$estimand[1])))
  })
})

test_that("a later note overwrites and leaves the earlier steps alone", {
  local({
    design <- declare_model(N = 4, Y = 1:4) +
      declare_notes(k = "first") +
      declare_inquiry(before = k) +
      declare_notes(k = "second") +
      declare_inquiry(after = k)
    out <- draw_estimands(design)
    expect_equal(out$estimand[out$inquiry == "before"], "first")
    expect_equal(out$estimand[out$inquiry == "after"], "second")
  })
})

test_that("a note may read the notes declared before it", {
  local({
    design <- declare_model(N = 10, Y = 1:10) +
      declare_notes(mx = max(Y), half = mx / 2) +
      declare_inquiry(h = half)
    expect_equal(draw_estimands(design)$estimand, 5)
  })
})

test_that("a note derived from a parameter follows a redesign", {
  local({
    design <- declare_parameters(m_arms = 3) +
      declare_notes(ks = seq_len(m_arms)[-1],
                    inquiry_names = paste0("ate_", ks),
                    term_names = paste0("factor(Z)", ks)) +
      declare_model(N = 60, u = rnorm(N)) +
      declare_inquiry(handler = function(data) {
        data.frame(inquiry = inquiry_names, estimand = 0)
      }) +
      declare_assignment(Z = complete_ra(N, conditions = seq_len(m_arms))) +
      declare_measurement(Y = u + as.numeric(Z)) +
      declare_estimator(Y ~ factor(Z), .method = lm_robust,
                        term = term_names, inquiry = inquiry_names)

    three <- run_design(design)
    expect_equal(three$inquiry, c("ate_2", "ate_3"))
    expect_equal(three$term, c("factor(Z)2", "factor(Z)3"))

    four <- run_design(redesign(design, m_arms = 4))
    expect_equal(four$inquiry, c("ate_2", "ate_3", "ate_4"))
    expect_equal(four$term, c("factor(Z)2", "factor(Z)3", "factor(Z)4"))
  })
})

test_that("a note is not a knob: it is out of design_parameters() and out of redesign()", {
  local({
    design <- declare_parameters(m_arms = 3) +
      declare_notes(ks = seq_len(m_arms)[-1]) +
      declare_model(N = 60) +
      declare_assignment(Z = complete_ra(N, conditions = seq_len(m_arms)))
    params <- design_parameters(design)
    expect_false("ks" %in% params$name)
    expect_true("m_arms" %in% params$name)
    expect_error(redesign(design, ks = 2:4), "is a note, not a parameter")
  })
})

test_that("design_notes() reports the notes and the expressions behind them", {
  local({
    design <- declare_parameters(m_arms = 3) +
      declare_notes(ks = seq_len(m_arms)[-1]) +
      declare_model(N = 10)
    notes <- design_notes(design)
    expect_equal(notes$name, "ks")
    expect_equal(notes$expr, "seq_len(m_arms)[-1]")
    expect_equal(notes$step, 2L)
    expect_equal(nrow(design_notes(declare_model(N = 10))), 0L)
  })
})

test_that("a column beats a note of the same name in the steps that follow", {
  local({
    design <- declare_model(N = 3, a = 1:3) +
      declare_notes(a = 99) +
      declare_inquiry(from_data = mean(a))
    expect_equal(draw_estimands(design)$estimand, 2)
  })
})

test_that("one name cannot be both a parameter and a note", {
  expect_error(declare_parameters(x = 1) + declare_notes(x = 2),
               "both a parameter and a note")
  expect_error(declare_notes(x = 2) + declare_parameters(x = 1),
               "both a parameter and a note")
})

test_that("every note must be named, and named once per call", {
  expect_error(declare_notes(1 + 1), "must be named")
  expect_error(declare_notes(k = 1, k = 2), "declared more than once")
})

test_that("a note declaration is skipped when the design runs", {
  local({
    design <- declare_model(N = 3, Y = 1:3) + declare_notes(k = max(Y))
    expect_equal(names(draw_data(design)), c("ID", "Y"))
    expect_equal(nrow(draw_data(design)), 3L)
  })
})

test_that("a failing note says which note failed", {
  local({
    design <- declare_model(N = 3) + declare_notes(k = no_such_object + 1)
    expect_error(draw_data(design), "Note `k` could not be computed")
  })
})

test_that("a note reaches a user-written .method out of its closure", {
  local({
    design <- declare_model(N = 40, u = rnorm(N), Z = rep(0:1, 20)) +
      declare_measurement(Y = u + Z) +
      declare_notes(scale_by = 100) +
      declare_estimator(
        Y ~ Z,
        .method = function(formula, data) {
          fit <- lm_robust(formula, data = data)
          fit$coefficients <- fit$coefficients * scale_by
          fit
        },
        term = "Z", label = "scaled")
    scaled <- run_design(design)$estimate
    plain <- run_design(
      declare_model(N = 40, u = rnorm(N), Z = rep(0:1, 20)) +
        declare_measurement(Y = u + Z) +
        declare_estimator(Y ~ Z, .method = lm_robust, term = "Z")
    )$estimate
    expect_gt(abs(scaled), 10 * abs(plain))
  })
})

test_that("a note reaches a handler and a term filter", {
  local({
    design <- declare_model(N = 40, u = rnorm(N), Z = rep(0:1, 20)) +
      declare_notes(shift = 10, keep = "Z") +
      declare_measurement(Y = u + Z) +
      declare_inquiry(handler = function(data) {
        data.frame(inquiry = "shifted", estimand = shift)
      }) +
      declare_estimator(Y ~ Z, .method = lm_robust, term = keep,
                        inquiry = "shifted")
    out <- run_design(design)
    expect_equal(out$estimand, 10)
    expect_equal(out$term, "Z")
  })
})

test_that("notes survive nested step-level draws", {
  local({
    design <- declare_model(N = 20, Y = rnorm(N)) +
      declare_notes(pop_mean = mean(Y)) +
      declare_sampling(S = complete_rs(N, n = 5), draws = 3) +
      declare_inquiry(pop_mean = pop_mean)
    out <- simulate_design(design)
    expect_equal(nrow(out), 3L)
    # The note sits above the fan-out, so every draw of the sample sees the
    # one population mean rather than its own.
    expect_equal(length(unique(out$estimand)), 1L)
  })
})

test_that("a note taken under a fan-out is taken once per branch", {
  local({
    design <- declare_model(N = 20, Y = rnorm(N)) +
      declare_sampling(S = complete_rs(N, n = 5), draws = 3) +
      declare_notes(samp_mean = mean(Y)) +
      declare_inquiry(samp_mean = samp_mean)
    out <- simulate_design(design)
    expect_equal(nrow(out), 3L)
    expect_equal(length(unique(out$estimand)), 3L)
  })
})

test_that("simulate_design() redraws a note on every simulation", {
  local({
    design <- declare_model(N = 5, Y = rnorm(N)) +
      declare_notes(m = mean(Y)) +
      declare_inquiry(m = m)
    out <- simulate_design(design, sims = 5)
    expect_equal(nrow(out), 5L)
    expect_equal(length(unique(out$estimand)), 5L)
  })
})

test_that("a design that takes no notes is untouched", {
  local({
    design <- simple_design()
    expect_equal(nrow(design_notes(design)), 0L)
    expect_true("ate" %in% design_parameters(design)$name)
  })
})
