# Capture: a design carries the workspace objects it reads.
#
# These tests need the environment topology of a user at the console, and a
# `test_that()` block does not have it. testthat sources a test file into an
# environment parented by the package namespace, so a declaration made inside a
# test reaches `globalenv()` only *through* that namespace, and
# `user_binding_env()` stops at package environments by design. Every test that
# exercises capture therefore evaluates its declaration in `globalenv()`
# explicitly, which is exactly what typing it at the prompt does. The barrier
# itself is pinned by the last test in this file.

# Bind names into globalenv() for the duration of one test, restoring whatever
# was there before.
with_globals <- function(..., code) {
  vals <- list(...)
  nms <- names(vals)
  existing <- nms[vapply(nms, exists, logical(1), envir = globalenv(),
                         inherits = FALSE)]
  old <- mget(existing, envir = globalenv())
  for (nm in nms) assign(nm, vals[[nm]], envir = globalenv())
  on.exit({
    rm(list = setdiff(nms, existing), envir = globalenv())
    for (nm in existing) assign(nm, old[[nm]], envir = globalenv())
  }, add = TRUE)
  force(code)
}

# Declare with globalenv() as the capture environment, as at the prompt.
declare_globally <- function(expr) eval(substitute(expr), envir = globalenv())

# Define in globalenv() too. A function built in the test frame and merely
# *assigned* into globalenv() keeps the test frame as its closure, and that
# chain reaches globalenv() only through the package namespace, which
# user_binding_env() stops at. Helpers written at the prompt do not have that
# problem, so the tests must not manufacture it.
define_globally <- function(expr) {
  eval(substitute(expr), envir = globalenv())
  invisible(NULL)
}

# Round-trip through disk and run `code` on the reloaded design while the
# globals are still gone, which is what a fresh session looks like.
#
# The assertion has to happen inside this call. An earlier version returned the
# reloaded design and restored the globals on the way out, so every expectation
# ran with the bindings back in place and passed whether or not the design had
# captured anything. Removing the implementation and watching these tests still
# pass is what caught it.
with_reloaded <- function(design, names, code) {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(design, f)
  keep <- mget(names, envir = globalenv())
  rm(list = names, envir = globalenv())
  on.exit({
    for (nm in names) assign(nm, keep[[nm]], envir = globalenv())
  }, add = TRUE)
  code(readRDS(f))
}

quo_env_of <- function(design, step = 1L, dot = 1L) {
  rlang::quo_get_env(attr(design[[step]], "dots")[[dot]])
}

# ---- the five headline cases -------------------------------------------------

test_that("a design does not change when the workspace changes under it", {
  with_globals(cap_b = 0.5, code = {
    design <- declare_globally(
      declare_model(N = 20, Y = rnorm(N) + cap_b) + declare_inquiry(m = mean(Y))
    )
    assign("cap_b", 99, envir = globalenv())
    expect_lt(mean(draw_data(design)$Y), 10)
  })
})

test_that("a design survives saveRDS/readRDS with the workspace gone", {
  with_globals(cap_N = 100, code = {
    design <- declare_globally(
      declare_model(N = cap_N, foo = rnorm(cap_N)) + declare_inquiry(m = mean(foo))
    )
    with_reloaded(design, "cap_N", function(back) {
      expect_equal(nrow(draw_data(back)), 100)
    })
  })
})

test_that("redesign still works on a reloaded design", {
  with_globals(cap_N = 100, code = {
    design <- declare_globally(
      declare_model(N = cap_N, foo = rnorm(cap_N)) + declare_inquiry(m = mean(foo))
    )
    with_reloaded(design, "cap_N", function(back) {
      expect_equal(nrow(draw_data(redesign(back, cap_N = 20))), 20)
    })
  })
})

test_that("a helper function carries its own environment (#293)", {
  # From the 2026-05 audit: DeclareDesign's env_deep_copy walked into function
  # environments and the rewrite's clone did not, so `m` was lost on rm().
  with_globals(cap_g = local({ m <- 2; function(x) m * x }), code = {
    design <- declare_globally(
      declare_model(N = 5, Y = cap_g(1)) + declare_inquiry(q = mean(Y))
    )
    with_reloaded(design, "cap_g", function(back) {
      expect_equal(unique(draw_data(back)$Y), 2)
    })
  })
})

test_that("simulation runs under a multisession plan", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  with_globals(cap_b = 0.5, code = {
    design <- declare_globally(
      declare_model(N = 20, Y = rnorm(N) + cap_b) + declare_inquiry(m = mean(Y))
    )
    old <- future::plan(future::multisession, workers = 2)
    on.exit(future::plan(old), add = TRUE)
    expect_equal(nrow(simulate_design(design, sims = 4)), 4)
  })
})

# ---- the cases that should break it ------------------------------------------

test_that("a data column is not shadowed by a workspace name of the same name", {
  # The trap that forced DeclareDesign 1.x to special-case `N` twice: `N` is
  # both a workspace object and the column an earlier step declares, and the
  # column has to win inside the design.
  with_globals(N = 500, code = {
    design <- declare_globally(
      declare_model(N = 100, Y = rnorm(N)) + declare_inquiry(m = mean(Y))
    )
    expect_equal(nrow(draw_data(design)), 100)
    with_reloaded(design, "N", function(back) {
      expect_equal(nrow(draw_data(back)), 100)
    })
  })
})

test_that("a workspace object does not displace a package function", {
  with_globals(cap_x = 3, code = {
    design <- declare_globally(
      declare_model(N = 6, g = rep(1:2, 3)) +
        declare_inquiry(m = dplyr::n_distinct(g) + cap_x)
    )
    expect_equal(draw_estimands(design)$estimand, 5)
    with_reloaded(design, "cap_x", function(back) {
      expect_equal(draw_estimands(back)$estimand, 5)
    })
  })
})

test_that("a package function is not copied into the design", {
  # Copying rnorm() in would pin a version and bloat every design that calls it.
  with_globals(cap_b = 1, code = {
    design <- declare_globally(
      declare_model(N = 10, Y = rnorm(N) + cap_b) + declare_inquiry(q = mean(Y))
    )
    env <- quo_env_of(design, 1L, 2L)
    expect_true(exists("cap_b", envir = env, inherits = FALSE))
    expect_false(exists("rnorm", envir = env, inherits = FALSE))
  })
})

test_that("a NULL object is carried, not silently dropped", {
  # `env_get()` returning NULL is indistinguishable from "nothing here" unless
  # the code checks, so a legitimately NULL binding is the obvious way to lose
  # one.
  with_globals(cap_null = NULL, code = {
    design <- declare_globally(
      declare_model(N = 4, Y = length(cap_null)) + declare_inquiry(q = mean(Y))
    )
    with_reloaded(design, "cap_null", function(back) {
      expect_equal(unique(draw_data(back)$Y), 0)
    })
  })
})

test_that("mutually recursive helpers do not hang the capture", {
  with_globals(
    cap_even = NULL, cap_odd = NULL,
    code = {
      define_globally(cap_even <- function(n) if (n == 0) TRUE else cap_odd(n - 1))
      define_globally(cap_odd <- function(n) if (n == 0) FALSE else cap_even(n - 1))
      design <- declare_globally(
        declare_model(N = 3, Y = as.numeric(cap_even(4))) +
          declare_inquiry(q = mean(Y))
      )
      expect_equal(unique(draw_data(design)$Y), 1)
      with_reloaded(design, c("cap_even", "cap_odd"), function(back) {
        expect_equal(unique(draw_data(back)$Y), 1)
      })
    }
  )
})

test_that("a self-referential function does not hang the capture", {
  with_globals(cap_fact = NULL, code = {
    define_globally(cap_fact <- function(n) if (n <= 1) 1 else n * cap_fact(n - 1))
    design <- declare_globally(
      declare_model(N = 3, Y = cap_fact(5)) + declare_inquiry(q = mean(Y))
    )
    expect_equal(unique(draw_data(design)$Y), 120)
    with_reloaded(design, "cap_fact", function(back) {
      expect_equal(unique(draw_data(back)$Y), 120)
    })
  })
})

test_that("capture binds the object rather than copying it", {
  with_globals(cap_df = data.frame(a = rnorm(1000)), code = {
    design <- declare_globally(
      declare_model(data = cap_df) + declare_inquiry(q = mean(a))
    )
    env <- quo_env_of(design, 1L, 1L)
    expect_true(identical(
      get("cap_df", envir = env, inherits = FALSE),
      get("cap_df", envir = globalenv(), inherits = FALSE)
    ))
  })
})

test_that("a design's data is written to disk once, not twice", {
  # R's serialiser does not deduplicate an object shared between a child
  # environment and its parent, so capturing a name that already travels with
  # the design would double the file. Differencing two designs cancels the
  # fixed overhead of the closures and quosures, leaving the data itself: the
  # growth has to be about one copy, and must not be two.
  size_of <- function(x) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    saveRDS(x, f)
    file.size(f)
  }
  # Each design gets its own scope. Sharing one would make the comparison
  # meaningless, for the reason the next test pins.
  one_copy <- local({ d <- data.frame(a = rnorm(20000)); size_of(d) })
  big <- local({
    d <- data.frame(a = rnorm(20000))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(a)))
  })
  small <- local({
    d <- data.frame(a = rnorm(1))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(a)))
  })
  expect_gt(big - small, one_copy * 0.8)
  expect_lt(big - small, one_copy * 1.2)
})

test_that("a design carries only what it reads, not its whole declaration scope", {
  # Under the copying variant a design kept its declaration environment as the
  # parent of the captured one, so two designs declared in one scope each
  # carried everything in it. Pruning detaches the scope instead. Neither
  # local() may nest inside the other, or the inner one inherits the big object
  # and the comparison says nothing.
  size_of <- function(x) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    saveRDS(x, f)
    file.size(f)
  }
  alone <- local({
    d <- data.frame(a = rnorm(1))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(a)))
  })
  beside_a_big_unread_object <- local({
    unread <- data.frame(a = rnorm(20000))
    d <- data.frame(a = rnorm(1))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(a)))
  })
  expect_lt(beside_a_big_unread_object, alone * 1.2)
})

test_that("a globalenv data frame is also written once", {
  size_of <- function(x) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    saveRDS(x, f)
    file.size(f)
  }
  with_globals(cap_big = data.frame(a = rnorm(20000)),
               cap_small = data.frame(a = rnorm(1)), code = {
    one_copy <- size_of(get("cap_big", envir = globalenv()))
    big <- size_of(declare_globally(
      declare_model(data = cap_big) + declare_inquiry(q = mean(a))
    ))
    small <- size_of(declare_globally(
      declare_model(data = cap_small) + declare_inquiry(q = mean(a))
    ))
    expect_gt(big - small, one_copy * 0.8)
    expect_lt(big - small, one_copy * 1.2)
  })
})

test_that("a subset quosure carries its workspace objects", {
  with_globals(cap_cut = 0, code = {
    design <- declare_globally(
      declare_model(N = 40, X = rep(0:1, 20), Y = rnorm(N)) +
        declare_inquiry(m = mean(Y)) +
        declare_estimator(Y ~ 1, .method = lm, subset = X > cap_cut,
                          inquiry = "m", label = "sub")
    )
    with_reloaded(design, "cap_cut", function(back) {
      expect_s3_class(draw_estimates(back), "data.frame")
    })
  })
})

test_that("a sampling filter carries its workspace objects", {
  with_globals(cap_keep = 0, code = {
    design <- declare_globally(
      declare_model(N = 40, X = rep(0:1, 20)) +
        declare_sampling(filter = X > cap_keep) +
        declare_inquiry(m = mean(X))
    )
    with_reloaded(design, "cap_keep", function(back) {
      expect_equal(nrow(draw_data(back)), 20)
    })
  })
})

test_that("steps declared separately compose and survive together", {
  # `declaration_9.1 + declare_test(...)` is how several library designs are
  # built, and the two halves have different capture environments.
  with_globals(cap_n = 30, cap_sd = 2, code = {
    design <- declare_globally(
      declare_model(N = cap_n, Y = rnorm(N, sd = cap_sd)) +
        declare_inquiry(m = mean(Y))
    )
    with_reloaded(design, c("cap_n", "cap_sd"), function(back) {
      expect_equal(nrow(draw_data(back)), 30)
    })
  })
})

test_that("redesign still warns about a name the design does not use", {
  with_globals(cap_b = 1, code = {
    design <- declare_globally(
      declare_model(N = 10, Y = rnorm(N) + cap_b) + declare_inquiry(q = mean(Y))
    )
    expect_warning(redesign(design, not_a_param = 2), "not found in the design")
    expect_no_warning(redesign(design, cap_b = 2))
  })
})

test_that("a name bound only after declaration still resolves", {
  # Capture freezes what exists at declaration; a name defined later keeps
  # resolving through the environment. DeclareDesign 1.1.1 behaves the same
  # way, so binding order deciding the semantics is inherited, not introduced.
  design <- declare_globally(
    declare_model(N = 5, Y = cap_later) + declare_inquiry(q = mean(Y))
  )
  with_globals(cap_later = 7, code = {
    expect_equal(unique(draw_data(design)$Y), 7)
  })
})

test_that("capture does not reach across a package namespace", {
  # Known boundary, pinned deliberately. `user_binding_env()` stops at package
  # environments, so a declaration made inside a function enclosed by a
  # namespace never sees globalenv() and captures nothing. That is the topology
  # testthat itself creates, and the reason every test above declares in
  # globalenv() explicitly. It is harmless in practice: such a design's
  # environment serialises with it anyway.
  with_globals(cap_hidden = 5, code = {
    f <- function() declare_model(N = 3, Y = cap_hidden)
    environment(f) <- asNamespace("DeclareDesignZero")
    env <- rlang::quo_get_env(attr(f(), "dots")[[2]])
    expect_false(exists("cap_hidden", envir = env, inherits = FALSE))
  })
})

# ---- the dynamic-lookup fallback ---------------------------------------------

test_that("a scoped design that resolves names at run time still works", {
  # The regression pruning introduced on its own: detaching the declaration
  # scope removes names that only exist as strings in the code, and those fail
  # in the same session rather than merely on reload.
  res <- local({
    scoped_v <- 11
    scoped_f <- function() 6
    list(
      by_get = draw_data(
        declare_model(N = 3, Y = get("scoped_v")) + declare_inquiry(q = mean(Y))
      )$Y,
      by_do_call = draw_data(
        declare_model(N = 3, Y = do.call("scoped_f", list())) +
          declare_inquiry(q = mean(Y))
      )$Y
    )
  })
  expect_equal(unique(res$by_get), 11)
  expect_equal(unique(res$by_do_call), 6)
})

test_that("the fallback fires only for the declaration that needs it", {
  # A design that reads names at run time keeps its scope; one that does not is
  # pruned. Both shapes appear in the same test so the comparison is direct.
  envs <- local({
    scoped_v <- 11
    list(
      dynamic = rlang::quo_get_env(attr(
        declare_model(N = 3, Y = get("scoped_v")), "dots"
      )[[2]]),
      static = rlang::quo_get_env(attr(
        declare_model(N = 3, Y = scoped_v), "dots"
      )[[2]])
    )
  })
  expect_true(exists("scoped_v", envir = envs$dynamic, inherits = TRUE))
  expect_false(identical(parent.env(envs$static), globalenv()) &&
                 exists("scoped_f", envir = envs$static, inherits = TRUE))
  expect_true(identical(parent.env(envs$static), globalenv()))
})

test_that("the detector looks inside helper function bodies", {
  # A helper that calls get() is as dangerous as an inline get(), and nothing
  # in the declaration's own expression tree says so.
  val <- local({
    scoped_v <- 3
    helper <- function() get("scoped_v")
    draw_data(declare_model(N = 2, Y = helper()) + declare_inquiry(q = mean(Y)))$Y
  })
  expect_equal(unique(val), 3)
})

test_that("the fallback still carries globalenv objects across a reload", {
  # Falling back must not give up the fix: an ordinary workspace name in a
  # dynamic-lookup declaration still has to travel.
  with_globals(cap_v = 8, code = {
    design <- declare_globally(
      declare_model(N = 3, Y = cap_v + length(get("cap_v"))) +
        declare_inquiry(q = mean(Y))
    )
    with_reloaded(design, "cap_v", function(back) {
      # 8 from the plain reference, 1 from length(get("cap_v")). The `get()`
      # resolves too, because the same name is also written literally and so
      # was captured; that is luck rather than design, and the point of the
      # assertion is the plain reference surviving.
      expect_equal(unique(draw_data(back)$Y), 9)
    })
  })
})

test_that("a design with no dynamic lookup is still pruned", {
  size_of <- function(x) {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    saveRDS(x, f)
    file.size(f)
  }
  static <- local({
    unread <- data.frame(a = rnorm(20000))
    d <- data.frame(a = rnorm(1))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(a)))
  })
  dynamic <- local({
    unread <- data.frame(a = rnorm(20000))
    d <- data.frame(a = rnorm(1))
    size_of(declare_model(data = d) + declare_inquiry(q = mean(get("a"))))
  })
  expect_lt(static, 50 * 1024)
  expect_gt(dynamic, static * 2)
})
