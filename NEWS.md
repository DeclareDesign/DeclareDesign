# DeclareDesign 2.0.0

DeclareDesign 2.0 is a ground-up reimplementation on tidyverse primitives. The declaration syntax is unchanged; the changes below are at the edges. `vignette("declaredesign2.0")` gives the reasoning for each and a grep table for porting a script.

## Breaking changes

* `redesign()` reaches a name the design reads (a value named above the design, a designer function's argument, or a `declare_parameters()` entry) and no longer a number written inside a step. `redesign(design, N = 1000)` over `declare_model(N = 500)` errors with the two ways to name the value; in 1.x it warned and returned the design unchanged.
* `declare_estimator(inquiry = )` takes the inquiry's label as a string. Passing the inquiry step object is an error; in 1.x it linked the estimator to an inquiry called `"inquiry"` and returned `NA` estimands.
* `diagnose_design()` and `simulate_design()` drop `make_groups`, `add_grouping_variables` and `future.seed`, each with an error naming the replacement. Group with `simulate_design() |> group_by() |> diagnose_design()`; seed with `set.seed()` before the call.
* `redesign()` and `expand_design()` take `.design`, `.designer` and `.expand`, so a parameter named `d` or `expand` is no longer swallowed by partial matching. `expand_design()` drops `prefix`.
* `draw_data()` takes the design alone; `data`, `start` and `end` are gone. Use `get_estimates(design, data)` to run estimators on supplied data and `design[i:j]` for a slice.
* `label_estimator(fn = )` and `label_test(fn = )` are `.method = `; `set_diagnosands(x = )` is `design = `; `reshape_diagnosis()` drops `select` and `exclude`.
* `compare_designs()`, `compare_design_code()`, `compare_design_summaries()`, `compare_design_estimates()`, `compare_design_estimands()`, `compare_design_data()` and `print_code()` are defunct and say so. `compare_diagnoses()` stays.
* `declare_potential_outcomes()` and `declare_reveal()` are defunct; `declare_estimators()` is gone.
* Every table of estimates carries an `estimand` column and puts `estimator`, `inquiry` and `estimand` after the tidied statistics. Two estimators with one label are renamed apart with a message rather than an error.

## Deprecations

* `insert_step()`, `delete_step()` and `replace_step()` warn once per session; rebuild the design with `design[1:2] + new_step + design[4:5]` instead.
* `declare_population()`, `model_handler()` and `tidy_estimator()` warn once per session.
* `declare_estimator(model = )` is read as `.method = ` with a warning.

## New

* `declare_parameters()` names the values a design can be redesigned over, and `declare_notes()` names the quantities it works out along the way; `design_parameters()` and `design_notes()` list them. A declared parameter is bound for every step after it, including a helper function that reads it.
* A design is a value: it carries the objects its expressions read, so it survives `saveRDS()`, reaches a parallel worker, and does not change meaning when a workspace variable is edited (closes #293).
* Step-level `draws` fan a step out and hold everything upstream fixed; the diagnosis of such a design carries a `variance_decomposition` attributing the variance of each per-simulation quantity to the step that generated it.
* An estimator that errors on a draw under `simulate_design()` is recorded as a row with `error = TRUE`, one warning per run gives the counts, and diagnosands use the draws that succeeded (closes #385). `run_design()` and `draw_estimates()` still stop.
* Every simulation draw runs on its own L'Ecuyer-CMRG stream under any `future::plan()`, so a seeded run gives the same table sequentially and in parallel.
* `progress = TRUE` on `simulate_design()` and `diagnose_design()`, or `progressr::handlers(global = TRUE)` once per session, reports progress through progressr, from parallel workers too.
* `diagnose_design()` accepts a simulations data frame and honours its grouping; `diagnose_simulations()` is the same entry point by name.
* `default_diagnosands()` and `select_diagnosands()` build a diagnosands set from names; `tidy()` works on a diagnosis.
* Estimator and step arguments pass to `.method` or the handler as written, so methods that do their own non-standard evaluation work (closes #463; also #456, #457, #479, #482, #509).

## Messages that were silences

* An estimator naming an inquiry no step produced warns once, naming the labels that exist.
* A sampling step that produces no `S` column and has no `filter` warns once that every row was kept.
* An error inside a step names the step and its verb, with the original error as the cause.
* A redesign of a name the design does not use warns; a bare vector handed to a vector-valued parameter warns that it is being read as several designs.
