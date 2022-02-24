# DeclareDesign 0.30.0

* New tidy(diagnosis) function to construct a tidy data frame of diagnosand estimates and summary statistics including bootstrapped standard error and confidence intervals.
* Remove draw_assignment and draw_sample function.
* Added diagnosis duration to summary function.
* Bug fixes.

# DeclareDesign 0.28.0

* To simplify output of diagnoses, we changed the names of the variables from design_label to design, inquiry_label to inquiry, and estimator_label to estimator.
* declare_assignment() and declare_sampling() have the default values for legacy set to FALSE. You can still use the legacy versions of these functions by manually setting legacy = TRUE for some time, but this functionality will later be removed. 

# DeclareDesign 0.26.0

* Rapid development phase is beginning to prepare for DeclareDesign 1.0. 
* Add new step `declare_model` for defining the model of the world including sample size, levels of the data, and variables. 
* Add new step `declare_inquiry` to replace `declare_estimand`. `declare_estimand` is still available but deprecated.
* Soft-introduce new syntax for declare_assignment and declare_sampling. Old syntax is still available with legacy = TRUE, the current default. To use the new syntax, set legacy = FALSE. In future versions of DeclareDesign, the default will be set to FALSE.
* `reveal_outcomes`, created in 0.24.0, has been removed as a step.
* Change labels produced in diagnoses and `run_design` output to be `inquiry_label` rather than `estimand_label`. 
* Allow estimands to be functions of other estimands.

# DeclareDesign 0.24.0

* Add new step `declare_measurement` for measuring outcome variables. 
* Add `declare_test` to enable hypothesis testing where no estimand is targeted. For example, `declare_test` could be used for a K-S test of distributional equality and `declare_estimator` for a difference-in-means estimate of an average treatment effect.
* Add `model_summary` option to `declare_estimator`, to enable specifying a model and then a separate post-estimation function to extract coefficient estimates (e.g., estimate of a treatment effect) or model summary statistics (e.g., R^2 or the result of an F-test from a regression).
* Simplify `declare_diagnosands` functionality. `diagnose_design()` by default runs an internal function with a set of default diagnosands, including power, RMSE, bias, type S rate, coverage, mean estimate, and mean estimand.
* Improve compatibility with dplyr verbs as handlers. `filter` now works. 
* Rename `declare_reveal` to `reveal_outcomes`. Both continue to work.

# DeclareDesign 0.22.0

* Fix ability to set `sampling_variable` in `declare_sampling`. 
* Add ability to retain nonsampled data after sampling via `drop_nonsampled` flag in `declare_sampling`.

# DeclareDesign 0.20.0

* Add `compare_diagnoses` function to compare two designs on the basis of their design diagnoses. 
* Compatibility with `rlang` 0.4.0
* Bug fixes

# DeclareDesign 0.18.0

* Add `compare_designs` functions to compare the code and output of designs side-by-side. 
* Bug fixes

# DeclareDesign 0.16.0

* Add `draw_assignment` function to draw an assignment vector(s) given data
* Add `draw_sample` function to draw a sample or multiple sequential samples from data
* Rewrite `draw_data` to optionally take a data argument. `draw_data` now can be used to draw data for the full design, or for subsets of it. `start` and `end` flags are added to select which portions of the design to run
* Bug fixes

# DeclareDesign 0.14.0

* Improved generics interoperability
* Bug fixes

# DeclareDesign 0.12.0

* Add ability to use `get_estimates` with data, useful for example for getting estimates after data is collected for a study. To draw estimates or estimands from simulated data, now use renamed `draw_estimates` and `draw_estimands` functions.
* Documentation improvements
* Bug fixes

# DeclareDesign 0.10.0

* First CRAN version
