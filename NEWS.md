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
