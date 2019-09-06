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
