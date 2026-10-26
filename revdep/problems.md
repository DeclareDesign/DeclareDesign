# DesignLibrary (0.1.10)

* GitHub: <https://github.com/DeclareDesign/DesignLibrary>
* Email: <mailto:jjc2247@columbia.edu>
* GitHub mirror: <https://github.com/cran/DesignLibrary>

Run `revdepcheck::revdep_details(, "DesignLibrary")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘DesignLibrary-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: binary_iv_designer
     > ### Title: Create a binary instrumental variables design
     > ### Aliases: binary_iv_designer simple_iv_designer
     > 
     > ### ** Examples
     > 
     > # Generate a simple iv design: iv identifies late not ate 
     > binary_iv_design_1 <- binary_iv_designer(N = 1000, b = c(.1, .2, .3, .4))
     Warning: `declare_population()` is deprecated. Use `declare_model()` instead.
     This warning is displayed once per session.
     Error in `declare_potential_outcomes()`:
     ! `declare_potential_outcomes()` is not a step in DeclareDesign 2.0.
     ℹ Put potential outcomes in the model, as a formula:
     • declare_model(N = 100, potential_outcomes(Y ~ 0.2 * Z + U))
     ℹ Named potential outcomes are ordinary model variables:
     • declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.2)
     Backtrace:
         ▆
      1. └─DesignLibrary::binary_iv_designer(...)
      2.   └─DeclareDesign::declare_potential_outcomes(Y ~ a[type] + b[type] * X + d[type] * Z + u_Y, assignment_variables = "X")
      3.     └─rlang::abort(...)
     Execution halted
     ```

## In both

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       Error in `declare_potential_outcomes(Y ~ (1 - Z) * (u_0 * control_sd + control_mean) + Z * (u_1 * treatment_sd + treatment_mean))`: `declare_potential_outcomes()` is not a step in DeclareDesign 2.0.
       ℹ Put potential outcomes in the model, as a formula:
       • declare_model(N = 100, potential_outcomes(Y ~ 0.2 * Z + U))
       ℹ Named potential outcomes are ordinary model variables:
       • declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.2)
       Backtrace:
           ▆
        1. └─DesignLibrary::two_arm_designer() at test_simulate_designers.R:4:3
        2.   └─DeclareDesign::declare_potential_outcomes(...)
        3.     └─rlang::abort(...)
       
       [ FAIL 4 | WARN 1 | SKIP 0 | PASS 19 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking whether package ‘DesignLibrary’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: package ‘randomizr’ was built under R version 4.6.1
       Warning: package ‘estimatr’ was built under R version 4.6.1
     See ‘/Users/alexandercoppock/git_projects/DeclareDesign/revdep/checks.noindex/DesignLibrary/new/DesignLibrary.Rcheck/00install.out’ for details.
     ```

# rdss (1.0.14)

* Email: <mailto:graeme.blair@gmail.com>
* GitHub mirror: <https://github.com/cran/rdss>

Run `revdepcheck::revdep_details(, "rdss")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         1. ├─testthat::expect_error(...) at test-regression-discontinuity.R:36:3
         2. │ └─testthat:::expect_condition_matching_(...)
         3. │   └─testthat:::quasi_capture(...)
         4. │     ├─testthat (local) .capture(...)
         5. │     │ └─base::withCallingHandlers(...)
         6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         7. └─DeclareDesign::simulate_design(declaration_16.5, sims = 1, future.seed = FALSE)
         8.   └─DeclareDesign:::flatten_designs(raw)
         9.     └─DeclareDesign:::stop_not_a_design(item, nm)
        10.       └─rlang::abort(...)
       
       [ FAIL 8 | WARN 11 | SKIP 1 | PASS 0 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘helpers-process-tracing.R’ ‘process_tracing_estimator.Rd’
     ```

