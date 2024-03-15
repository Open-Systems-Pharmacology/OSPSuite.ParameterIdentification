# ospsuite.parameteridentification 1.3

* Added new optimization algorithms: the default local algorithm is now an implementation of the BOBYQA algorithm (bounded optimization by quadratic approximation) from the `{nloptr}` package; additional local algorithm is `HJKB`, a bounded implementation of the Hooke-Jeeves derivative-free algorithm from the `{dfoptim}` package; a global algorithm is `DEoptim` for differential evolution optimization.
* `FME::modCost()` is re-implemented as part of the parameter identification package and used for calculation of residuals.

## Breaking changes

* The parameter in the `PIConfiguration` class that is controlling the feedback at each function evaluation is now called `printEvaluationFeedback` instead of `printIterationFeedback`.

## Minor bug fixes and improvements

* Calculation of residuals does not fail if observed data contains only one time point.
* Calculation of the hessian close to the bounds of parameter values is improved.


# ospsuite.parameteridentification 1.2  

## Breaking changes

* requires `{ospsuite}` v11.1 or later.

## Minor bug fixes and improvements

* `getSteadyState()` accepts steady state time individually for each simulation.
