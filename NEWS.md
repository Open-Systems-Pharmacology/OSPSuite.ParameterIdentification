# ospsuite-parameteridentification 1.3

- Added new optimization algorithms: the default local algorithm is now an implementation of the BOBYQA algorithm (bounded optimization by quadratic approximation) from the `{nloptr}` package; additional local algorithm is `HJKB`, a bounded implementation of the Hooke-Jeeves derivative-free algorithm from the `{dfoptim}` package; a global algorithm is `DEoptim` for differential evolution optimization.
- `FME::modCost()` is re-implemented as part of the parameter identification 
package and used for calculation of residuals.

### BREAKING CHANGES
- The parameter in the `PIConfiguration` class that is controlling the feedback at each function evaluation is now called `printEvaluationFeedback` instead of `printIterationFeedback`

### BUG FIXES
- Calculation of residals does not fail if observed data contains only one 
time point.


# ospsuite-parameteridentification 1.2

### BREAKING CHANGES

- requires `{ospsuite}` v11.1 or later

### MAJOR CHANGES

### MINOR CHANGES

- `getSteadyState()` accepts steady state time invidiaul for each simulation.

------
