# ospsuite-parameteridentification 1.3

- `FME::modCost()` is re-implemented as part of the parameter identification 
package and used for calculation of residuals.
- Observed data is converted to base units before adding DataSet objects to DataCombined and applying data transformations. This means that offsets in data transformations should be specified in base units and not in source units.

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
