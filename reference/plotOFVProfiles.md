# Plot Objective Function Value (OFV) Profiles

Visualizes the OFV profiles produced by
`ParameterIdentification$calculateOFVProfiles()`. Each profile is
rendered as a scatter plot of the OFV against the corresponding
parameter value, with point color encoding OFV (lower = brighter,
reversed Viridis).

## Usage

``` r
plotOFVProfiles(profiles)
```

## Arguments

- profiles:

  A named list of tibbles as returned by
  `ParameterIdentification$calculateOFVProfiles()`. Each tibble must
  contain exactly two columns: the parameter values (named after the
  parameter's path) and the matching objective function values (named
  `ofv`). The list names must match the parameter-value column names.

## Value

A list of `ggplot` objects, one per parameter profile, in the same order
as `profiles`.

## Details

Around a (local) minimum the objective function is expected to be convex
and roughly parabolic. Inspecting the OFV profile around a tentative
optimum is therefore a quick visual check that the optimization has
converged to a sensible point in parameter space.

Because parameter paths in OSP simulations can be long (e.g.
`Neighborhoods|Kidney_pls_Kidney_ur|Aciclovir|Renal Clearances-TS-Aciclovir|TSspec`),
the x-axis label uses only the last `|`-separated segment, while the
full path is shown as the plot title.

## See also

[ParameterIdentification](https://www.open-systems-pharmacology.org/OSPSuite.ParameterIdentification/reference/ParameterIdentification.md)
for `calculateOFVProfiles()`.

## Examples

``` r
if (FALSE) { # \dontrun{
# piTask is a configured ParameterIdentification instance
ofvProfiles <- piTask$calculateOFVProfiles()
plots <- plotOFVProfiles(ofvProfiles)
plots[[1]]
} # }
```
