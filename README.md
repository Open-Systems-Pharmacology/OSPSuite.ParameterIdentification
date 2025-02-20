
# ospsuite.parameteridentification

<!-- badges: start -->

[![Main-Workflow](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/Main-Workflow.yaml?branch=main&label=Build)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/actions/workflows/Main-Workflow.yaml)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/graph/badge.svg?token=FL1AEQ7316)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification)

<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

The **ospsuite.parameteridentification** R package provides the
functionality of performing parameter identification (i.e., fitting the
model to observed data) with simulations created in the Open Systems
Pharmacology Software tools PK-Sim and MoBi. The package requires the
[**ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)
package to run the simulations.

## Installation

The package can be installed from GitHub using the `{pak}` package.
Simply run the following code:

``` r
install.packages("pak")

pak::pak("Open-Systems-Pharmacology/OSPSuite.ParameterIdentification@*release")
```

Get the latest development version with:

``` r
pak::pak("Open-Systems-Pharmacology/OSPSuite.ParameterIdentification")
```

## User guide

Examples of running parameter estimation tasks using this package are
detailed in `vignette('user-guide')`.

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc…) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

The `{OSPSuite.ParameterIdentification}` package is released under the
[GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
