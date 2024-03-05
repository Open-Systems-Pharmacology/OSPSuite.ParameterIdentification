
# ospsuite.parameteridentification

<!-- badges: start -->

[![R-CMD-check](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/actions/workflows/R-CMD-check-build.yaml/badge.svg)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/actions/workflows/R-CMD-check-build.yaml)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/branch/main/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification)
[![pkgdown](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/actions/workflows/pkgdown.yaml)

<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

The **ospsuite.parameteridentification** R package provides the
functionality of performing parameter identification (i.e., fitting the
model to observed data) with simulations created in the Open Systems
Pharmacology Software tools PK-Sim and MoBi. The package requires the
[**ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)
package to run the simulations.

## Installation

### Windows

The package can be installed from GitHub using the `{remotes}` package.
Under Windows, simply run the following code:

``` r
# {ospsuite.parameteridentification} and its Open Systems Pharmacology Suite's dependencies relies on
# {rClr} (https://github.com/Open-Systems-Pharmacology/rClr) which is not
# available on CRAN.
# Therefore, these must be installed from github using `{remotes}`.
# You can skip this step if you have already installed the `{ospsuite`} package.

install.packages("remotes")
install.packages("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip",
  type = "binary"
)

remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ParameterIdentification@*release")
```

Get the latest development version with:

``` r
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ParameterIdentification")
```

### Linux

For Linux, follow the instructions to install
[{`ospsuite`}](https://github.com/Open-Systems-Pharmacology/OSPSuite-R?tab=readme-ov-file#on-linux)
first and then run

``` r
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ParameterIdentification@*release")
```

(remove `@*release` to get the latest development version).

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
