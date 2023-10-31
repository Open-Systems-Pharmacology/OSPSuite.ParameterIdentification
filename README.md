# OSPSuite.ParameterIdentification
R package for parameter identification for OSPS models

  <!-- badges: start -->

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/ospsuite-parameteridentification)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification)

  <!-- badges: end -->

# Overview
The **OSPSuite.ParameterIdentification** R-package provides the functionality of performing parameter identification (i.e., fitting the model to observed data) with simulations created in the Open Systems Pharmacology Software tools PK-Sim and MoBi. The package requires the [**ospsuite**](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) package to run the simulations.

# Installation

**OSPSuite.ParameterIdentification** requires following packages to be installed:

- [rClr](https://github.com/Open-Systems-Pharmacology/rClr/releases/latest) >0.9
- [ospsuite](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) >10
- [R6](https://github.com/r-lib/R6)

### For building from source and developing
- Rtools (https://cran.r-project.org/bin/windows/Rtools/)
  - After installation, add the folder to your $PATH: In start menu, type in "PATH", select "Change path environment for user", and add the path to Rtools folder.
- roxygen2 (CRAN)
- devtools (CRAN)
- rmarkdown (CRAN)
- testthat (CRAN)
- pander (CRAN)
- knitr (CRAN)
- styler (CRAN)

# User guide

Examples of running parameter estimation tasks using this package are detailed in [the user guide](articles/user-guide.html).

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite-R is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
