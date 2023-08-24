
<!-- README.md is generated from README.Rmd. Please edit that file -->

# werptoolkitr

<!-- badges: start -->

[![R-CMD-check](https://github.com/MDBAuth/WERP_toolkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MDBAuth/WERP_toolkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R package forms the core of the WERP climate adaptation toolkit,
ingesting hydrological scenarios representing historical or future
climates or adaptation options, and processing those through various
response models (currently [MDBA EWR
tool](https://github.com/MDBAuth/EWR_tool), with intention to include
other tools in future). Subsequent processing of outcomes along spatial,
theme, and temporal axes are available or underdevelopment, as well as
control over outputs and comparisons between scenarios. Causal networks
defining relationships in the response models are included.

## Installation

You should be able to install the development version of werptoolkitr
from [GitHub](https://github.com/MDBAuth/WERP_toolkit) with
`devtools::install_git`. For `devtools::install_git()` to work with SSH,
first install the {git2r} package (though even this doesn’t work in R
4.3).

``` r
# install.packages("devtools", "git2r")
devtools::install_git("git@github.com:MDBAuth/WERP_toolkit.git", ref = 'master', force = TRUE, upgrade = 'ask')
```

But, if using R 4.3, the {git2r} package does not support ssh, and so
you have to clone the directory, and use

``` r
`devtools::install_local('path/to/repo', force = TRUE, upgrade = 'ask')`
```

## Use

Can be run piecemeal or all at once, scripted. Point at a directory of
hydrologic scenarios, and the toolkit will run them through the modules,
aggregate the outputs, and present results, with control by the user
through function arguments. See the
[WERP_toolkit_demo](https://special-disco-eyonrvy.pages.github.io/) for
a full demonstration

## Developement

[see developer page](developer.md)

### Python dependency

To run, this needs a Python environment containing `py_ewr` (currently
1.0.6). On first use, the package checks the environment and either uses
an existing python environment or builds one with that dependency when
the package is loaded. There are `poetry.lock` and `pyproject.toml`
files in the repo that allow for dev work and building one manually if
desired.

## Example

See the
[WERP_toolkit_demo](https://special-disco-eyonrvy.pages.github.io/) for
a full demonstration.

## Who do I talk to?

- Galen Holt, <g.holt@deakin.edu.au>
