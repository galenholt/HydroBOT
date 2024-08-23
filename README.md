
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HydroBOT

<!-- badges: start -->

[![R-CMD-check](https://github.com/MDBAuth/HydroBOT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MDBAuth/HydroBOT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R package forms the core of the WERP climate adaptation toolkit,
ingesting hydrological scenarios representing historical or future
climates or adaptation options, and processing those through various
response models (currently [MDBA EWR
tool](https://github.com/MDBAuth/EWR_tool), with intention to include
other tools in future). Subsequent processing of outcomes along spatial,
theme, and temporal axes are available or under development, as well as
control over outputs and comparisons between scenarios. Causal networks
defining relationships in the response models are included.

The [template repo](https://github.com/MDBAuth/toolkit_use) can be
helpful for establishing project structure to use the toolkit and
automating the setup process, particularly if you are on Linux or want
to manage your python environments.

## Installation

Install the development version of HydroBOT from
[GitHub](https://github.com/MDBAuth/HydroBOT) with

``` r
# install.packages("devtools")
devtools::install_git("git@github.com:MDBAuth/HydroBOT.git", ref = 'master', force = TRUE, upgrade = 'ask', git = 'external')
```

R uses a different Home directory than standard (typically
`~/Documents`), and so if this fails, try to [set up SSH
keys](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
in that location as well.

There are slightly slicker ways to do it, but they don’t work well
currently. See [developer docs](developer.md) for details or if the
above doesn’t work.

### Python dependency

To run, this needs a Python environment containing `py_ewr` (currently
2.0.0). The package will manage that for you if you just start using it-
on first use, the package checks the environment and either uses an
existing python environment or builds one with that dependency when the
package is loaded.

There are `poetry.lock` and `pyproject.toml` files in the repo that
allow for dev work and building the venv manually if more control over
python is desired.

## Use

Can be run piecemeal or all at once, scripted. Point at a directory of
hydrologic scenarios, and the toolkit will run them through the modules,
aggregate the outputs, and present results, with control by the user
through function arguments. See the
[WERP_toolkit_demo](https://mdbauth.github.io/WERP_toolkit_demo/) for a
full demonstration and the [template
repo](https://github.com/MDBAuth/toolkit_use) for a bare-bones project
structure.

## Developement

[see developer page](developer.md)

## Example

See the
[WERP_toolkit_demo](https://mdbauth.github.io/WERP_toolkit_demo/) for a
full demonstration.

## Who do I talk to?

- Galen Holt, <g.holt@deakin.edu.au>
