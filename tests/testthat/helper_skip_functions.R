# helper to skip testing the data build functions that create causal networks
# from data-raw. We want to be able to test these during dev, but we don't want
# to build that data during build, and data-raw isn't available to built
# packages so it's not available to devtools::check() and similar

skip_if_no_file <- function(path) {
  if (!file.exists(path)) {
    skip(glue::glue(
      "File '{path}' not available- normal to not have 'data-raw/' if building the package"
    ))
  }
}

# Vdiffr often throws failures on external systems. Skip those tests

# *Highly* modified with inspiration from https://github.com/tidyverse/ggplot2/blob/main/tests/testthat/helper-vdiffr.R

if (testthat:::on_ci() || testthat:::on_cran()) {
  # assign a dummy function

  expect_doppelganger <- function(...) cat('\nSkipping exteral vdiffr\n')

} else {
  expect_doppelganger <- vdiffr::expect_doppelganger

}
