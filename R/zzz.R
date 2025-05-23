.onLoad <- function(libname, pkgname) {
  reticulate::py_require('py-ewr')

  rlang::local_use_cli()

  # if not interactive, disable the EWR progress bars
  if (!interactive()) {
    Sys.setenv('TQDM_DISABLE' = '1')
  }

}

