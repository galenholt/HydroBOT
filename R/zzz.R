.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  rlang::local_use_cli()

  # if not interactive, disable the EWR progress bars
  if (!interactive()) {
    Sys.setenv('TQDM_DISABLE' = '1')
  }

}

