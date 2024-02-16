.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  rlang::local_use_cli()

  # if not interactive, disable the EWR progress bars
  if (!interactive()) {
    Sys.setenv("TQDM_DISABLE" = "1")
  }

  if (interactive()) {
    rlang::inform(c("EWR progress bars can be disabled.",
      "*" = "They are off by default in non-interactive sessions",
      "*" = "In an interactive session, run",
      "i" = "`Sys.setenv('TQDM_DISABLE' = '1')`",
      "*" = "before running `prep_run_save_ewrs()`
                  or otherwise loading the python environment"
    ))
  }
}
