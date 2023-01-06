
# reference to py functions,
controller_functions <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)


  # controller_functions <<- reticulate::import_from_path("controller_functions", path = system.file("python", package = 'werptoolkitr'), delay_load = TRUE)

  # also works, closer to the reticulate::import(module) we eventually want
  # controller_functions <<- reticulate::import_from_path("controller_functions", "../werptoolkitpy/werptoolkitpy/controller_functions", delay_load = TRUE)

}

