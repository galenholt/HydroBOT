#' Read in data and aggregate along theme and spatial dimension
#'
#' Allows passing only paths to data rather than objects (though objects work as
#' well for consistency). Wrapper over [read_and_geo()], [make_edges()] and
#' [multi_aggregate()]. Particularly useful if we want to pass parameters as
#' strings from a config before anything is read in, and parallelisation (set a
#' [future::plan()]).
#'
#' @inheritParams multi_aggregate
#' @inheritParams read_and_geo
#' @inherit multi_aggregate params return
#'
#' @param datpath path to indicator data, or indicator data itself as a data
#'   frame. Currently needs to be EWR (same as `ewrpath` argument in
#'   [read_and_geo()]), but left more general here for future
#' @param causalpath path to the causal relationships .rds file or the causal
#'   network list object or its name
#' @param prepfun a function that does any post-read and pre-aggregation
#'   preparation of the module data. This is where mutates should go. If no data
#'   transformation is needed, use [identity()]. That should be the default for
#'   generality, but given the common use with the EWR tool, the defaults for
#'   EWR tool are included in HydroBOT (and [prep_ewr_output()] is the default
#'   here).
#' @param prepargs a list of arguments to `prepfun`. e.g. `list(type =
#'   'achievement', add_max = FALSE)`. Setting `prepargs = list(type =
#'   achievement')` and `type = 'yearly'` is a better (more
#'   general) way to declare that processing.
#' @param returnList default `TRUE`, whether to return the output to the current
#'   session
#' @param savepath default `NULL`, a path to save the output to. Note that this
#'   names the output rds file directly '`type`_aggregated.rds', so the path
#'   should include only the directory structure.  If `NULL`, does not save. If
#'   `savepath = NULL` and `returnList = FALSE`, the function errors to avoid
#'   wasting resources.
#' @param extrameta list, extra information to include in saved metadata
#'   documentation for the run. Default NULL.
#' @param rparallel logical, default FALSE. If TRUE, parallelises over the
#'   scenarios in hydro_dir using `furrr`. To use, install `furrr` and set a
#'   [future::plan()] (likely `multisession` or `multicore`)
#' @param par_recursive logical, default TRUE. If parallel, do we use the
#'   innermost level of directory containing EWR outputs (TRUE) or the next
#'   level in from datpath (FALSE)
#' @param savepar 'combine' (default) or 'each'. If parallel over scenarios,
#'   should this combine the output (default) or save each scenario's
#'   aggregation separately ('each')
#' @param ... passed to [read_and_geo()], primarily `gaugefilter`,
#'   `scenariofilter`.
#'
#' @export
#'
read_and_agg <- function(datpath,
                         type,
                         geopath,
                         causalpath,
                         groupers = "scenario",
                         group_until = rep(NA, length(groupers)),
                         prepfun = 'prep_ewr_output',
                         prepargs = list(),
                         aggCols,
                         aggsequence,
                         funsequence,
                         saveintermediate = FALSE,
                         namehistory = TRUE,
                         keepAllPolys = FALSE,
                         failmissing = TRUE,
                         auto_ewr_PU = FALSE,
                         pseudo_spatial = NULL,
                         returnList = TRUE,
                         savepath = NULL,
                         extrameta = NULL,
                         rparallel = FALSE,
                         par_recursive = TRUE,
                         savepar = "combine",
                         ...) {
  if (!returnList && is.null(savepath)) {
    rlang::abort(
      message = "not returning output to disk or session.
      aborting to not use the resources."
      )
  }

  # allow passing the causal network by name or path
  if (is.character(causalpath) && !grepl("\\.", causalpath)) {
    causalpath <- get(causalpath)
  }
  if (is.character(causalpath) && grepl("*.rds", causalpath)) {
    causalpath <- readRDS(causalpath)
  }

  # set up metadata placeholders to know if the run failed
  if (!is.null(savepath)) {
    if (!dir.exists(savepath)) {
      dir.create(savepath, recursive = TRUE)
    }
    # set up flags for the metadata in case the ewr fails partway
    init_params <- list(
      meta_message = "Started run, has not finished. New metadata file will write when it does. If this metadata entry persists, the run failed.",
      agg_status = FALSE,
      time = format(Sys.time(), digits = 0, usetz = TRUE)
    )
    yaml::write_yaml(init_params,
      file = file.path(savepath, "agg_metadata.yml")
    )
    if (rlang::is_installed("jsonlite")) {
      jsonlite::write_json(init_params,
        path = file.path(savepath, "agg_metadata.json")
      )
    } else {
      rlang::inform("json metadata not saved. If desired, install `jsonlite`",
        .frequency = "regularly", .frequency_id = "jsoncheck"
      )
    }
  }

  # There's got to be a cleaner way to do this
  # Recurse over inner directories if parallel
  if (rparallel) {
    if (par_recursive) {
      # Go all the way in (i.e. loop over every folder with a csv)
      gfiles <- list.files(datpath, pattern = ".csv",
                           full.names = TRUE, recursive = TRUE)
      dps <- gsub("/[^/]+$", "", gfiles) |> unique()
    } else {
      # only first level
      dps <- list.dirs(datpath, recursive = FALSE)
    }

    names(dps) <- gsub(paste0(datpath, "/"), "", dps)

    # make cmd-check happy
    y <- NULL

    if (savepar == "each") {
      spath <- rlang::expr(file.path(savepath, y))
    } else if (savepar == "combine") {
      spath <- NULL
    } else {
      rlang::abort("Unacceptable value for savepar")
    }

    aggout <- safe_imap(dps, \(x, y) read_and_agg(x,
      type = type,
      geopath = geopath,
      causalpath = causalpath,
      groupers = groupers,
      group_until = group_until,
      prepfun = prepfun,
      prepargs = prepargs,
      aggCols = aggCols,
      aggsequence = aggsequence,
      funsequence = funsequence,
      saveintermediate = saveintermediate,
      namehistory = namehistory,
      keepAllPolys = keepAllPolys,
      failmissing = failmissing,
      auto_ewr_PU = auto_ewr_PU,
      pseudo_spatial = pseudo_spatial,
      returnList = returnList,
      savepath = eval(spath),
      extrameta = extrameta,
      rparallel = FALSE,
      par_iter = which(y == names(dps)), # so I can not write safe_map and helps manage add_max
      ...
    ),
    parallel = TRUE
    )

    aggout <- purrr::list_transpose(aggout) |>
      purrr::map(dplyr::bind_rows)
    # return(aggout)
  }

  if (!rparallel) {
    # Let some old defaults continue to work
    if (type == "achievement") {
      pull_type <- "yearly"
      prepargs <- utils::modifyList(prepargs, list(type = type))
      if (!is.null(list(...)$par_iter) && list(...)$par_iter > 1) {
        prepargs <- utils::modifyList(prepargs, list(add_max = FALSE))
      }
      # this would be great, but too many testing issues
      # lifecycle::deprecate_soft("0.2.3", "read_and_agg(type = 'should not use the special values achievement and interevents. Put them in `prepargs`')")
    } else if (type == 'interevents') {
      pull_type <- 'all_successful_interEvents'
      prepargs <- utils::modifyList(prepargs, list(type = type))
      # lifecycle::deprecate_soft("0.2.3", "read_and_agg(type = 'should not use the special values achievement and interevents. Put them in `prepargs`')")
    } else {
      pull_type <- type
    }

    data <- read_and_geo(datpath, type = pull_type,
                         geopath = geopath, whichcrs = 4283, ...)

    # Run any data cleaning on input (including mutates)
    # rlang::exec could handle bare functions or names, but I need to know the first arg
    prepfun <- functionlister(prepfun)
    if (length(prepfun) > 1) {
      rlang::abort('Only one prep function possible.
                   If need iterative prep, write a wrapper and pass that.')
    }
    firstarg <- names(formals(prepfun[[1]])[1])
    prepargs <- utils::modifyList(prepargs, list(f1 = data))
    names(prepargs)[names(prepargs) == 'f1'] <- firstarg
    data <- rlang::exec(prepfun[[1]],
                        !!!prepargs)


    # parse any character names for the spatial data, then character will be the
    # themes
    aggsequence <- purrr::map(aggsequence, parse_geo)
    stepdim <- identify_dimension(
      aggsequence,
      causalpath
    )

    # multi_aggregate can handle the raw network just fine, but this saves
    # re-calculating edges dfs each time.
    edges <- make_edges(
      dflist = causalpath,
      fromtos = aggsequence[stepdim == "theme"]
    )

    # be aggressive about parsing tidyselect into characters or expressions get
    # lost in the stack
    groupers <- selectcreator(rlang::enquo(groupers), data, failmissing)
    aggCols <- selectcreator(rlang::enquo(aggCols), data, failmissing)

    # we need to handle different ways of getting `group_until`. The easiest way
    # to do this is to do it here to make it standard, although it will also
    # happen in multi-aggregate.

    group_until <- parse_group_until(
      group_until = group_until,
      groupers = groupers,
      aggsequence = aggsequence
    )

    # Annoying how much of this is just pass-through arguments. Could use dots,
    # but then would need to specify the read_and_geo dots.
    aggout <- multi_aggregate(data,
      edges,
      groupers = groupers,
      group_until = group_until,
      aggCols = aggCols,
      aggsequence = aggsequence,
      funsequence = funsequence,
      saveintermediate = saveintermediate,
      namehistory = namehistory,
      keepAllPolys = keepAllPolys,
      failmissing = failmissing,
      auto_ewr_PU = auto_ewr_PU,
      pseudo_spatial = pseudo_spatial
    )
  }

  # use RDS so can read it in to a different name.
  if (!is.null(savepath)) {
    if (!dir.exists(savepath)) {
      dir.create(savepath, recursive = TRUE)
    }
    # save the output if sequential or if we want to save the combined parallel
    if (!rparallel || savepar == "combine") {
      saveRDS(aggout, file.path(savepath, paste0(type, "_aggregated.rds")))
    }


    char_aggsequence <- purrr::imap(aggsequence, parse_char)
    char_funsequence <- purrr::map(funsequence, parse_char_funs)
    # using names makes the yaml cleaner
    names(char_funsequence) <- names(char_aggsequence)

    agg_params <- list(
      datpath = datpath,
      type = type,
      groupers = groupers,
      group_until = group_until,
      pseudo_spatial = pseudo_spatial,
      aggCols = aggCols,
      aggsequence = char_aggsequence,
      funsequence = char_funsequence,
      namehistory = namehistory,
      keepAllPolys = keepAllPolys,
      auto_ewr_PU = auto_ewr_PU,
      returnList = returnList,
      finish_time = format(Sys.time(), digits = 0, usetz = TRUE),
      status = TRUE,
      HydroBOT_version = as.character(utils::packageVersion("HydroBOT"))
    )

    # add any passed metadata info
    if (is.list(extrameta)) {
      agg_params <- utils::modifyList(agg_params, extrameta)
    }

    # append any module metadata, so it all stays together
    ymlmodpath <- list.files(datpath, pattern = "*.yml")
    if (length(ymlmodpath) != 0) {
      ymlmods <- file.path(datpath, ymlmodpath) |>
        yaml::read_yaml()
    } else {
      ymlmods <- list()
    }

    yaml::write_yaml(utils::modifyList(ymlmods, list(aggregation = agg_params)),
      file = file.path(savepath, "agg_metadata.yml")
    )

    if (rlang::is_installed("jsonlite")) {
      jsonmodpath <- list.files(datpath, pattern = "*.json")
      if (length(jsonmodpath) != 0) {
        jsonmods <- file.path(datpath, jsonmodpath) |>
          jsonlite::read_json()
      } else {
        jsonmods <- NULL
      }
      jsonlite::write_json(list(jsonmods$ewr, aggregation = agg_params),
        path = file.path(savepath, "agg_metadata.json")
      )
    } else {
      rlang::inform("json metadata not saved. If desired, install `jsonlite`",
        .frequency = "regularly", .frequency_id = "jsoncheck"
      )
    }
  }

  if (returnList) {
    return(aggout)
  } else {
    return(NULL)
  }
}
