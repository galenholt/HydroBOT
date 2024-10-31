#' Read in data and aggregate along theme and spatial dimensiont
#'
#' Allows passing only paths to data rather than objects. Wrapper over
#' [prep_ewr_agg()] (though can be made more general once we have other
#' modules), [make_edges()] and [multi_aggregate()]. Particularly useful if we
#' want to pass parameters as strings from a config before anything is read in,
#' and parallelisation.
#'
#' @inheritParams multi_aggregate
#' @inherit multi_aggregate params return
#'
#' @param datpath path to indicator data. Currently needs to be EWR (same as
#'   `ewrpath` argument in [prep_ewr_agg()]), but left more general here for
#'   future
#' @param type character of which type of EWR output (currently `'summary'`,
#'   `'annual'`, or `'both'`). New values, e.g. `'all`' probably work but
#'   untested.
#' @param geopath path to the file with gauge locations in lat/long (assumes BOM
#'   currently), or an `sf` with gauge locations
#' @param causalpath path to the causal relationships .rds file. Should
#'   typically be created in `Causal networks`
#' @param returnList default `TRUE`, whether to return the output to the current
#'   session
#' @param savepath default `NULL`, a path to save the output to. Note that this
#'   names the output rds file directly '`type`_aggregated.rds', so the path
#'   should include only the directory structure.  If `NULL`, does not save. If
#'   `savepath = NULL` and `returnList = FALSE`, the function errors to avoid
#'   wasting resources.
#' @param extrameta list, extra information to include in saved metadata documentation for the run. Default NULL.
#' @param rparallel logical, default FALSE. If TRUE, parallelises over the
#'   scenarios in hydro_dir using `furrr`. To use, install `furrr` and set a
#'   [future::plan()] (likely `multisession` or `multicore`)
#' @param par_recursive logical, default TRUE. If parallel, do we use the innermost level of directory containing EWR outputs (TRUE) or the next level in from datpath (FALSE)
#' @param ... passed to [prep_ewr_agg()]
#'
#' @export
#'
read_and_agg <- function(datpath,
                         type,
                         geopath,
                         causalpath,
                         groupers = "scenario",
                         group_until = rep(NA, length(groupers)),
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
                         ...) {

  if (!returnList & is.null(savepath)) {
    rlang::abort(message = "not returning output to disk or session. aborting to not use the resources.")
  }

  # Recurse over inner directories if parallel
  if (rparallel) {
    if (par_recursive) {
      # Go all the way in (i.e. loop over every folder with a csv)
      gfiles <- list.files(datpath, pattern = '.csv', full.names = TRUE, recursive = TRUE)
      dps <- gsub("/[^/]+$",'', gfiles) |> unique()
    } else {
      # only first level
      dps <- list.dirs(datpath, recursive = FALSE)
    }

    names(dps) <- gsub(paste0(datpath, '/'),'', dps)
    aggout <- safe_imap(dps, \(x, y) read_and_agg(x,
                                               type = type,
                                               geopath = geopath,
                                               causalpath = causalpath,
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
                                               pseudo_spatial = pseudo_spatial,
                                               returnList = returnList,
                                               savepath = file.path(savepath, y),
                                               extrameta = extrameta,
                                               rparallel = FALSE,
                                               y, # so I can not write safe_map
                                               ...),
                        parallel = TRUE)
    aggout <- purrr::list_transpose(aggout) |>
      purrr::map(dplyr::bind_rows)
    return(aggout)
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

  data <- prep_ewr_agg(datpath, type = type, geopath = geopath, whichcrs = 4283, ...)

  # parse any character names for the spatial data, then character will be the themes
  aggsequence <- purrr::map(aggsequence, parse_geo)
  stepdim <- identify_dimension(aggsequence,
                                causalpath)

  # multi_aggregate can handle the raw network just fine, but this saves re-calculating edges dfs each time.
  edges <- make_edges(
    dflist = causalpath,
    fromtos = aggsequence[stepdim == 'theme']
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
  # but then would need to specify the prep_ewr_agg dots.
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

  # use RDS so can read it in to a different name.
  if (!is.null(savepath)) {
    if (!dir.exists(savepath)) {
      dir.create(savepath, recursive = TRUE)
    }
    saveRDS(aggout, file.path(savepath, paste0(type, "_aggregated.rds")))

    char_aggsequence <- purrr::imap(aggsequence, parse_char)
    char_funsequence <- purrr::map(funsequence, parse_char_funs)
    # using names makes the yaml cleaner
    names(char_funsequence) <- names(char_aggsequence)

    agg_params <- list(
      agg_input_path = datpath,
      aggType = type,
      agg_groups = groupers,
      agg_group_until = group_until,
      agg_pseudo_spatial = pseudo_spatial,
      agg_var = aggCols,
      aggregation_sequence = char_aggsequence,
      aggregation_funsequence = char_funsequence,
      namehistory = namehistory,
      keepAllPolys = keepAllPolys,
      auto_ewr_PU = auto_ewr_PU,
      aggReturn = returnList,
      agg_finish_time = format(Sys.time(), digits = 0, usetz = TRUE),
      agg_status = TRUE,
      agg_HydroBOT_version = as.character(utils::packageVersion('HydroBOT'))
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
      ymlmods <- NULL
    }

    yaml::write_yaml(c(ymlmods, agg_params),
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
      jsonlite::write_json(c(jsonmods, agg_params),
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
