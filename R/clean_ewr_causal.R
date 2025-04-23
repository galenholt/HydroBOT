#' Cleans causal networks that come in from py-ewr
#'
#' @param ewrnet causal network returned by pdi$get_causal_ewr()
#' @param verbose default FALSE, if TRUE, print out a bunch of unique values for troubleshooting
#'
#' @returns a list of dataframes
#' @keywords internal
#'
clean_ewr_causal <- function(ewrnet, verbose = FALSE) {
  if (!grepl('2.3.7', get_ewr_version())) {
    rlang::warn(c("!" = "Cleanup code written for causal network provided by py-ewr 2.3.7.",
                  glue::glue("you have {get_ewr_version()}."),
                "Each version is likely to have new issues and need new cleanings. This should still catch basic typos, but check your network carefully.",
                "i" = "try `verbose = TRUE` for some simple diagnostics."))
  }

  # don't modify in place so I can double check
  e2o <- ewrnet$ewr2obj |>
    dplyr::mutate(env_obj = stringr::str_remove_all(.data$env_obj, '\n'),
                  env_obj = stringr::str_remove_all(.data$env_obj, ' $')) |>
    dplyr::mutate(Target = stringr::str_to_sentence(.data$Target),
                  Target = ifelse(grepl('Ecosystem functions', .data$Target),
                                  'Ecosystem function', .data$Target),
                  Target = ifelse(grepl('Q-NF', .data$env_obj) &
                                    is.na(.data$Target),
                                  'Native fish', .data$Target)) |>
    dplyr::distinct()

  o2t <- ewrnet$obj2target |>
    dplyr::mutate(Target = ifelse(grepl('Ecosystem functions', .data$Target),
                                  'Ecosystem function', .data$Target),
                  Target = ifelse(grepl('Priority ecosystem function',
                                        .data$Target),
                                  'Ecosystem function', .data$Target),
                  Target = stringr::str_to_sentence(.data$Target),
                  Target = ifelse(grepl('Waterbird', .data$Target), 'Waterbirds',
                                  .data$Target),
                  Target = ifelse(grepl('Q-NF', .data$env_obj) &
                                    is.na(.data$Target),
                                  'Native fish', .data$Target)
                  ) |>
    dplyr::distinct()

  o2yt <- ewrnet$obj2yrtarget |>
    dplyr::mutate(Target = ifelse(grepl('Priority ecosystem function', .data$Target),
                                  'Ecosystem function', .data$Target),
                  Target = ifelse(grepl('Waterbird', .data$Target),
                                  'Waterbirds', .data$Target),
                  Target = ifelse(grepl('Q-NF', .data$env_obj) & is.na(.data$Target),
                                  'Native fish', .data$Target)
    ) |>
    dplyr::distinct()

  if (verbose) {

    rlang::inform(c(i = "EWR CODES"))

    rlang::inform(
      glue::glue(
        "Unique `ewr_code_timing` are\n
        {paste0(unique(e2o$ewr_code_timing), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `ewr_code` are\n
        {paste0(unique(e2o$ewr_code), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "ENV_OBJ"))


    rlang::inform(
      glue::glue(
        "Unique `env_obj` in ewr2obj are\n
        {paste0(unique(e2o$env_obj), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `env_obj` in obj2target are\n
        {paste0(unique(o2t$env_obj), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "The `env_obj` codes
      {paste0(setdiff(o2t$env_obj, e2o$env_obj), collapse = '\n')}
      are in obj2target but not in ewr2obj.
        The codes
        {paste0(setdiff(e2o$env_obj, o2t$env_obj), collapse = '\n')}
        are in ewr2obj but not obj2target"
      )
    )

    rlang::inform(c(i = "TARGET"))


    rlang::inform(
      glue::glue(
        "Unique `Target` in ewr2obj are\n
        {paste0(unique(e2o$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Target` in obj2target are\n
        {paste0(unique(o2t$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Target` in o2yt are\n
        {paste0(unique(o2yt$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "The `Targets` in e2o and o2t differ by
        {paste0(setdiff(o2t$Target, e2o$Target), collapse = '\n')}
      and between o2t and o2yt by
      {paste0(setdiff(o2t$Target, o2yt$Target), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "Specific_goal"))


    rlang::inform(
      glue::glue(
        "Unique `Specific_goal` in obj2target are\n
        {paste0(unique(o2t$Specific_goal), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "Objective"))


    rlang::inform(
      glue::glue(
        "Unique `Objective` in obj2target are\n
        {paste0(unique(o2t$Objective), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Objective` in obj2yrtarget are\n
        {paste0(unique(o2yt$Objective), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "check values in Objective and yrtargets manually"))


  }


  return(list(ewr2obj = e2o, obj2target = o2t, obj2yrtarget = o2yt))


}
