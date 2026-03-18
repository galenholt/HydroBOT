#' Cleans causal networks that come in from py-ewr
#'
#' @param ewrnet causal network returned by pdi$get_causal_ewr()
#' @param verbose default FALSE, if TRUE, print out a bunch of unique values for troubleshooting
#'
#' @returns a list of dataframes
#' @keywords internal
#'
clean_ewr_causal <- function(ewrnet, verbose = FALSE) {
  if (grepl('2.3.7', get_ewr_version())) {
    clean_causal <- clean_ewr_237(ewrnet)
  } else if (grepl('2.4.1', get_ewr_version())) {
    clean_causal <- clean_ewr_241(ewrnet)
  } else {
    rlang::warn(c("!" = "Cleanup code only available for causal network provided by py-ewr 2.3.7 or 2.4.1",
                  glue::glue("you have {get_ewr_version()}."),
                  "Each version is likely to have new issues and need new cleanings. This should still catch basic typos, but check your network carefully.",
                  "i" = "try `verbose = TRUE` for some simple diagnostics."))
  }


  if (verbose) {
    # need to update this for the new situation
    check_causal_issues(clean_causal)
  }


  return(clean_causal)


}


clean_ewr_241 <- function(ewrnet) {
  names(ewrnet) <- nameclean(names(ewrnet))
  clean_causal <- ewrnet |>
    dplyr::rename(eco_objective = eco_obj,
                  objective_text = objective,
                  theme = target)
  return(clean_causal)
}

#' Clean causal network for EWR tool 2.3.7 and thereabouts
#'
#' @param ewrnet a raw causal table to be cleaned
#'
#' @examples
clean_ewr_237 <- function(ewrnet) {
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

  return(list(ewr2obj = e2o, obj2target = o2t, obj2yrtarget = o2yt))
}

check_causal_issues <- function(causal_list) {

  # should write a method
  if (!inherits(clean_causal, 'data.frame')) {
    et <- get_ewr_table()
    unique_causals <- purrr::map(clean_causal, unique)

    purrr::imap(unique_causals, \(x,i) rlang::inform(glue::glue('Unique values of {i} are {paste(x, collapse = ", ")}')))

    }

  # Should have written a method
  if (!inherits(clean_causal, 'data.frame')) {
    rlang::inform(c(i = "EWR CODES"))

    rlang::inform(
      glue::glue(
        "Unique `ewr_code_timing` are\n
        {paste0(unique(causal_list$e2o$ewr_code_timing), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `ewr_code` are\n
        {paste0(unique(causal_list$e2o$ewr_code), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "ENV_OBJ"))


    rlang::inform(
      glue::glue(
        "Unique `env_obj` in ewr2obj are\n
        {paste0(unique(causal_list$e2o$env_obj), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `env_obj` in obj2target are\n
        {paste0(unique(causal_list$o2t$env_obj), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "The `env_obj` codes
      {paste0(setdiff(causal_list$o2t$env_obj, causal_list$e2o$env_obj), collapse = '\n')}
      are in obj2target but not in ewr2obj.
        The codes
        {paste0(setdiff(causal_list$e2o$env_obj, causal_list$o2t$env_obj), collapse = '\n')}
        are in ewr2obj but not obj2target"
      )
    )

    rlang::inform(c(i = "TARGET"))


    rlang::inform(
      glue::glue(
        "Unique `Target` in ewr2obj are\n
        {paste0(unique(causal_list$e2o$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Target` in obj2target are\n
        {paste0(unique(causal_list$o2t$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Target` in o2yt are\n
        {paste0(unique(causal_list$o2yt$Target), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "The `Targets` in e2o and o2t differ by
        {paste0(setdiff(causal_list$o2t$Target, causal_list$e2o$Target), collapse = '\n')}
      and between o2t and o2yt by
      {paste0(setdiff(causal_list$o2t$Target, causal_list$o2yt$Target), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "Specific_goal"))


    rlang::inform(
      glue::glue(
        "Unique `Specific_goal` in obj2target are\n
        {paste0(unique(causal_list$o2t$Specific_goal), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "Objective"))


    rlang::inform(
      glue::glue(
        "Unique `Objective` in obj2target are\n
        {paste0(unique(causal_list$o2t$Objective), collapse = '\n')}"
      )
    )

    rlang::inform(
      glue::glue(
        "Unique `Objective` in obj2yrtarget are\n
        {paste0(unique(causal_list$o2yt$Objective), collapse = '\n')}"
      )
    )

    rlang::inform(c(i = "check values in Objective and yrtargets manually"))
  }

}
