#' A wrapper for [purrr::imap()] and [furrr::future_imap()] that bypasses errors and potentially retries them
#'
#' Uses [purrr::safely()] and unpacks the results and errors. Also useful for debugging with a breakpoint
#'
#' @param .x object to iterate over, as in [purrr::imap()]
#' @param .f a function, as in [purrr::imap()]
#' @param ... additional arguments to .f, as in [purrr::imap()]
#' @param retries Number of times to retry any errors. Default 0 (just try once). Retries only make sense if errors are intermittent for some reason (network connectivity, etc)
#' @param parallel Run in parallel with [furrr::future_imap()] (`TRUE`) or sequentially with [purrr::imap()] (`FALSE`, default)
#'
#' @return A vector the same length as `.x`
#' @export
#'
#' @examples
#'safe_imap(as.list(1:10) |> setNames(letters[1:10]),
#'                   \(x, y) ifelse(sample(c(1,2), 1) == 1,
#'                               stop(), paste(x, y)),
#'                   retries = 10) |>
#'  unlist()
#'
#'
safe_imap <- function(.x, .f, ..., retries = 0, parallel = FALSE) {
  whicherrors <- 1:length(.x)
  full_results <- vector(mode = 'list', length = length(.x))
  # the indices, to track which are being filled/left
  orig_indices <- 1:length(.x)
  counter = 0

  while (length(whicherrors) > 0 & counter <= retries) {
    # run the purrr
    if (!parallel) {
      im_out <- purrr::imap(.x, purrr::safely(.f))
    }
    if (parallel) {
      im_out <- furrr::future_imap(.x, purrr::safely(.f),
                               .options = furrr::furrr_options(seed = TRUE))
    }
    # get the results, dropping the NULLs
    results_out <- purrr::map(im_out, purrr::pluck('result'))

    # if we want the errors, we could put in a debug here
    error_out <- purrr::map(im_out, purrr::pluck('error'))

    # replace the indices that were errors with new data. Some might still be errors, they will fill subsequently
    full_results[orig_indices] <- results_out

    # where are the errors
    whicherrors <- purrr::map(im_out,
                              \(x) rlang::is_error(x$error)) |>
      unlist() |>
      which()

    # Cut the data to the fails
    .x <- .x[whicherrors]

    # which ORIGINAL indices are we left failing?
    orig_indices <- orig_indices[whicherrors]

    counter <- counter + 1

  }

  if (length(whicherrors) > 0) {
    rlang::inform(c(glue::glue("The EWR tool has run, but the scenario(s) {names(whicherrors)} have failed and have been bypassed after {retries} retries."),
                    "The first error is:"),
                  glue::glue("{error_out[[1]]}"))
    full_results <- full_results[-orig_indices]
  }

  return(full_results)
}
