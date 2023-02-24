#' Function to ensure colors match across plots
#'
#' @param levels character vector of levels to use (should be all possible for the variable)
#' @param palette character vector of palette name from {paletteer}
#' @param refvals character vector of levels we want to set manually (typically as references)
#' @param refcols character vector (names or hex) of colors to set manually in order matching `refvals`
#' @param includeRef logical, defailt FALSE. Should the colors be chosen including (TRUE) or not including (FALSE, the default) the levels in `refvals`. Typically `TRUE` if we sometimes want to accentuate a level, and sometimes not.
#' @param returnUnref logical, default FALSE. Should we return the base palette with the refvals matched to `palette` in addition to the version where they match `refcols` (TRUE). Only works if `includeRef = TRUE`. If `TRUE`, returns a list with the base palette and the palette matching `refcols`
#'
#' @return typically a named character vector of colours, unless `includeRef = TRUE` and `returnUnref = TRUE`. In that case, a list of two named colour vectors.
#' @export
#'
#' @examples
make_pal <- function(levels, palette,
                     refvals = NULL, refcols = NULL,
                     includeRef = FALSE, returnUnref = FALSE) {

  # need some name references to know what function to use
  cnames <- paletteer::palettes_c_names %>%
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) %>%
    dplyr::select(formatted) %>% dplyr::pull()

  dnames <- paletteer::palettes_d_names %>%
    dplyr::mutate(formatted = stringr::str_c(package, palette, sep = '::')) %>%
    dplyr::select(formatted) %>% dplyr::pull()

  if (returnUnref) {
    if (!includeRef) {
      stop("does not make sense to return a reffed and unreffed palette that don't match")
    }
  }

  if (is.factor(levels)) {levels <- as.character(levels)}

  if (!includeRef) {levels <- levels[!(levels %in% refvals)]}

  if (palette %in% cnames) {
    cols <- paletteer::paletteer_c(palette, length(levels))
  } else if (palette %in% dnames) {
    cols <- paletteer::paletteer_d(palette, length(levels))
  }
  # cols <- paletteer::paletteer_d(palette, length(levels))

  if (returnUnref & includeRef) {unref <- setNames(cols, levels)}

  # delete the reference levels out of the vectors
  whichlevs <- which(!(levels %in% refvals))
  nonrefs <- levels[whichlevs]
  nonrefcols <- cols[whichlevs]

  namedcols <- setNames(c(refcols, nonrefcols), c(refvals, nonrefs))
  class(namedcols) <- 'colors'

  if (returnUnref & includeRef) {
    return(list(refcols = namedcols, unrefcols = unref))
  } else {
    return(namedcols)
  }

}
