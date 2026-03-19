#' An example of a `prepfun` for [read_and_agg()]
#'
#' *This should be considered a template, and does not have general functionality*
#'
#' @param dat
#' @param year_roll
#' @param k_freq
#' @param k_inter
#' @param combo
#'
#' @returns
#' @export
#'
control_achievement <- function(dat, year_roll = 10, k_freq, k_inter, combo) {
  # some cleanup that lets this get used on its own if necessary
  if ('eventYears' %in% names(dat)) {
    names(dat) <- nameclean(names(dat))
  }

  if (!inherits(dat, 'sf')) {
    dat <- join_to_geo(dat, HydroBOT::bom_basin_gauges)
  }

  # assorted cleanup
  dat <- cleanewrs(dat)


  yeardat <- clean_ewr_yearly(dat)

  combo <- functionlister(combo)

  # assess achievement
  outdf <- assess_ewr_flex(yeardat, year_roll = year_roll, k_freq, k_inter, combo)

  return(outdf)
}


#' Example of a fairly complex function to prepare data pre-aggregation (as a `prepfun` argument to [read_and_agg()])
#'
#' *This should be considered a template, and does not have general functionality*
#'
#' @param annualdf
#' @param year_roll
#' @param k_freq
#' @param k_inter
#' @param combo
#'
#' @export
#'
assess_ewr_flex <- function(annualdf, year_roll, k_freq, k_inter, combo) {

  # Need to get the ewr targets to check against
  ewr_requirements <- clean_ewr_requirements()

  # Join target frequencies to annualdf
  annualdf <- dplyr::left_join(annualdf, ewr_requirements,
                               by = c('ewr_code', 'ewr_code_main', 'gauge',
                                      'planning_unit_name', 'state', 'SWSDLName'),
                               relationship = "many-to-many"
  )

  # FLIP EVENTS FOR CEASE-TO-FLOW
  # This makes a 1 a good thing, like all the others.
  # we also have to flip the target frequency; e.g. previously had a target that said we needed to have less than 80% ceases, we now need to have more than 20% not-ceases
  annualdf <- annualdf |>
    dplyr::mutate(event_years = ifelse(grepl('^CF', .data$ewr_code),
                                       1-.data$event_years, .data$event_years),
                  target_frequency = ifelse(grepl('^CF', .data$ewr_code),
                                            100-.data$target_frequency, .data$target_frequency),
                  max_interevent = .data$max_interevent*365)

  # Frequency checks (ACHIEVEMENT test)

  # calculate number of event years, frequency, and EWR pass/fail at defined (year_roll) year rolling time frames.
  # cease to flows are the inverse of success.
  annualdf <- annualdf |>
    # dplyr::group_by(scenario, planning_unit_name, gauge, ewr_code, ewr_code_main) |>
    dplyr::arrange(.data$scenario, .data$planning_unit_name,
                   .data$gauge, .data$ewr_code, .data$ewr_code_main,
                   .data$year) |>
    dplyr::mutate(frequency_occurred = roll_frequency(.data$event_years, year_roll),
                  # the interevents are highly variable (and often sub-yearly),
                  # and there's already a rolling interevent col
                  .by = c("scenario", "planning_unit_name", 'state', 'SWSDLName',
                          "gauge", "ewr_code", "ewr_code_main"))

  annualdf <- annualdf |>
    # dplyr::rowwise() |> # This will be slow, should I vectorise adjusted_logistic? probably.
    dplyr::mutate(
      # frequency_achieved = .data$frequency_occurred >= .data$target_frequency,
                  frequency_achieved = adjusted_logistic(x = .data$frequency_occurred,
                                                         threshold = .data$target_frequency,
                                                         k = k_freq, thresh_is = 'min'),
                  interevent_achieved = adjusted_logistic(x = .data$rolling_max_inter_event,
                                                          threshold = .data$max_interevent,
                                                          k = k_inter, thresh_is = 'max'),

                  # interevent_achieved = as.numeric(.data$rolling_max_inter_event <= .data$max_interevent),
                  # both have to occur for the EWR to 'pass'
                  ewr_achieved = combo[[1]](.data$frequency_achieved, .data$interevent_achieved)
      ) |>
    # dplyr::ungroup() |>
    # Not sure we want this.
    dplyr::mutate(k_freq = k_freq, k_inter = k_inter, combo = names(combo))

  annualdf <- annualdf |>
    dplyr::select('scenario', 'year', 'date', 'gauge',
                  'planning_unit_name', 'state', 'SWSDLName',
                  'ewr_code', 'ewr_code_main',
                  'event_years', 'frequency_achieved',
                  'interevent_achieved', 'ewr_achieved',
                  'k_freq', 'k_inter', 'combo'
                  )

  return(annualdf)
}


#' A modified logistic function to use for achievement assessment with thresholds
#'
#' @param x the input value
#' @param threshold the threshold to compare against
#' @param k the k of the logistic
#' @param mid the midpoint of logistic, default 0 *be very careful if change*
#' @param thresh_is is the threshold a 'min' or a 'max', e.g. maximum interevents are a 'max', frequency requirements are a 'min'
#'
#' @returns a value
#' @export
#'
#' @examples
adjusted_logistic <- function(x, threshold, k, mid = 0, thresh_is) {
  # relativise the x to the threshold

  # The threshold is the maximum acceptable x for 'x'. anything over the
  # threshold should fail, anything under pass (in a binary sense) (e.g. a
  # maximum interevent)
  if (thresh_is == 'max') {
    divx <- 1-(x-threshold)/threshold
  }
  # The threshold is the minimum acceptable x for 'x'. anything under
  # the threshold should fail, anything over should pass (e.g. a required
  # frequency)
  if (thresh_is == 'min') {
    divx <- x/threshold
  }


  # overload k to get linear
  if (k < 0) {
    # and with this special x, cap at 1
    if (k == -1) {
      divx[divx > 1] <- 1
    }
    y <- divx
  }

  if (k >= 0) {
    # Get the logistic with the mid set relative to the threshold as 1.
    y <- logistic(divx, k, findmid(k,1,mid))
  }

  # zero-force (input xs of zero shouldn't gain x)
  if (thresh_is == 'min') {
    xto0 <- which(!is.na(x) & x<=0)
    y[xto0] <- 0
  }

  # 1-force (for max thresholds, if the threshold is 0, xs of 0 should yield 1)
  if (thresh_is == 'max') {
    # need the y<1 because linear with k not -1 will be ABOVE 1 in a lot of
    # cases, and we don't want to mess with that. It's NaN with a threshold of 0
    # due to /0, but we still want that to pass
    xto1 <- which(!is.na(x) & x<= 0 & ((y<1)|is.nan(y)))
    y[xto1] <- 1
  }

  # and don't allow negatives (only show up in linear)
  y[y < 0] <- 0

  return(y)
}


# Helpers
# The bare logistic (with the max fixed at 1)
logistic <- function(x, k, mid) {
  1/(1+exp(-k*(x-mid)))
}

# find the midpoint to use to get a logistic relative to the threshold
findmid <- function(k, threshold, mid = 0) {
  # way overshoot. Could write a search loop, but this is easier.
  x <- seq(0, 100, 0.001)
  y <- logistic(x, k, mid) # the standard logistic has mid = 0
  xtop <- x[min(which(y>=0.999))]

  return(threshold-xtop)
}

