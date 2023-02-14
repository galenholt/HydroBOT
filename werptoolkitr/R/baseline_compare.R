baseline_compare <- function(val_df, scene_col, base_lev, values_col, comp_fun, ...,
                             failmissing = TRUE,
                             names_to = 'name', values_to = 'value') {

  # generate the standard format with the reference
  val_df <- create_base(val_df, scene_col, base_lev, values_col, failmissing, names_to, values_to)

  # We have to generate valcols both in create_base and here, unfortunately.And
  # Deal with the expected change to multiple valcols
  if (length(values_col) > 1) {values_col <- values_to}
  valcols <- selectcreator(rlang::enquo(values_col), val_df, failmissing)


  # The functions here are fundamentally different than in the aggregation-
  # there we were worried about groups and different aggregations, here we're
  # always comparing two cols with known names.

  # Should only be one valcol by this point, but stay safe. Different functions
  # for different variables not yet implemented. Pass in separately for now

  # if funlist is a bare function, leave it alone but get its name
  # https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
  funname <- as.character(substitute(comp_fun))
  # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
  nameparser <- paste0(funname,'_{.col}')

  val_df <- val_df %>%
    dplyr::mutate(dplyr::across({{valcols}},
                                ~comp_fun(.data[[valcols]],
                                          .data[[stringr::str_c('ref_', valcols)]], ...),
                                .names = nameparser))

  return(val_df)

}


create_base <- function(val_df, scene_col, base_lev, values_col,
                             failmissing = TRUE, names_to, values_to) {


# Create the ref_ column --------------------------------------------------
  # Should probably split this out into a separate function.

  # Get the scenario columns however they were passed
  compcols <- selectcreator(rlang::enquo(scene_col), val_df, failmissing)

  valcols <- selectcreator(rlang::enquo(values_col), val_df, failmissing)

  # save for later but allow changes to valcols itself
  orig_valcols <- valcols

  # Enforce long data for scenarios, let there by arbitrary numbers of values
  # columns. e.g. could be flow, stage height etc or aggregation 1, aggregation
  # 2, etc... It's FAR easier to write the mutate if I make these long. Will
  # just use the default "name" and "value" to keep general
  if (length(valcols) > 1) {
    rlang::warn("data with multiple values columns. Making long and will return long with column names in 'name' column")
    val_df <- val_df %>% tidyr::pivot_longer({{valcols}}, names_to = names_to, values_to = values_to)
    valcols <- values_to
  }

  # Get the grouping columns- assume everything but the scene cols and values
  # cols- otherwise we'd end up duplicating values. Maybe that's OK, but needs
  # more thought and clear tests
  groupcols <- names(val_df)[which(!(names(val_df) %in% c(compcols, valcols)))]


  # Deal with the most-common situation- baselev as a single lev in the scenario column
  if (is.character(base_lev) &&
      length(compcols) == 1 &&
      (base_lev %in% dplyr::pull(val_df, var = {{compcols}}))) {
    refdf <- dplyr::filter(val_df, .data[[compcols]] == base_lev) %>%
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', .),
                         .cols = tidyselect::all_of(valcols)) %>%
      dplyr::select(-{{compcols}})
    val_df <- dplyr::left_join(val_df, refdf, by = groupcols)
  }

  # The easiest thing to do is if base_lev is a scalar
  if ((length(base_lev) == 1) & (is.numeric(base_lev))) {
    val_df <- val_df %>%
      dplyr::mutate(refcol = base_lev) %>%
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', valcols),
                         .cols = refcol)
  }

  # we might also want to feed base_lev as a dataframe (e.g. as historical
  # average or something). Need a real use-case I think to build it
  if (is.data.frame(base_lev)) {
    rlang::abort("a baseline dataframe is a good idea but not yet implemented")
  }

  # and we might want to feed numeric values for each of several valcols. I
  # think mods to this to expand_grid to groupcols would end up looking like the
  # passed dataframe thing above needs to look
  if ((((length(orig_valcols) > 1) &
        (length(base_lev) == length(orig_valcols)))) &
      (is.numeric(base_lev))) {
    refdf <- tibble::tibble(name = orig_valcols, refcol = base_lev) %>%
      dplyr::rename_with(.fn = ~stringr::str_c('ref_', valcols),
                         .cols = refcol)  %>%
      dplyr::rename_with(.fn = ~names_to,
                         .cols = name)
    val_df <- val_df %>%
      dplyr::left_join(refdf, by = names_to)
  }

  return(val_df)

}
