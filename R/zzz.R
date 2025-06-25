# utilities

cnt <- function(
    .data, 
    ..., 
    n_distinct_vars = NULL, 
    n_distinct_combined = TRUE, 
    n_cumulative = TRUE, 
    prop = FALSE, 
    pct = FALSE
) {
  
  # set visible bindings
  n <- NULL
  
  assertthat::assert_that(
    is.data.frame(.data),
    is.logical(n_distinct_combined),
    is.logical(n_cumulative),
    is.logical(prop),
    is.logical(pct)
  )
  
  # if more than 1 n_distinct_vars are provided, create a variable that combines
  # records across all n_distinct_vars and count the unique number of
  # combinations
  n_distinct_vars_names <- dplyr::select(.data, {{ n_distinct_vars }}) %>% 
    names()
  
  if(length(n_distinct_vars_names) <= 1 || !isTRUE(n_distinct_combined)) {
    
    n_distinct_vars_all_name <- NULL
    
  } else {
    
    n_distinct_vars_all_name <- paste0(n_distinct_vars_names, collapse = "_")
    
    .data <- .data %>% 
      tidyr::unite(
        col = !!n_distinct_vars_all_name,
        {{ n_distinct_vars }},
        sep = "_",
        remove = FALSE,
        na.rm = FALSE
      )
    
  }
  
  # variables that will be dropped
  drop_vars_names <- c("n_cumulative", "prop", "pct")[!c(n_cumulative, prop, pct)]
  
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      dplyr::across(.cols = c({{ n_distinct_vars }}, dplyr::any_of(n_distinct_vars_all_name)),
                    .fns = dplyr::n_distinct,
                    .names = "n_{col}"),
      n = dplyr::n(),
      prop = n / nrow(.),
      pct = 100 * n / nrow(.),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_cumulative = cumsum(n)
    ) %>% 
    dplyr::select(
      -dplyr::any_of(drop_vars_names)
    )
  
}
