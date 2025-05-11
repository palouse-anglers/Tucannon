filter_data_bytime <- function(data, year_col = "Year", month_col = "Month", year_range, month_vals){
  
  assertthat::assert_that(is.data.frame(data),
                          msg = "data must be a data.frame.")
  
  assertthat::assert_that(is.character(year_col),
                          msg = "year_col must be a character.")
  assertthat::assert_that(length(year_col) == 1,
                          msg = "year_col must have a length of 1.")
  assertthat::assert_that(year_col %in% colnames(data),
                          msg = "year_col must be in data.")
  
  assertthat::assert_that(is.character(month_col),
                          msg = "month_col must be a character.")
  assertthat::assert_that(length(month_col) == 1,
                          msg = "month_col must have a length of 1.")
  assertthat::assert_that(month_col %in% colnames(data),
                          msg = "month_col must be in data.")
  
  assertthat::assert_that(length(year_range) == 2,
                          msg = "The year range for data should have two values.")
  assertthat::assert_that(is.numeric(year_range),
                          msg = "The year range for data must be numeric.")
  
  assertthat::assert_that(is.character(month_vals),
                          msg = "The month values for data must be characters.")
  # assertthat::assert_that(length(month_vals) > 0,
  #                         msg = "The month_vals for data should have at least one value.")
  assertthat::assert_that(all(month_vals %in% unique(data[[month_col]])),
                          msg = "Selected months are not in data.")
  
  output <- data %>%
    dplyr::filter(!!sym(year_col) >= year_range[1] & !!sym(year_col) <= year_range[2]) %>% 
    dplyr::filter(!!sym(month_col) %in% month_vals)
  
  return(output)
  
}

# filter_data_bytime(params, year_range = c(2011, 2022), month_vals = c("Jan","May")) %>% cnt(Year, Month) %>% print(n = Inf)
# filter_data_bytime(station_stage, year_range = c(2011, 2022), month_vals = c("Jan","May")) %>% cnt(Year, Month) %>% print(n = Inf)


filter_params <- function(data, param_vals, group_vars = c("Param", "Date", "Year", "Month"), round_dig = NULL, arr_vars = c("Year", "Month"), res_gt0 = FALSE){
  
  old_inform <- getOption("dplyr.summarise.inform")  # capture the old value
  options(dplyr.summarise.inform = FALSE)            # set new value
  on.exit(options(dplyr.summarise.inform = old_inform), add = TRUE)  # restore on exit
  
  
  assertthat::assert_that(is.data.frame(data),
                          msg = "data must be a data.frame.")
  
  assertthat::assert_that(is.character(param_vals),
                          msg = "param_vals must be a character.")
  assertthat::assert_that(length(param_vals) >= 1,
                          msg = "At least one param_vals must be provided.")
  assertthat::assert_that(is.character(group_vars),
                          msg = "group_vars must be a character.")
  assertthat::assert_that(length(group_vars) >= 1,
                          msg = "At least one group_vars must be provided.")
  assertthat::assert_that(is.character(arr_vars),
                          msg = "arr_vars must be a character.")
  assertthat::assert_that(length(arr_vars) >= 1,
                          msg = "At least one arr_vars must be provided.")
  
  assertthat::assert_that(all(c(group_vars, arr_vars, "Result", "Param") %in% colnames(data)),
                          msg = "Required columns not in dataset.")
  
  assertthat::assert_that(all(param_vals %in% unique(data$Param)),
                          msg = "One of the provided param_vals is not in the data.")
  
  
  if(!is.null(round_dig)){
    assertthat::assert_that(is.numeric(round_dig),
                            msg = "round_dig must be numeric if provided.")
  }
  
  assertthat::assert_that(res_gt0 %in% c(TRUE, FALSE),
                          msg = "res_gt0 must be TRUE or FALSE.")
  
  
  data %>% 
    dplyr::filter(Param %in% param_vals) %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars)))) %>% 
    dplyr::summarise(Result = mean(Result, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    {if(!is.null(round_dig)) dplyr::mutate(., Result = round(Result, round_dig)) else .} %>% 
    dplyr::arrange(!!!syms(arr_vars)) %>% 
    {if(res_gt0) dplyr::filter(., Result > 0) else .}
  
}


# x <- params %>% 
#   filter(Param=="Total Phosphorus") %>%
#   group_by(Date) %>%
#   mutate(Result=round(mean(Result),2))%>%
#   distinct(Date,Result,Units) %>%
#   arrange(Date)%>%
#   ungroup()
# 
# y <- filter_params(data = params, param_vals = "Total Phosphorus", group_vars = c("Date", "Units"), round_dig = 2, arr_vars = "Date") %>% 
#   relocate(Result, .before = "Units")
# 
# identical(x,y)

# x <- params %>% 
#   filter(Param=="Temperature, water") %>%
#   group_by(Date) %>%
#   summarise(Result=mean(Result,na.rm = TRUE)) %>%
#   mutate(Month=lubridate::month(Date,label=TRUE,abbr = TRUE),
#          Year=lubridate::year(Date)
#   ) %>%
#   arrange(Year,Month) %>%
#   filter(Result >0) %>%
#   ungroup()
# 
# y <- filter_params(data = params, param_vals = "Temperature, water", res_gt0 = TRUE) %>% 
#   select(Date, Result, Month, Year) # rearrange and drop param
# identical(x,y)

x <- filter_data_bytime(params, 
                   year_range = c(2011, 2024), 
                   month_vals = month.abb[c(1:12)]) %>% 
  dplyr::left_join(bmps_byyear, by = "Year")

df <- filter_params(data = x, 
                    param_vals = "Total Suspended Solids", 
                    group_vars = c("Date", "Units"), 
                    round_dig = 2, 
                    arr_vars = "Date")
