# 35B150--Tucannon R. nr Marengo- Downloaded from Department of Ecology 
# Raw text files removed to trim app
# e.g. 35B150_2005_STG_DV, 35B150_2014_WTM_DV

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


# Processing function ----

read_text_files <- function(files, exclude_chrs = NULL) {
  
  data <- purrr::map(files, function(file){
    
    lines <- readr::read_lines(file)
    
    # Remove non-ASCII characters
    lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "")
    # lines <- gsub("[^\\x00-\\x7F]", "", lines)
    if(!is.null(exclude_chrs)){
      assertthat::assert_that(is.character(exclude_chrs),
                              msg = "exclude_chr must be a character if provided")
      assertthat::assert_that(length(exclude_chrs) == 1,
                              msg = "exclude_chr must have a length of 1. If multiple characters should be excluded, use the | opperator in the regex")
      lines <- gsub(exclude_chrs, "", lines)
    }
    
    
    date_pattern <- "\\b\\d{2}/\\d{2}/\\d{4}\\b"
    
    header <- which(grepl(date_pattern, lines))[1] - 2
    headers <- toupper(stringr::str_split(lines[header], "\\s{2,}")[[1]])
    
    dash_line <- lines[header + 1]
    
    # Find the start and end of each series of dashes
    matches <- stringr::str_match_all(dash_line, "(-+)")
    dash_spans <- matches[[1]]
    # Use gregexpr to get start positions of each dash group
    starts <- unlist(gregexpr("-+", dash_line))
    breaks <- c(starts[-1]-1, nchar(dash_line))
    
    # check for quality codes
    if(any(stringr::str_detect(lines, "Quality Codes Key:"))){
      # If present, grab to merge on later
      quality <- TRUE
      quality_start <- which(stringr::str_detect(lines, "Quality Codes Key:")) - 1
      quality_lines <- which(stringr::str_detect(lines, "\\d+\\s-\\s.*"))
      quality_lines <- quality_lines[quality_lines > quality_start]
      quality_lines <- lines[quality_lines]
      quality_lines <- stringr::str_split(quality_lines, 
                                          pattern = "\\s-\\s", 
                                          n = 2, 
                                          simplify = TRUE)
      quality_lines <- as.data.frame(quality_lines)
      colnames(quality_lines) <- c("QUALITY", "QUALITY_KEY")
      
    } else {
      quality <- FALSE
    }
    
    lines <- lines[grepl(date_pattern, lines)]
    lines <- lines[!grepl("---{4,}", lines)]
    
    if (length(lines) == 0) {
      return(
        data.frame(setNames(as.list(c(.x, rep("", length(headers)))), 
                            c("file_id", headers)), 
                   stringsAsFactors = FALSE)
      )
    } else {
      # Read the fixed-width data
      df <- purrr::map(lines, function(line){
        
        x <- purrr::map2(starts, breaks, ~ str_trim(str_sub(line, .x, .y))) %>% 
          as.data.frame()
        colnames(x) <- headers
        return(x)
        
      }) %>% 
        dplyr::bind_rows()
      
      rownames(df) <- NULL
      
      if(quality){
        df <- dplyr::left_join(df, quality_lines, by = "QUALITY")
      }
      
      df <- df %>% 
        dplyr::mutate(file_id = file) %>% 
        dplyr::relocate(file_id)
      
      return(df)
      
    }
    
  }) %>% 
    dplyr::bind_rows() %>% 
    # dplyr::filter(str_detect(Result, "\\d")) %>%
    dplyr::mutate(
      Date = lubridate::mdy(DATE),
      Year = lubridate::year(Date),
      Month = lubridate::month(Date, abbr = TRUE, label = TRUE),
      # Result = ifelse(Year %in% c(2003:2011), as.numeric(Result), as.numeric(Time))
    ) %>%
    # Replace all "\"\"" with ""
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~stringr::str_replace(.x, "\"\"", ""))) %>% 
    dplyr::select(-c(file_id, TIME, DATE))
  
  # Make QUALITY numeric
  if("QUALITY" %in% colnames(data)){
    data <- data %>% 
      dplyr::mutate(QUALITY = as.numeric(QUALITY))
  }
  
  
  return(data)
}

# water data ----

water_files <- list.files("inst/marengo_raw/", full.names = TRUE, pattern = "WTM")

# Check inputs

purrr::walk(water_files, function(file){
  
  lines <- readr::read_lines(file)
  
  cat(rep("=", 10))
  cat("\n")
  cat(print(file))
  cat("\n")
  
  print(head(lines, n = 15))
  cat("\n")
  print(tail(lines, n=15))
  cat("\n")
  cat(print(paste0("Detect codes:", any(stringr::str_detect(lines, "Quality Codes Key:")))))
  cat("\n\n")
  
  })

# Appears 35B150_2003_WTM_DV.TXT is the only without quality codes.
# 3 different methods of using the degree symbol, so just removing (one removed by removing no-ascii in code.

marengo_water <- read_text_files(files = water_files, "[�]|°") 

glimpse(marengo_water)

marengo_water %>% cnt(QUALITY, QUALITY_KEY)
# QUALITY QUALITY_KEY                                n      n_cumulative
# 1       Good quality REVIEWED data                 506    506
# 1       NA                                         384    890
# 2       NA                                         100    990
# 140     Data not yet checked                       6602   7592
# 230     Instrument impaired                        8      7600
# 250     Ice impacted data (will not be reported)   3      7603
# 255     Incomplete day                             30     7633
# 255     No Data                                    27     7660
# 255     NA                                         246    7906

marengo_water %>% 
  dplyr::mutate(check = `WATER TEMP. (C)` == "" | is.na(`WATER TEMP. (C)`)) %>% 
  cnt(QUALITY, check)
# QUALITY check     n n_cumulative
# 1       FALSE   890          890
# 2       FALSE   100          990
# 140     FALSE  6602         7592
# 230     TRUE      8         7600
# 250     TRUE      3         7603
# 255     TRUE    303         7906

# Removing where "" or NA get rid of all missing data
# May still need to remove QAULITY = 140
marengo_water <- marengo_water %>% 
  dplyr::filter(!(`WATER TEMP. (C)` == ""), !is.na(`WATER TEMP. (C)`)) %>% 
  dplyr::rename(Result = `WATER TEMP. (C)`) %>% 
  dplyr::mutate(Result = as.numeric(Result))

glimpse(marengo_water)

marengo_water %>% 
  cnt(QUALITY)
# QUALITY     n n_cumulative
# 1         890          890
# 2         100          990
# 140      6602         7592

marengo_water %>% cnt(Year) %>% print(n=Inf)
# Year     n n_cumulative
# 2003   212          212
# 2004   365          577
# 2005   364          941
# 2006   361         1302
# 2007   365         1667
# 2008   356         2023
# 2009   352         2375
# 2010   365         2740
# 2011   365         3105
# 2012   365         3470
# 2013   366         3836
# 2014   352         4188
# 2015   365         4553
# 2016   366         4919
# 2017   362         5281
# 2018   365         5646
# 2019   366         6012
# 2020   364         6376
# 2021   363         6739
# 2022   355         7094
# 2023   366         7460
# 2024   132         7592

data.table::fwrite(x = marengo_water,"inst/marengo_processed/marengo_water.csv")

# stage data ----

guage_files <- list.files("inst/marengo_raw/", full.names = TRUE, pattern = "STG")

purrr::walk(guage_files, function(file){
  
  lines <- readr::read_lines(file)
  
  cat(rep("=", 10))
  cat("\n")
  cat(print(file))
  cat("\n")
  
  print(head(lines, n = 15))
  cat("\n")
  print(tail(lines, n=15))
  cat("\n")
  cat(print(paste0("Detect codes:", any(stringr::str_detect(lines, "Quality Codes Key:")))))
  cat("\n\n")
  
})

# Appears 35B150_2003_STG_DV.TXT is the only without quality codes.

marengo_stage <- read_text_files(files = guage_files)

glimpse(marengo_stage)

marengo_stage %>% cnt(QUALITY, QUALITY_KEY)
# QUALITY QUALITY_KEY                                       n n_cumulative
# 1       Good quality REVIEWED data                     4403         4403
# 1       NA                                              119         4522
# 2       Good quality provisional data                  2590         7112
# 2       NA                                                1         7113
# 3       Good quality provisional data - edited           22         7135
# 50      Estimated data                                  152         7287
# 50      NA                                               59         7346
# 77      Estimated from another station, same variable    81         7427
# 82      Linear interpolation across gap in records.       4         7431
# 140     Data not yet checked                             41         7472
# 215     Data rejected                                     5         7477
# 230     Instrument impaired                              68         7545
# 250     Ice impacted data (will not be reported)         77         7622
# 255     Incomplete day                                   38         7660
# 255     NA                                              246         7906

marengo_stage %>% 
  dplyr::mutate(check = `STAGE (FT.)` == "" | is.na(`STAGE (FT.)`)) %>% 
  cnt(QUALITY, check)
# QUALITY check     n n_cumulative
# 1       FALSE  4522         4522
# 2       FALSE  2591         7113
# 3       FALSE    22         7135
# 50      FALSE   211         7346
# 77      FALSE    81         7427
# 82      FALSE     4         7431
# 140     FALSE    41         7472
# 215     TRUE      5         7477
# 230     TRUE     68         7545
# 250     TRUE     77         7622
# 255     TRUE    284         7906

# Removing where "" or NA get rid of all missing data
# May still need to remove QAULITY = 140
marengo_stage <- marengo_stage %>% 
  dplyr::filter(!(`STAGE (FT.)` == ""), !is.na(`STAGE (FT.)`)) %>% 
  dplyr::rename(Result = `STAGE (FT.)`) %>% 
  dplyr::mutate(Result = as.numeric(Result))

glimpse(marengo_stage)

marengo_stage %>% 
  cnt(QUALITY)
# QUALITY     n n_cumulative
# 1        4522         4522
# 2        2591         7113
# 3          22         7135
# 50        211         7346
# 77         81         7427
# 82          4         7431
# 140        41         7472

marengo_stage %>% cnt(Year) %>% print(n=Inf)
# Year     n n_cumulative
# 2003   212          212
# 2004   360          572
# 2005   364          936
# 2006   365         1301
# 2007   362         1663
# 2008   346         2009
# 2009   357         2366
# 2010   362         2728
# 2011   361         3089
# 2012   366         3455
# 2013   360         3815
# 2014   364         4179
# 2015   366         4545
# 2016   366         4911
# 2017   283         5194
# 2018   365         5559
# 2019   366         5925
# 2020   365         6290
# 2021   334         6624
# 2022   357         6981
# 2023   366         7347
# 2024   125         7472

data.table::fwrite(x = marengo_stage,"inst/marengo_processed/marengo_stage.csv")




