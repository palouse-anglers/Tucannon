# 32B100--Touchet-Bolles- Downloaded from Department of Ecology 
# Raw text files removed to trim app
# e.g. 32B100_2005_STG_DV, 32B100_2014_WTM_DV

library(stringr)
source("R/zzz.R")

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

water_files <- list.files("inst/bolles_raw/", full.names = TRUE, pattern = "WTM")

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

# 3 different methods of using the degree symbol, so just removing (one removed by removing no-ascii in code.

water <- read_text_files(files = water_files, "[�]|°") 

glimpse(water)

water %>% cnt(QUALITY, QUALITY_KEY)
# QUALITY QUALITY_KEY                                     n n_cumulative
# 1       Good quality REVIEWED data                    141          141
# 2       Linear interpolation across gap in records.     1          142
# 140     Data not yet checked                         6699         6841
# 255     Incomplete day                                 23         6864
# 255     No Data                                       701         7565

water %>% 
  dplyr::mutate(check = `WATER TEMP. (C)` == "" | is.na(`WATER TEMP. (C)`)) %>% 
  cnt(QUALITY, check)
# QUALITY check     n n_cumulative
# 1       FALSE   141          141
# 82      FALSE     1          142
# 140     FALSE  6699         6841
# 255     TRUE    724         7565

# Removing where "" or NA get rid of all missing data
# May still need to remove QAULITY = 140
water <- water %>% 
  dplyr::filter(!(`WATER TEMP. (C)` == ""), !is.na(`WATER TEMP. (C)`)) %>% 
  dplyr::rename(Result = `WATER TEMP. (C)`) %>% 
  dplyr::mutate(Result = as.numeric(Result))

glimpse(water)

water %>% 
  cnt(QUALITY)
# QUALITY     n n_cumulative
# 1         141          141
# 82          1          142
# 140      6699         6841

water %>% cnt(Year) %>% print(n=Inf)
# Year     n n_cumulative
# 2002   142          142
# 2007   346          488
# 2008   365          853
# 2009   365         1218
# 2010   364         1582
# 2011   364         1946
# 2012   364         2310
# 2013   366         2676
# 2014   359         3035
# 2015   365         3400
# 2016   365         3765
# 2017   364         4129
# 2018   364         4493
# 2019   364         4857
# 2020   367         5224
# 2021   366         5590
# 2022   366         5956
# 2023   366         6322
# 2024   366         6688
# 2025   153         6841

data.table::fwrite(x = water, "inst/bolles_processed/bolles_water.csv")

# stage data ----

guage_files <- list.files("inst/bolles_raw/", full.names = TRUE, pattern = "STG")

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


stage <- read_text_files(files = guage_files)

glimpse(stage)

stage %>% cnt(QUALITY, QUALITY_KEY)
# QUALITY QUALITY_KEY                                       n n_cumulative
# 1       Good quality REVIEWED data                     5325         5325
# 2       Good quality provisional data                   572         5897
# 50      Estimated data                                  246         6143
# 50      NA                                               49         6192
# 77      Estimated from another station, same variable   141         6333
# 82      Linear interpolation across gap in records.       5         6338
# 140     Data not yet checked                             22         6360
# 215     Data rejected                                    96         6456
# 250     Ice impacted data (will not be reported)         75         6531
# 255     Incomplete day                                    3         6534
# 255     No Data                                         665         7199

stage %>% 
  dplyr::mutate(check = `STAGE (FT.)` == "" | is.na(`STAGE (FT.)`)) %>% 
  cnt(QUALITY, check)
# QUALITY check     n n_cumulative
# 1       FALSE  5325         5325
# 2       FALSE   572         5897
# 50      FALSE   295         6192
# 77      FALSE   141         6333
# 82      FALSE     5         6338
# 140     FALSE    22         6360
# 215     TRUE     96         6456
# 250     TRUE     75         6531
# 255     TRUE    668         7199

# Removing where "" or NA get rid of all missing data
# May still need to remove QAULITY = 140
stage <- stage %>% 
  dplyr::filter(!(`STAGE (FT.)` == ""), !is.na(`STAGE (FT.)`)) %>% 
  dplyr::rename(Result = `STAGE (FT.)`) %>% 
  dplyr::mutate(Result = as.numeric(Result))

glimpse(stage)

stage %>% 
  cnt(QUALITY)
# QUALITY     n n_cumulative
# 1        5325         5325
# 2         572         5897
# 50        295         6192
# 77        141         6333
# 82          5         6338
# 140        22         6360

stage %>% cnt(Year) %>% print(n=Inf)
# Year     n n_cumulative
# 2002   174          174
# 2007   348          522
# 2008   358          880
# 2009   360         1240
# 2010   361         1601
# 2011   365         1966
# 2012   367         2333
# 2013   358         2691
# 2014   273         2964
# 2015    92         3056
# 2016   365         3421
# 2017   349         3770
# 2018   366         4136
# 2019   301         4437
# 2020   338         4775
# 2021   363         5138
# 2022   355         5493
# 2023   365         5858
# 2024   355         6213
# 2025   147         6360

data.table::fwrite(x = stage,"inst/bolles_processed/bolles_stage.csv")




