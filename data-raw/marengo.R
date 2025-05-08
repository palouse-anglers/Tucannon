# 35B150--Tucannon R. nr Marengo- Downloaded from Department of Ecology 
# Raw text files removed to trim app
# e.g. 35B150_2005_STG_DV, 35B150_2014_WTM_DV

# Processing function ----

read_text_files <- function(files) {
  
  data <- purrr::map(files, function(file){
    
    lines <- readr::read_lines(file)
    
    # Remove non-ASCII characters
    lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "")
    # lines <- gsub("[^\\x00-\\x7F]", "", lines)
    lines <- gsub("[�]|°", "", lines)
    
    
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
    select(-c(file_id, TIME, DATE))
  
  
  return(data)
}

# water data ----

water_files <- list.files("inst/marengo_raw/", full.names = TRUE, pattern = "WTM")

marengo_water <- read_text_files(files = water_files) %>% 
  dplyr::filter(!(`WATER TEMP. (C)` == "\"\""), !is.na(`WATER TEMP. (C)`)) %>% 
  dplyr::rename(Result = `WATER TEMP. (C)`)

data.table::fwrite(x = marengo_water,"inst/marengo_processed/marengo_water.csv")

# stage data ----

guage_files <- list.files("inst/marengo_raw/", full.names = TRUE, pattern = "STG")

marengo_stage <- read_text_files(files = guage_files) %>% 
  dplyr::filter(!(`STAGE (FT.)` == "\"\""), !is.na(`STAGE (FT.)`)) %>% 
  dplyr::rename(Result = `STAGE (FT.)`)

data.table::fwrite(x = marengo_stage,"inst/marengo_processed/marengo_stage.csv")


