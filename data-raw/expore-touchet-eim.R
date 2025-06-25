source("R/zzz.R")

touchet_params <- data.table::fread("inst/touchet_eim_raw/EIMDiscreteResults_2025Jun05_8112.csv")

touchet_params %>% cnt(Study_ID, Study_Name)
touchet_params %>% cnt(Location_ID, Location_Name)
touchet_params %>% cnt(Study_Specific_Location_ID)

c("32K070","32E050","32B100") %in% unique(touchet_params$Location_ID)

touchet_eim_stu <- data.table::fread("inst/touchet_eim_raw/EIMStudyDetails_2025Jun05_14.csv")
touchet_eim_stu %>% cnt(Study_ID, Study_Name, EIM_Data_Entry_Review_Status, Study_QA_Assessment_Level)
touchet_eim_stu %>% cnt(Study_QA_Assessment_Level)

touchet_eim_stu <- touchet_eim_stu %>% 
  filter(!stringr::str_detect(Study_QA_Assessment_Level, "Level 1:"))

touchet_eim_stu %>% cnt(Study_ID, Study_Name, EIM_Data_Entry_Review_Status, Study_QA_Assessment_Level)

# Think about dropping where EIM Data Entry Review is "Not Reviewed"

touchet_eim_stu <- touchet_eim_stu %>% pull(Study_ID) %>% unique()

touchet_params <- touchet_params %>% 
  filter(Study_ID %in% touchet_eim_stu#,
         #Location_ID %in% c("32K070","32E050","32B100")
         )

touchet_params %>% cnt(Study_ID, Study_Name)
touchet_params %>% cnt(Location_ID, Location_Name)

touchet_params %>% cnt(Result_Parameter_Name)

touchet_params <- touchet_params %>% 
  dplyr::transmute(
    Param = Result_Parameter_Name,
    Date = lubridate::mdy(Field_Collection_Start_Date),
    Year = lubridate::year(Date),
    Units = Result_Value_Units,
    Month = lubridate::month(Date, label = TRUE, abbr = TRUE),
    Result = as.numeric(Result_Value)
  )

write.csv(touchet_params, file = "inst/touchet_eim_processed/touchet_filtered.csv")
