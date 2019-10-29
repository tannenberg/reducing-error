# this script fetches and cleans the data
# note that qualtrics credentials for the API are needed

library(qualtRics)
library(tidyverse)
library(lubridate)

#browse all surveys
surveys <- all_surveys()
surveys

#we want data from the 5th one in the list (at least among my list of surveys)
df <- fetch_survey(surveys$id[5], convert = FALSE, force_request = TRUE)
names(df)

#lets clean and mutate the variables
df <- df %>% 
  select(ResponseID:EndDate, Finished, age:audit, a_control, a_treatment, 
         b_control, b_placebo, b_treatment, c_control, c_treatment, d_control,
         d_treatment, e_control, e_placebo, e_treatment, fmc_a, fmc_b, fmc_e,
         placebo_1_direct:sponsor, 
         lat = LocationLatitude, lng = LocationLongitude, LocationAccuracy) %>% 
  mutate(imc_1 = ifelse(is.na(IMC_1_1) & is.na(IMC_1_2) & is.na(IMC_1_4) & is.na(IMC_1_5) & is.na(IMC_1_6), 
                        1, 0),
         imc_2_1 = ifelse(imc_2_1 == "说不上同意或反对", 1, 0), 
         imc_2_2_inc_somewhat = ifelse(imc_2_2 == "强烈同意" | imc_2_2 == "某种程度上同意", 1, 0),
         imc_2_2 = ifelse(imc_2_2 == "强烈同意", 1, 0),
         attentive_imc = ifelse(imc_1 == 1 & imc_2_1 == 1 & imc_2_2 == 1,
                                1, 0), 
         audit = ifelse(is.na(audit), "control", "received"), 
         fmc_a = ifelse(fmc_a == "在我们国家，空气污染是一个重要的问题。", 1, 0),
         fmc_b = ifelse(fmc_b == "政府就像是家长，应该告诉人民应该做什么。", 1, 0),
         fmc_e = ifelse(fmc_e == "政府为所有人提供优质的公共服务。", 1, 0),
         c_direct_dk = ifelse(c_direct == "不想回答", 1, 0), #for priming checks
         d_direct_dk = ifelse(d_direct == "不想回答", 1, 0), #and filter drop DK
         e_direct_dk = ifelse(e_direct == "不想回答", 1, 0), #in main analysis
         c_direct = ifelse(c_direct == "同意", 1, 0), 
         d_direct = ifelse(d_direct == "同意", 1, 0), 
         e_direct = ifelse(e_direct == "同意", 1, 0),
         placebo_direct = ifelse(placebo_1_direct == "同意" | placebo_2_direct == "同意", 1, 0),
         time_spent = time_length(interval(as_datetime(StartDate), as_datetime(EndDate))) #in seconds!
         )  %>% 
  select(-c(IMC_1_1:IMC_1_6, info, imc_2_3:imc_2_7, placebo_1_direct:placebo_2_direct,
            StartDate, EndDate))

  #lets remove"项" from the treatment, control and placebo groups
  keep_digit <- function(x) gsub("[^0-9]", "", x)
  
  vars <- c("a_control", "a_treatment", "b_control", "b_placebo", "b_treatment",
            "c_control", "c_treatment", "d_control","d_treatment",
            "e_control", "e_placebo", "e_treatment")

  df <-  df %>% mutate_at(vars, keep_digit)
  
# lets write this data in the data folder note we both have to run this 
# script to have tha data locally as i put "survey-data.csv" in the .gitignore 

write_csv(df, "data/survey-data.csv")





