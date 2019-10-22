# this scripts fetches and clean the data
# note that qualtrics credentials for the API are needed

library(qualtRics)
library(tidyverse)
library(lubridate)

#browse all surveys
surveys <- all_surveys()
surveys

#we want data from the 5th one in the list
survey_df <- fetch_survey(surveys$id[5], convert = FALSE)
names(survey_df)

#lets clean and mutate vars
survey_df %>% 
  select(ResponseID:EndDate, Finished, age:audit, a_control, a_treatment, fmc_a, 
         b_control, b_placebo, b_treatment, fmc_b, c_control, c_treatment, d_control,
         d_treatment, e_control, e_placebo, e_treatment, fmc_e, placebo_1_direct:sponsor, 
         LocationLatitude = lat, LocationLongitude = lat, LocationAccuracy) %>% 
  mutate(imc_1 = ifelse(is.na(IMC_1_1) & is.na(IMC_1_2) & is.na(IMC_1_3) & is.na(IMC_1_4) & is.na(IMC_1_5) & is.na(IMC_1_6), 
                        1, 0), #imc_1 gets value 1 if resp passed, 0 otherwise.
         imc_2 = ifelse(IMC_2_6 == "既非赞同，也非反对", 1, 
                        IMC_2_8 == "强烈同意", 2, 0), #imc_2 gets value 1 or 2 if passed, 0 otherwise
         attentive_imc = ifelse(imc_1 == 1 & imc_2 >= 1, "attentive", "inattentive"), 
         audit = ifelse(is.na(audit), "control", audit) # control did not recive the audit
         )




