# file to prep data for placebo research note (ie remove other lists etc from the ds to be shared)

library(tidyverse)

# import survey data
df <- rio::import("data/survey-data.csv") %>% 
  filter(using == "yes") %>% #this filters out duplicated pilot rounds data and duplicated IP numbers
  select(age, income, education, work, female, urban_hukou, 
         b_treatment, b_control, b_placebo)

names(df)

write_csv(df, path = "psrm_replication/data/psrm-ds.csv")
