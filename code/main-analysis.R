# this script runs the analysis for list A, B and C 
# and produces figure # and table # in output folder

library(tidyverse)
library(list)
library(broom)
library(estimatr)

#set ggplot theme
theme_set(theme_classic())

# import survey data
df <- rio::import("data/survey-data.csv") %>% 
  filter(using == "yes") #this filters out duplicated pilot rounds data and duplicated IP numbers
names(df)


#lets first create treatment/control indicator and outcome var for all lists
df <- df %>% 
  mutate(treat_a = ifelse(!is.na(a_treatment), 1, 0),
         y_a = ifelse(treat_a==1, a_treatment, 
                      ifelse(treat_a==0, a_control, NA)),
         treat_b = ifelse(!is.na(b_treatment), 1, 0), 
         y_b = ifelse(treat_b==1, b_treatment, 
                      ifelse(treat_b==0, b_control, NA)), 
         y_b_placebo =  ifelse(treat_b==1, b_treatment, 
                               ifelse(treat_b==0, b_placebo, NA)), 
         treat_c = ifelse(!is.na(c_treatment), 1, 0), 
         y_c = ifelse(treat_c==1, c_treatment, 
                      ifelse(treat_c==0, c_control, NA)), 
         treat_d = ifelse(!is.na(d_treatment), 1, 0), 
         y_d = ifelse(treat_d==1, d_treatment, 
                      ifelse(treat_d==0, d_control, NA)),
         treat_e = ifelse(!is.na(e_treatment), 1, 0), 
         y_e = ifelse(treat_e==1, e_treatment, 
                      ifelse(treat_e==0, e_control, NA)), 
         y_e_placebo = ifelse(treat_e==1, e_treatment, 
                      ifelse(treat_e==0, e_placebo, NA))
         
  )

# List A - Time of the season ($\tau$ = 1/4)

get_dim_a <- function(data, filter) {
  
  m <- lm_robust(y_a ~ treat_a, data = data) %>% 
        tidy() %>% 
        slice(2) %>% 
        mutate(term = filter, 
               model = "A")
  n <- lm(y_a ~ treat_a, data = data) %>% nobs()
  
  cbind(m,n) %>% 
    mutate(term = paste(term, "\nN = ", as.character(n)))
}

# lets compare etimates using different filters
no <- get_dim_a(df, "No")
att <- get_dim_a(df %>% filter(attentive == 1) , "Attentive")
v_att <- get_dim_a(df %>% filter(very_attentive == 1) , "Very attentive")
fmc <- get_dim_a(df %>% filter(fmc_a == 1), "FMC")
att_fmc <- get_dim_a(df %>% filter(attentive == 1 & fmc_a == 1), "Attentive \nand FMC")
v_att_fmc <- get_dim_a(df %>% filter(very_attentive == 1 & fmc_a == 1), "Very attentive \nand FMC")
audit <- get_dim_a(df %>% filter(audit_pass == 1), "Audit")

a_estimates <- rbind(no,att,v_att,fmc,att_fmc,v_att, audit)

a_estimates %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = .25, colour = "grey", linetype = 2),
                   dot_args = list(aes(color = model), size = 3),
                   whisker_args = list(aes(color = model)), dodge_size = .7)
  
  

#super strange! we get worse estimates as we hone in 
# on the more attentive respondents! 


# List B - Zodiac animal ($\tau$ = 1/6)

#first with nomral control group (j=4)
get_dim_b <- function(data, filter) {
  
  m <- lm_robust(y_b ~ treat_b, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "B")
  n <- lm(y_b ~ treat_b, data = data) %>% nobs()
  
  cbind(m,n) %>% 
    mutate(term = paste(term, "\nN = ", as.character(n)))
}

# apply different filters and store estimates
no <- get_dim_b(df, "No")
att <- get_dim_b(df %>% filter(attentive == 1) , "Attentive")
v_att <- get_dim_b(df %>% filter(very_attentive == 1) , "Very attentive")
fmc <- get_dim_b(df %>% filter(fmc_b == 1), "FMC")
att_fmc <- get_dim_b(df %>% filter(attentive == 1 & fmc_b == 1), "Attentive \nand FMC")
v_att_fmc <- get_dim_b(df %>% filter(very_attentive == 1 & fmc_b == 1), "Very attentive \nand FMC")
audit <- get_dim_b(df %>% filter(audit_pass == 1), "Audit")


b_estimates <- rbind(no,att,v_att,fmc,att_fmc,v_att, audit) 

b_estimates %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = 1/6, colour = "grey", linetype = 2),
                     dot_args = list(aes(color = model), size = 3),
                     whisker_args = list(aes(color = model)), dodge_size = .7)

#this looks as expected, except the FMC



#how about inclusion of a placebo item?

# and now with placebo control group (j=5)
get_dim_b_p <- function(data, filter) {
  
  m <- lm_robust(y_b_placebo ~ treat_b, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "B DiM \nplacebo (j=5)")
  n <- lm(y_b_placebo ~ treat_b, data = data) %>% nobs()
  
  cbind(m,n) %>% 
    mutate(term = paste(term, "\nN = ", as.character(n)))
}

# apply different filters and store estimates
no <- get_dim_b_p(df, "No")
p_fail <- get_dim_b_p(df %>% filter(is.na(placebo_direct)), "Placebo fail")
att <- get_dim_b_p(df %>% filter(attentive == 1) , "Attentive")
v_att <- get_dim_b_p(df %>% filter(very_attentive == 1) , "Very attentive")
fmc <- get_dim_b_p(df %>% filter(fmc_b == 1), "FMC")
att_fmc <- get_dim_b_p(df %>% filter(attentive == 1 & fmc_b == 1), "Attentive \nand FMC")
v_att_fmc <- get_dim_b_p(df %>% filter(very_attentive == 1 & fmc_b == 1), "Very attentive \nand FMC")
audit <- get_dim_b_p(df %>% filter(audit_pass == 1), "Audit")


b_placebo_estimates <- rbind(no,p_fail,att,v_att,fmc,att_fmc,v_att, audit)

b_placebo_estimates %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = 1/6, colour = "grey", linetype = 2),
                     dot_args = list(aes(color = model), size = 3),
                     whisker_args = list(aes(color = model)), dodge_size = .7)
  
# with placebo control group we underestimate regardless of filter


#how about list E then
get_dim_e <- function(data, filter) {
  
  m <- lm_robust(y_e ~ treat_e, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "E DiM \nconventional (j=4)")
  
  p <- lm_robust(y_e_placebo ~ treat_e, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "E DiM \nplacebo (j=5)")
  
  d <-lm_robust(e_direct ~ 1, data = data) %>% 
    tidy() %>% 
    slice(1) %>% 
    mutate(term = filter, 
           model = "E Direct")
  
  rbind(m,p,d)

  }


#apply different filters and store estimates
no <- get_dim_e(df, "No")
att <- get_dim_e(df %>% filter(attentive == 1) , "Attentive")
v_att <- get_dim_e(df %>% filter(very_attentive == 1) , "Very attentive")
p_fail <- get_dim_e(df %>% filter(is.na(placebo_direct)), "Placebo fail")
fmc <- get_dim_e(df %>% filter(fmc_e == 1), "FMC")
att_fmc <- get_dim_e(df %>% filter(attentive == 1 & fmc_e == 1), "Attentive \nand FMC")
v_att_fmc <- get_dim_e(df %>% filter(very_attentive == 1 & fmc_e == 1), "Very attentive \nand FMC")
audit <- get_dim_e(df %>% filter(audit_pass == 1), "Audit")

e_estimates <-  rbind(no,att,v_att,fmc,att_fmc,audit, p_fail) 

e_estimates %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = 0, colour = "grey", linetype = 2),
                     dot_args = list(aes(color = model), size = 3),
                     whisker_args = list(aes(color = model)), dodge_size = .7)

e_estimates


#



