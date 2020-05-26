# This script run all analysis and create all figures for; 
# "Non-strategic respondent error in list experiments: Are placebo items the solution?"
#


###
#Preperation
###

#load recuired libraries
library(tidyverse)
library(estimatr)


#import dataset from /data folder
df <- rio::import("psrm_replication/data/psrm-ds.csv")
names(df)


# create treatment, control and control_placebo indicator and outcome variable for the list
df <- df %>% 
  mutate(treat_b = ifelse(!is.na(b_treatment), 1, 0), # get 1 if presented treatment list
         y_b = ifelse(treat_b==1, b_treatment, 
                      ifelse(treat_b==0, b_control, NA)), # list outcome for treatment and (conventional) control 
         y_b_placebo =  ifelse(treat_b==1, b_treatment, 
                               ifelse(treat_b==0, b_placebo, NA)) # list outcome for treatment and placebo control
  )

###
# Estimate the "sensitive" item with the DiM estimator
###

#using conventional control group (j=4)
m <- lm_robust(y_b ~ treat_b, data = df) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Conventional")

n <- lm_robust(y_b ~ treat_b, data = df) %>% nobs()

conventional <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)))


#using placebo control group (j=5)
m <- lm_robust(y_b_placebo ~ treat_b, data = df) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Placebo")

n <- lm_robust(y_b_placebo ~ treat_b, data = df) %>% nobs()

placebo <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)))

# lets compare them visually 
estimates <- rbind(placebo, conventional)


estimates %>% 
  ggplot() +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, 
                      y = model, shape = model, linetype = model)) +
  geom_vline(aes(xintercept = (1/6)), linetype= 2, alpha=.5) +
  labs(y="", x="Estimated prevalence", shape="", linetype = "",
     title = "") + 
  theme(legend.position = "none") + 
  NULL
  
ggsave("psrm_replication/output/fig_1.pdf", width = 5, height = 2)

  
# how large is the bias?
estimates %>% mutate(bias = (1/6) - estimate)


# lets show what happens to the estimates w.o. innatentives
estimates$sample <- rep("Full", 2) 

#using conventional control group (j=4) with only attentives
m <- lm_robust(y_b ~ treat_b, data = df %>% filter(attentive==1)) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Conventional")

n <- lm_robust(y_b ~ treat_b, data = df %>% filter(attentive==1)) %>%
  nobs()

conventional <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)), 
         sample = "Attentive")


#using placebo control group (j=5) with only attentives
m <- lm_robust(y_b_placebo ~ treat_b, data = df %>% filter(attentive==1)) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Placebo")

n <- lm_robust(y_b_placebo ~ treat_b, data = df %>% filter(attentive==1))%>%
  nobs()

placebo <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)), 
         sample = "Attentive")

# update estimate df
estimates <- rbind(estimates, placebo, conventional)


# and now only very attentive 

#using conventional control group (j=4) with only very attentives
m <- lm_robust(y_b ~ treat_b, data = df %>% filter(very_attentive==1)) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Conventional")

n <- lm_robust(y_b ~ treat_b, data = df %>% filter(very_attentive==1)) %>%
  nobs()

conventional <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)), 
         sample = "Very attentive")


#using placebo control group (j=5) with only very attentives
m <- lm_robust(y_b_placebo ~ treat_b, data = df %>% filter(very_attentive==1)) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(model = "Placebo")

n <- lm_robust(y_b_placebo ~ treat_b, data = df %>% filter(very_attentive==1))%>%
  nobs()

placebo <- cbind(m,n) %>% 
  mutate(model = paste(model, "\nN = ", as.character(n)), 
         sample = "Very attentive")

# update estimate df again
estimates <- rbind(estimates, placebo, conventional) %>% 
  mutate(control = ifelse(outcome == "y_b", "Conventional", "Placebo"))
  
estimates$sample = factor(estimates$sample, levels=c('Full','Attentive','Very attentive'))

#lets see what that looks like
estimates %>% 
  ggplot() +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, 
                      y = control, shape = control, linetype = control)) +
  geom_vline(aes(xintercept = (1/6)), linetype= 2, alpha=.5) +
  labs(y="", x="Estimated prevalence", shape="", linetype = "",
       title = "") + 
  theme(legend.position = "none") + 
  facet_wrap(~sample)+
  NULL

ggsave("psrm_replication/output/fig_2.pdf", width = 5, height = 2)








  





