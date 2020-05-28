# This script run all analysis and create all figures for; 
# "Non-strategic respondent error in list experiments: Are placebo items the solution?"

###
#Preperation
###

#load recuired libraries
library(tidyverse)
library(estimatr)
library(stargazer)


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



###
# Appendix
###

#sumstats

stargazer(df, digits = 1, out = "psrm_replication/output/sum_stats.tex")

# freqency distributions of responses for each group

treatment <- df %>% 
  group_by(b_treatment) %>% 
  summarise(N=n()) %>% 
  slice(1:6) %>% 
  mutate(prop = round(N/sum(N), digits = 3),
         treatment = paste(N, " (", prop, ")", sep="")) %>% 
  select(treatment)
         
  
control <- df %>% 
  group_by(b_control) %>% 
  summarise(N=n()) %>% 
  slice(1:6) %>% 
  mutate(prop = round(N/sum(1648), digits = 3),
         control = paste(N, " (", prop, ")", sep="")) %>% 
  select(control)

placebo <- df %>% 
  group_by(b_placebo) %>% 
  summarise(N=n()) %>% 
  slice(1:6) %>% 
  mutate(prop = round(N/sum(N), digits = 3),
         placebo = paste(N, " (", prop, ")", sep="")) %>% 
  select(placebo)

cbind(treatment, control, placebo) %>% 
  mutate(items = 0:5) %>% 
  select(items, treatment, control, placebo) %>% 
  stargazer(summary = FALSE, out = "psrm_replication/output/freq_dist.tex")

# results table


