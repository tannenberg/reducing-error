# this script cotains code for analysing the pilot data (in the order i thought about it)
# also found in code chunks in pilot-analysis.Rmd

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(rio)
library(mapview)
library(sf)

#get data and filter out test generated data (those without an ip-number)
df <- import("data/survey-data.csv") %>% 
  filter(!is.na(IPAddress))

###
#Attentiveness checks (1 = PASS, 0 = FAIL)
###

#How many pass the imc/s?
#imc_1 (click next), imc_2_1 (chose middle option), imc_2_2 (two greater than one)

df %>% select(imc_1, imc_2_1, imc_2_2, imc_2_2_inc_somewhat) %>% 
  gather() %>% 
  group_by(key, value) %>% 
  summarise(N=n())

#how many are attentive in our sample?
df %>% group_by(attentive_imc) %>% summarise(N=n())

###
#How many pass the FMC/s?
###

df %>% select(fmc_a:fmc_e) %>% 
  gather() %>% 
  group_by(key, value) %>% 
  summarise(N=n())

###
# Survey time
###

#How much time do respondents spend on the survey? in minutes
df %>% 
  ggplot(aes(x=time_spent/60)) +
  geom_density()

# How about difference between attentive and innatentive?
# lets look at this using different attention filters

df %>% 
  ggplot(aes(x=time_spent/60, fill = as.factor(imc_2_2_inc_somewhat))) +
  geom_density(alpha= .5) +
  scale_fill_viridis_d()


# whats the correlation structure between all our attentiveness meassures?
df %>% 
  select(imc_1, imc_2_1, imc_2_2_inc_somewhat, imc_2_2, fmc_a:fmc_e) %>% 
  cor() %>% 
  corrplot::corrplot(., 
                   tl.col = "black", 
                   tl.cex = 0.75,
                   col = viridis::viridis(10))


###
# IP-duplicates?
###

#There should be a block so that same ip-number can't take survey twice,
#but lets double check that block worked:

length(unique(df$IPAddress, na.rm=TRUE))

###
# Where are ppl at?
###
map <- df %>% 
  st_as_sf(coords = c("lng", "lat")) 

st_crs(map) <- st_crs(4326) #sets projection

mapview::mapview(map)

###
# LIST DESIGN
###

### Ceeling and floor issues
#lets check that that we don't have too many MIN and MAX-values

df %>% 
select(a_control:e_treatment) %>% 
  gather() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~key)


#lets see if its better when we drop innatentives:

df %>% 
filter(imc_2_1 == 1) %>% 
select(a_control:e_treatment) %>% 
  gather() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~key)

df %>% 
  filter(imc_2_1 == 0) %>% 
  select(a_control:e_treatment) %>% 
  gather() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~key)

###
# Simple DIM:s
###

#First, can we recover the A estimand of 1/4 and B estimand of 1/6?

df %>% 
  select(a_control:b_treatment) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  mutate(A_dim = a_treatment - a_control, 
         B_dim = b_treatment - b_control, 
         B_dim_placebo = b_treatment - b_placebo) %>% 
  select(A_dim:B_dim_placebo) %>% 
  gather() %>% 
  mutate(sample = rep("all", length(.[,1]))) %>% 
  ggplot(aes(x=value, y=key, color = key)) +
  geom_point() +
  geom_vline(aes(xintercept=.25), linetype= 2) + 
  geom_vline(aes(xintercept= .166), linetype= 3) + 
  scale_color_viridis_d()
  
# and are we better at it dropping inattentives?
df %>% 
  filter(imc_2_1 == 1) %>% 
  select(a_control:b_treatment) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  mutate(A_dim = a_treatment - a_control, 
         B_dim = b_treatment - b_control, 
         B_dim_placebo = b_treatment - b_placebo) %>% 
  select(A_dim:B_dim_placebo) %>% 
  gather() %>% 
  mutate(sample = rep("only attentives", length(.[,1]))) %>% 
  ggplot(aes(x=value, y=key, color = key)) +
  geom_point() +
  geom_vline(aes(xintercept=.25), linetype= 2) + 
  geom_vline(aes(xintercept= .166), linetype= 3) + 
  scale_color_viridis_d()


# Do we get the expected direction for C, D and E?
df %>% select(a_control:e_treatment, c_direct, d_direct, e_direct) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  mutate(c_dim = c_treatment - c_control,
         d_dim = d_treatment - d_control,
         e_dim = e_treatment - c_control,
         e_dim = e_treatment - c_control,
         e_dim_placebo = e_treatment - e_placebo,
         #c_est = c_direct - c_dim, 
         #d_est = d_direct - d_dim, 
         #e_est = e_direct - e_dim, 
         #e_est_placebo = e_direct - e_dim_placebo
         )%>% 
  select(c_dim:e_dim_placebo, c_direct, d_direct, e_direct) %>% 
  gather() %>% 
  ggplot(aes(x=value, y=key, color = key)) +
  geom_point() +
  scale_color_viridis_d()
  

###
#Placebo items
###

#Did the placebo item get aprox 0 agreement? I.e we don't want 1:s here!
df %>% group_by(placebo_direct) %>% summarise(N=n())

#do we get fewer 1s when dropping inattentives? 
#we can try with different attention checks here 

df %>% filter(imc_2_1 == 1) %>% 
group_by(placebo_direct) %>% summarise(N=n())

###
# pRMSE of A and B
###

#lets write our function 
prmse <- function(estimand, treatment, control) {
  sqrt( 
    (var(treatment, na.rm=TRUE) + var(control, na.rm=TRUE))
    + (estimand - ((mean(treatment, na.rm=TRUE) - mean(control, na.rm=TRUE))^2))
  )
}

prmse_a <-  prmse(estimand = .25, df$a_treatment, df$a_control)

#don't know how I failed with the function but I cant pie into it..
df_att_a <- df %>% filter(fmc_a == 1) #lets filter on the fmc first
prmse_a_att <-  prmse(estimand = .25, df_att_a$a_treatment, df_att_a$a_control)

#lower when dropping inattentives?
prmse_a_att < prmse_a

# lets do that for B
prmse_b <- prmse(estimand = (1/6), df$b_treatment, df$b_control) 
prmse_b_placebo <- prmse(estimand = (1/6), df$b_treatment, df$b_placebo) 

#and for attentives only
df_att_b <- df %>% filter(fmc_b == 1) 
prmse_b_att <- prmse(estimand = (1/6), df_att_b$b_treatment, df_att_b$b_control) 
prmse_b_placebo_att <- prmse(estimand = (1/6), df_att_b$b_treatment, df_att_b$b_placebo) 

#lower with placebo?
prmse_b_placebo < prmse_b

#lower dropping inattentive?
prmse_b_att < prmse_b
prmse_b_placebo_att < prmse_b_placebo


###
#Audit
###

#effects on FMC A; B; and C

a <- glm(fmc_a ~ audit, data = df) %>%  broom::tidy() 
b <- glm(fmc_b ~ audit, data = df) %>%  broom::tidy()
c <- glm(fmc_e ~ audit, data = df) %>%  broom::tidy()
  

#install.pckages(dotwhisker)
rbind(a,b,c) %>% 
  filter(term!="(Intercept)") %>%
  mutate(model = c("a", "b", "c")) %>%  
  dotwhisker::dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                                dot_args = list(aes(color = model), size = 3),
                                whisker_args = list(aes(color = model)), dodge_size = .7) +
  labs(title = "Estimated effect of audit message \non passing fmc")

#how about audit on time?
lm(time_spent ~ audit, data = df) %>%  
  broom::tidy() %>%
  filter(term!="(Intercept)") %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                     dot_args = list(size = 3),
                     whisker_args = list(), dodge_size = .7) +
  labs(title = "Estimated effect of audit message \non time spent")
  
#prmse for audit/non audit
df_audit <- df %>% filter(audit == "received")
df_no_audit <- df %>% filter(audit == "control")

prmse_a_audit <-  prmse(estimand = .25, df_audit$a_treatment, df_audit$a_control)
prmse_a_no_audit <-  prmse(estimand = .25, df_no_audit$a_treatment, df_no_audit$a_control)

prmse_a_audit < prmse_a_no_audit #actually that says very little lets just look at the values for now. 

