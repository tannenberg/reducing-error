library(list)
library(tidyverse)

df <- df %>% 
  mutate(treat_c = ifelse(!is.na(c_treatment), 1, 0), 
         y_c = ifelse(treat_c==1, c_treatment, 
                      ifelse(treat_c==0, c_control, NA)),
         treat_c = ifelse(!is.na(c_treatment), 1, 0), 
         y_c = ifelse(treat_c==1, c_treatment, 
                      ifelse(treat_c==0, c_control, NA)), 
         treat_d = ifelse(!is.na(d_treatment), 1, 0), 
         y_d = ifelse(treat_d==1, d_treatment, 
                      ifelse(treat_d==0, d_control, NA)),
         treat_e = ifelse(!is.na(e_treatment), 1, 0), 
         y_e = ifelse(treat_e==1, e_treatment, 
                      ifelse(treat_e==0, e_control, NA)))


#C Trust in national government
fit.list.c <- ictreg(y_c ~ 1, data = df %>% filter(attentive==1), 
                                treat = "treat_c", J=4, method = "nls")

fit.sens.c <- glm(c_direct ~ 1,
                           data = df%>% filter(attentive==1  & c_direct_dk == 0), family = binomial("logit"))

avg.pred.bias.c <- predict(fit.list.c,
                           direct.glm = fit.sens.c, se.fit = TRUE)

avg.pred.bias.c

#D Trust in local government
fit.list.d <- ictreg(y_d ~ 1, data = df%>% filter(attentive==1), 
                     treat = "treat_d", J=4, method = "nls")

fit.sens.d <- glm(d_direct ~ 1,
                  data = df%>% filter(attentive==1 & c_direct_dk == 0), family = binomial("logit"))

avg.pred.bias.d <- predict(fit.list.d,
                           direct.glm = fit.sens.d, se.fit = TRUE)

avg.pred.bias.d

#E Witnessed corruption
fit.list.e <- ictreg(y_e ~ 1, data = df%>% filter(attentive==1 &sponsor_gov==1), 
                     treat = "treat_e", J=4, method = "lm")

fit.sens.e <- glm(e_direct ~ 1,
                  data = df%>% filter(attentive==1 &sponsor_gov==1), family = binomial("logit"))

avg.pred.bias.e.g1 <- predict(fit.list.e,
                           direct.glm = fit.sens.e, se.fit = TRUE)

#women vs men

#C Trust in national government
fit.list.c <- ictreg(y_c ~ income + age + female + education, data = df %>% filter(attentive==1 & urban_hukou ==0), 
                     treat = "treat_c", J=4, method = "nls")

fit.sens.c <- glm(c_direct ~ income + age + female + education,
                  data = df%>% filter(attentive==1  & c_direct_dk == 0 & urban_hukou ==0), family = binomial("logit"))

avg.pred.bias.c.rural <- predict(fit.list.c,
                           direct.glm = fit.sens.c, se.fit = TRUE)

avg.pred.bias.c.urban
avg.pred.bias.c.rural



avg.pred.bias.c.female 
avg.pred.bias.c.male


#how about list C then
get_est <- function(data, filter) {
  
  cl <- lm_robust(y_c ~ treat_c, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "Indirect", 
           list = "C: Trust national \ngovernment")
  
  cd <-lm_robust(c_direct ~ 1, data = data) %>% 
    tidy() %>% 
    slice(1) %>% 
    mutate(term = filter, 
           model = "Direct",
           list = "C: Trust national \ngovernment")
  
  dl <- lm_robust(y_d ~ treat_d, data = data) %>% 
    tidy() %>% 
    slice(2) %>% 
    mutate(term = filter, 
           model = "Indirect", 
           list = "D: Trust local \ngovernment")
  
  dd <-lm_robust(d_direct ~ 1, data = data) %>% 
    tidy() %>% 
    slice(1) %>% 
    mutate(term = filter, 
           model = "Direct",
           list = "D: Trust local \ngovernment")
  
rbind(cl,cd,dl,dd)
  
}


#apply different filters and store estimates
no <- get_est(df, "No")
att <- get_est(df %>% filter(attentive == 1) , "Attentive")
v_att <- get_est(df %>% filter(very_attentive == 1) , "Very attentive")

estimates <-  rbind(no,att,v_att) 

estimates %>% 
  dotwhisker::dwplot(vline = geom_vline(xintercept = 0, colour = "grey", linetype = 2),
                     dot_args = list(aes(color = model, shape=list), size = 3),
                     whisker_args = list(aes(color = model)), dodge_size = .7)



