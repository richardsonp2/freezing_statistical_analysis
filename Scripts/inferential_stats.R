library(tidyverse)
library(lme4)
library(ggplot2)
library(broom)
library(ggfortify)
library(car)
library(lsr)
library(ggpubr)
library(rstatix)
library(emmeans)
# Get the n's involved for the whole thing 

#### n's LOW-------------- 
complete_ds_low_forNs <- complete_ds_low %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

# n's HIGH----------------
complete_ds_high_forNs <- complete_ds_high %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

#### Inferential analysis

#PrePost
freezing_acquisition_high_long <- freezing_acquisition_high %>% 
  pivot_longer(c("Pre", "Post"), names_to = "Pre_Post", values_to = "Percentage_freezing")

freezing_acquisition_low_long <- freezing_acquisition_low %>% 
  pivot_longer(c("Pre", "Post"), names_to = "Pre_Post", values_to = "Percentage_freezing")

prepost_lm_high <- lm(Post - Pre ~ Sex * Stress, data = freezing_acquisition_high)
summary(prepost_lm_high)
prepost_aov_high <- aov(Post - Pre ~ Sex * Stress, data = freezing_acquisition_high)
summary(prepost_aov_high)
etaSquared(prepost_aov_high)
anova_test(prepost_aov_high)


prepost_lm_low <- lm(Post - Pre ~ Sex * Stress, data = freezing_acquisition_low)
summary(prepost_lm_low)
prepost_aov_low <- aov(Post - Pre ~ Sex * Stress, data = freezing_acquisition_low)
summary(prepost_aov_low)
etaSquared(prepost_aov_low)
anova_test(prepost_aov_high)

#Low
hist(prepost_lm_low$residuals)

#High
hist(prepost_lm_high$residuals)


#### Inferential stats ----
#### HIGH Pre-post shock ----

pre_post <- freezing_acquisition_long %>% 
  filter(Timepoint == "Post")
hist(pre_post$Percentage)

model.diag.metrics <- augment(prepost_lm)
head(model.diag.metrics)
par(mfrow = c(2,2))
plot(prepost_lm)

#All  of the above suggests non normal data. 
leveneTest(Percentage ~ Sex * Stress * Timepoint, data = freezing_acquisition_long)
# Levenes test is very signficant. 
re_lm <- lm(data = freezing_acquisition, Pre ~ Sex + Stress + Sex*Stress)
summary(pre_lm)

plot(freezing_acquisition$Sex, rstandard(pre_lm))
hist(pre_lm$residuals)
hist(post_lm$residuals)


#check the residuals as a histogram
hist(prepost_lm_2$residuals)

freezing_acquisition_long <- freezing_acquisition %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")

prepost_lm_2 <- lm(data = freezing_acquisition_long, Percentage ~ Timepoint + Sex + Stress + Sex:Stress)
summary(prepost_lm_2)


# HIGH 2 minute extinction if I want to only look at 2 minute group
complete_ds_high_2min <- complete_ds_high %>% 
  select(c(1:4),recall_1) %>% 
  filter(Condition == 2)

#combined 2 and 10 miunte, looking at recall 
complete_ds_high_2min_w10min <- complete_ds_high %>% 
  select(c(1:4),recall_1)

recall_lm_high <- lm(data = complete_ds_high_2min_w10min, recall_1 ~ Sex*Stress)
summary(recall_lm_high)
anova(recall_lm_high)

#LOW 2 minute extinction if I want to only look at 2 minute group
complete_ds_low_2min <- complete_ds_low %>% 
  select(c(1:4),recall_1) %>% 
  filter(Condition == 2)

#combined 2 and 10 miunte, looking at recall 
complete_ds_low_2min_w10min <- complete_ds_low %>% 
  select(c(1:4),recall_1)

recall_lm_low <- lm(data = complete_ds_low_2min_w10min, recall_1 ~ Sex*Stress)
summary(recall_lm_low)
anova(recall_lm_low)






#### LOW Inferential 10 minute (ten_minute_extinction) ----
ten_minute_extinction_low <- complete_ds_low %>% 
  select(c(1:4),c(8:12)) %>% 
  filter(Condition ==10)

#ten_minute_ds <- list(ten_minute_extinction$ext1_curve, ten_minute_extinction$ext2_curve, ten_minute_extinction$ext3_curve, ten_minute_extinction$ext4_curve, ten_minute_extinction$ext5_curve)

ten_minute_extinction_low_long <- ten_minute_extinction_low %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()
ten_minute_extinction_low_long$timepoint <- as.factor(ten_minute_extinction_low_long$timepoint)

# MODEL 1: Sex + Stress + Sex * Stress + timepoint <- very simple model
low_ten_min_extinction_model <- lm(data = ten_minute_extinction_low_long, percentage ~ Stress + Sex + Sex * Stress * timepoint)

summary(low_ten_min_extinction_model)
Anova(low_ten_min_extinction_model)

#### HIGH Inferential 10 minute (ten_minute_extinction) ----
ten_minute_extinction_high <- complete_ds_high %>% 
  select(c(1:4),c(8:12)) %>% 
  filter(Condition ==10)

#ten_minute_ds <- list(ten_minute_extinction$ext1_curve, ten_minute_extinction$ext2_curve, ten_minute_extinction$ext3_curve, ten_minute_extinction$ext4_curve, ten_minute_extinction$ext5_curve)

ten_minute_extinction_high_long <- ten_minute_extinction_high %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()
ten_minute_extinction_high_long$timepoint <- as.factor(ten_minute_extinction_high_long$timepoint)

# MODEL 1: Sex + Stress + Sex * Stress + timepoint <- very simple model
high_ten_min_extinction_model <- lm(data = ten_minute_extinction_high_long, percentage ~ Stress + Sex + Sex * Stress * timepoint)

summary(high_ten_min_extinction_model)
Anova(high_ten_min_extinction_model)



#### Individual 10 minute splits: 
# LOW
ten_minute_extinction_low_ELS_M <- ten_minute_extinction_low %>% 
  filter(Stress == "ELS" & Sex == "Male")

ten_minute_extinction_low_long_ELS_M <- ten_minute_extinction_low_ELS_M %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

low_ten_min_extinction_model_ELS_M <- lm(data = ten_minute_extinction_low_long_ELS_M, percentage ~ timepoint)
summary(low_ten_min_extinction_model_ELS_M)
Anova(low_ten_min_extinction_model_ELS_M)

ten_minute_extinction_low_NS_M <- ten_minute_extinction_low %>% 
  filter(Stress == "NS" & Sex == "Male")

ten_minute_extinction_low_long_NS_M <- ten_minute_extinction_low_NS_M %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

low_ten_min_extinction_model_NS_M <- lm(data = ten_minute_extinction_low_long_NS_M, percentage ~ timepoint)
summary(low_ten_min_extinction_model_NS_M)
Anova(low_ten_min_extinction_model_NS_M)

ten_minute_extinction_low_ELS_F <- ten_minute_extinction_low %>% 
  filter(Stress == "ELS" & Sex == "Female")

ten_minute_extinction_low_long_ELS_F <- ten_minute_extinction_low_ELS_F %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

low_ten_min_extinction_model_ELS_F <- lm(data = ten_minute_extinction_low_long_ELS_F, percentage ~ timepoint)
summary(low_ten_min_extinction_model_ELS_F)
Anova(low_ten_min_extinction_model_ELS_F)

ten_minute_extinction_low_NS_F <- ten_minute_extinction_low %>% 
  filter(Stress == "NS" & Sex == "Female")

ten_minute_extinction_low_long_NS_F <- ten_minute_extinction_low_NS_F %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

low_ten_min_extinction_model_NS_F <- lm(data = ten_minute_extinction_low_long_NS_F, percentage ~ timepoint)
summary(low_ten_min_extinction_model_NS_F)
Anova(low_ten_min_extinction_model_NS_F)


# HIGH
ten_minute_extinction_high_ELS_M <- ten_minute_extinction_high %>% 
  filter(Stress == "ELS" & Sex == "Male")

ten_minute_extinction_high_long_ELS_M <- ten_minute_extinction_high_ELS_M %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

high_ten_min_extinction_model_ELS_M <- lm(data = ten_minute_extinction_high_long_ELS_M, percentage ~ timepoint)
summary(high_ten_min_extinction_model_ELS_M)
Anova(high_ten_min_extinction_model_ELS_M)

ten_minute_extinction_high_NS_M <- ten_minute_extinction_high %>% 
  filter(Stress == "NS" & Sex == "Male")

ten_minute_extinction_high_long_NS_M <- ten_minute_extinction_high_NS_M %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

high_ten_min_extinction_model_NS_M <- lm(data = ten_minute_extinction_high_long_NS_M, percentage ~ timepoint)
summary(high_ten_min_extinction_model_NS_M)
Anova(high_ten_min_extinction_model_NS_M)

ten_minute_extinction_high_ELS_F <- ten_minute_extinction_high %>% 
  filter(Stress == "ELS" & Sex == "Female")

ten_minute_extinction_high_long_ELS_F <- ten_minute_extinction_high_ELS_F %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

high_ten_min_extinction_model_ELS_F <- lm(data = ten_minute_extinction_high_long_ELS_F, percentage ~ timepoint)
summary(high_ten_min_extinction_model_ELS_F)
Anova(high_ten_min_extinction_model_ELS_F)

ten_minute_extinction_high_NS_F <- ten_minute_extinction_high %>% 
  filter(Stress == "NS" & Sex == "Female")

ten_minute_extinction_high_long_NS_F <- ten_minute_extinction_high_NS_F %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()

high_ten_min_extinction_model_NS_F <- lm(data = ten_minute_extinction_high_long_NS_F, percentage ~ timepoint)
summary(high_ten_min_extinction_model_NS_F)
Anova(high_ten_min_extinction_model_NS_F)

#### HIGH extinction recall (recall_1) 
recall_1_high <- complete_ds_high %>% 
  select(c(1:4),"extinction_recall")

hist(recall_1_high$extinction_recall)

#log transform
recall_1_high$extinction_recallPLusone <- recall_1_high$extinction_recall + 1
recall_1_high$extinction_recall_log <- log(recall_1_high$extinction_recallPLusone)

#sqrt transform
recall_1_high$extinction_recall_sqrt <- sqrt(recall_1_high$extinction_recall)

#box cox - doesnt work 
recall_1_high$extinction_recall_box <- boxCox(recall_1_high$extinction_recall)

hist(recall_1_high$extinction_recall)
hist(recall_1_high$extinction_recall_log)
shapiro.test(recall_1_high$extinction_recall)
shapiro.test(recall_1_high$extinction_recall_sqrt)


ext_recall_high_lm <- lm(data = recall_1_high, extinction_recall_log ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition)
summary(ext_recall_high_lm)
Anova(ext_recall_high_lm)
#try a possion dist <- not sure that this is correct
poisson_model_high_glm <- glm(extinction_recall ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition, data = recall_1_high, family = poisson(link = "log"))
summary(poisson_model_high_glm)
#individual t tests conducted assess each group 
# M ELS
m_els_ds <- recall_1 %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_ds %>%
  group_by(Condition) %>% 
  count()  

#for multiple histograms in one spot
par(mfrow = c(4,2))


m_els_data1 <- m_els_ds %>% 
  filter(Condition == 2)  
m_els_data2 <- m_els_ds %>% 
  filter(Condition == 10)

# M NS
m_ns_ds <- recall_1 %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1 <- m_ns_ds %>% 
  filter(Condition == 2)  
m_ns_data2 <- m_ns_ds %>% 
  filter(Condition == 10)

# F ELS
f_els_ds <- recall_1 %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1 <- f_els_ds %>% 
  filter(Condition == 2)  
f_els_data2 <- f_els_ds %>% 
  filter(Condition == 10)

# F NS
f_ns_ds <- recall_1 %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1 <- f_ns_ds %>% 
  filter(Condition == 2)  
f_ns_data2 <- f_ns_ds %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(extinction_recall ~ Condition, data = m_els_ds_high)
t.test(extinction_recall ~ Condition, data = m_ns_ds_high)
t.test(extinction_recall ~ Condition, data = f_els_ds_high)
t.test(extinction_recall ~ Condition, data = f_ns_ds_high)


#this isnt working yet!
#lapply(split_datasets, t.test(extinction_recall ~ Condition))


#HIGH reminder (reminder_shock) 
#individual t tests conducted assess each group 
# M ELS
m_els_ds <- reminder_shock %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_ds %>%
  group_by(Condition) %>% 
  count()  

#for multiple histograms in one spot
par(mfrow = c(4,2))


m_els_data1 <- m_els_ds %>% 
  filter(Condition == 2)  
m_els_data2 <- m_els_ds %>% 
  filter(Condition == 10)

# M NS
m_ns_ds <- reminder_shock %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1 <- m_ns_ds %>% 
  filter(Condition == 2)  
m_ns_data2 <- m_ns_ds %>% 
  filter(Condition == 10)

# F ELS
f_els_ds <- reminder_shock %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1 <- f_els_ds %>% 
  filter(Condition == 2)  
f_els_data2 <- f_els_ds %>% 
  filter(Condition == 10)

# F NS
f_ns_ds <- reminder_shock %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1 <- f_ns_ds %>% 
  filter(Condition == 2)  
f_ns_data2 <- f_ns_ds %>% 
  filter(Condition == 10)

#histogram_datasets <- list(m_els_data1$extinction_recall, m_els_data2$extinction_recall,
#                           m_ns_data1$extinction_recall, m_ns_data2$extinction_recall,
#                           f_els_data1$extinction_recall, f_els_data2$extinction_recall,
#                           f_ns_data1$extinction_recall, f_ns_data2$extinction_recall)

#lapply(histogram_datasets, hist)



# Check the t-tests 
t.test(reminder_day1_shock ~ Condition, data = m_els_ds)

t.test(reminder_day1_shock ~ Condition, data = m_ns_ds)

t.test(reminder_day1_shock ~ Condition, data = f_els_ds)

t.test(reminder_day1_shock ~ Condition, data = f_ns_ds)





#### HIGH reminder shock --------
#individual t tests conducted assess each group 
reminder_shock <- reminder_shock %>%
  drop_na() %>% 
  filter(Shock == "h")

# M ELS
m_els_ds <- reminder_shock %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_data1 <- m_els_ds %>% 
  filter(Condition == 2)  
m_els_data2 <- m_els_ds %>% 
  filter(Condition == 10)

# M NS
m_ns_ds <- reminder_shock %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1 <- m_ns_ds %>% 
  filter(Condition == 2)  
m_ns_data2 <- m_ns_ds %>% 
  filter(Condition == 10)

# F ELS
f_els_ds <- reminder_shock %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1 <- f_els_ds %>% 
  filter(Condition == 2)  
f_els_data2 <- f_els_ds %>% 
  filter(Condition == 10)

# F NS
f_ns_ds <- reminder_shock %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1 <- f_ns_ds %>% 
  filter(Condition == 2)  
f_ns_data2 <- f_ns_ds %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day1_shock ~ Condition, data = m_els_ds)
t.test(reminder_day1_shock ~ Condition, data = m_ns_ds)
t.test(reminder_day1_shock ~ Condition, data = f_els_ds)
t.test(reminder_day1_shock ~ Condition, data = f_ns_ds)




#### HIGH reminder recall (reminder_recall) ----
reminder_recall_high <- reminder_recall %>%
  drop_na() %>% 
  filter(Shock == "h")


#individual t tests conducted assess each group 
# M ELS
m_els_ds_rem_recall <- reminder_recall_high %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_ds %>%
  group_by(Condition) %>% 
  count()  

#for multiple histograms in one spot
par(mfrow = c(4,2))


m_els_2ext_rem_recall <- m_els_ds %>% 
  filter(Condition == 2)  
m_els_10ext_rem_recall <- m_els_ds %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_rem_recall <- reminder_recall_high %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_2ext_rem_recall <- m_ns_ds %>% 
  filter(Condition == 2)  
m_ns_10ext_rem_recall <- m_ns_ds %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_rem_recall <- reminder_recall_high %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_2ext_rem_recall <- f_els_ds %>% 
  filter(Condition == 2)  
f_els_10ext_rem_recall <- f_els_ds %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_rem_recall <- reminder_recall_high %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_2ext_rem_recall <- f_ns_ds %>% 
  filter(Condition == 2)  
f_ns_10ext_rem_recall <- f_ns_ds %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day2 ~ Condition, data = m_els_ds_rem_recall)
t.test(reminder_day2 ~ Condition, data = m_ns_ds_rem_recall)
t.test(reminder_day2 ~ Condition, data = f_els_ds_rem_recall)
t.test(reminder_day2 ~ Condition, data = f_ns_ds_rem_recall)


#### Low shock ----------
#### Pre-post shock ---------

freezing_acquisition_long_low <- freezing_acquisition_low %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")


prepost_lm <- lm(Percentage ~ Sex * Stress + Timepoint, data = freezing_acquisition_long_low)
summary(prepost_lm)
prepost_aov <- aov(Percentage ~ Sex * Stress + Timepoint, data = freezing_acquisition_long_low)
summary(prepost_aov)
etaSquared(prepost_aov)


pre_lm_low <- lm(data = freezing_acquisition_low, Pre ~ Sex + Stress + Sex*Stress)
summary(pre_lm_low)

plot(pre_lm_low)


post_lm_low <- lm(data = freezing_acquisition_low, Post ~ Sex + Stress + Sex*Stress)
summary(post_lm_low)


plot(freezing_acquisition_low$Sex, rstandard(pre_lm_low))
hist(pre_lm_low$residuals)
hist(post_lm_low$residuals)

freezing_acquisition_low$Pre1 <- freezing_acquisition_low$Pre + 1

pre_lm1_low <- lm(data = freezing_acquisition_low, Pre1 ~ Sex * Stress)

#combined test 
prepost_lm_low <- lm(data = freezing_acquisition_low, Post ~ Sex + Stress + Sex:Stress + Pre)
summary(prepost_lm_low)

prepost_lm_2_low <- lm(data = freezing_acquisition_long_low, Percentage ~ Timepoint + Sex + Stress + Sex:Stress)
summary(prepost_lm_2_low)

anova(prepost_lm_2_low)



##### comparison between low and high 
high_low_post_compare <- complete_ds %>% 
  select(1:6)

high_low_post_compare <- drop_na(high_low_post_compare)

# descriptives 
high_low_post_compare_descriptives <- high_low_post_compare %>% 
  group_by(Sex, Stress, Shock)%>% 
  summarize(mean_freezing_pre = mean(Pre, na.rm = T), sem_freezing_pre = sd(Pre, na.rm = T)/sqrt(length(Pre)), mean_freezing_post = mean(Post, na.rm = T), sem_freezing_post = sd(Post, na.rm = T)/sqrt(length(Post)))


#checking for post shock
high_low_post_compare_lm <- lm(data = high_low_post_compare, Post ~ Sex * Stress * Shock )
summary(high_low_post_compare_lm)

#Control - testing for pre shock
high_low_preControl_compare_lm <- lm(data = high_low_post_compare, Pre ~ Sex * Stress * Shock )
summary(high_low_preControl_compare_lm)



#### LOW recall (two_minute_extinction)----
two_minute_extinction_low_0count <- two_minute_extinction_low  %>% 
  filter(recall_1 == 0)
two_minute_extinction_low_0count %>% 
  group_by(Sex, Stress) %>% 
  count()


two_minute_extinction <- complete_ds %>% 
  select(1:4,"recall_1")

nonstress_two_minute_extinction <- two_minute_extinction %>% 
  filter(Stress == "NS")

shock_test_lm <- lm(data = nonstress_two_minute_extinction, recall_1 ~ Sex * Shock)
summary(shock_test_lm)
aov_shock_test <- aov(shock_test_lm)
summary(aov_shock_test)

recall_lm_low <- lm(data = two_minute_extinction_low, recall_1 ~ Sex + Stress + Sex:Stress)
summary(recall_lm_low)
recall_aov <- aov(recall_lm_low)

recall_lm_no_int_low <- lm(data = two_minute_extinction_low, recall_1 ~ Sex + Stress)
summary(recall_lm_no_int_low)

hist(two_minute_extinction_low$recall_1)


#### Extinction recall (recall_1) ----
  # LOW 
#Count for n's
recall_1_low_n <- recall_1_low %>% 
  filter(extinction_recall == 0) %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()


# LOW individual t tests conducted assess each group 
# M ELS
m_els_ds_low <- recall_1_low %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_ds_low %>%
  group_by(Condition) %>% 
  count()  

#for multiple histograms in one spot
par(mfrow = c(4,2))


m_els_data1_low <- m_els_ds_low %>% 
  filter(Condition == 2)  
m_els_data2_low <- m_els_ds_low %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_low <- recall_1_low %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_low <- m_ns_ds_low %>% 
  filter(Condition == 2)  
m_ns_data2_low <- m_ns_ds_low %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_low <- recall_1_low %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_low <- f_els_ds_low %>% 
  filter(Condition == 2)  
f_els_data2_low <- f_els_ds_low %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_low <- recall_1_low %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_low <- f_ns_ds_low %>% 
  filter(Condition == 2)  
f_ns_data2_low <- f_ns_ds_low %>% 
  filter(Condition == 10)

histogram_datasets <- list(m_els_data1_low$extinction_recall, m_els_data2_low$extinction_recall,
                           m_ns_data1_low$extinction_recall, m_ns_data2_low$extinction_recall,
                           f_els_data1_low$extinction_recall, f_els_data2_low$extinction_recall,
                           f_ns_data1_low$extinction_recall, f_ns_data2_low$extinction_recall)

lapply(histogram_datasets, hist)

# Check the t-tests 
t.test(extinction_recall ~ Condition, data = m_els_ds_low)
t.test(extinction_recall ~ Condition, data = m_ns_ds_low)
t.test(extinction_recall ~ Condition, data = f_els_ds_low)
t.test(extinction_recall ~ Condition, data = f_ns_ds_low)

# HIGH individual t tests conducted assess each group 
# M ELS
m_els_ds_high <- recall_1_high %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_ds_high %>%
  group_by(Condition) %>% 
  count()  

#for multiple histograms in one spot
par(mfrow = c(4,2))


m_els_data1_high <- m_els_ds_high %>% 
  filter(Condition == 2)  
m_els_data2_high <- m_els_ds_high %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_high <- recall_1_high %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_high <- m_ns_ds_high %>% 
  filter(Condition == 2)  
m_ns_data2_high <- m_ns_ds_high %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_high <- recall_1_high %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_high <- f_els_ds_high %>% 
  filter(Condition == 2)  
f_els_data2_high <- f_els_ds_high %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_high <- recall_1_high %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_high <- f_ns_ds_high %>% 
  filter(Condition == 2)  
f_ns_data2_high <- f_ns_ds_high %>% 
  filter(Condition == 10)

histogram_datasets <- list(m_els_data1_high$extinction_recall, m_els_data2_high$extinction_recall,
                           m_ns_data1_high$extinction_recall, m_ns_data2_high$extinction_recall,
                           f_els_data1_high$extinction_recall, f_els_data2_high$extinction_recall,
                           f_ns_data1_high$extinction_recall, f_ns_data2_high$extinction_recall)

lapply(histogram_datasets, hist)

# Check the t-tests 
t.test(extinction_recall ~ Condition, data = m_els_ds_high)
t.test(extinction_recall ~ Condition, data = m_ns_ds_high)
t.test(extinction_recall ~ Condition, data = f_els_ds_high)
t.test(extinction_recall ~ Condition, data = f_ns_ds_high)




##### Low reminder shock ------------------


reminder_shock_low <- reminder_shock %>%
  drop_na() %>% 
  filter(Shock == "l")

#early exploratory anova

reminder_shock_lm <- lm(data = reminder_shock_low, reminder_day1_shock ~ Sex * Stress * Condition)
summary(reminder_shock_lm)
aov_reminder_shock <- aov(reminder_shock_lm)
summary(aov_reminder_shock)

#individual t tests conducted assess each group 
# M ELS
m_els_ds_rem_shock_low <- reminder_shock_low %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_data1_low <- reminder_shock_low %>% 
  filter(Condition == 2)  
m_els_data2_low <- reminder_shock_low %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_rem_shock_low <- reminder_shock_low %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_low <- reminder_shock_low %>% 
  filter(Condition == 2)  
m_ns_data2_low <- reminder_shock_low %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_rem_shock_low <- reminder_shock_low %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_low <- reminder_shock_low %>% 
  filter(Condition == 2)  
f_els_data2_low <- reminder_shock_low %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_rem_shock_low <- reminder_shock_low %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_low <- reminder_shock_low %>% 
  filter(Condition == 2)  
f_ns_data2_low <- reminder_shock_low %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day1_shock ~ Condition, data = m_els_ds_rem_shock_low)
t.test(reminder_day1_shock ~ Condition, data = m_ns_ds_rem_shock_low)
t.test(reminder_day1_shock ~ Condition, data = f_els_ds_rem_shock_low)
t.test(reminder_day1_shock ~ Condition, data = f_ns_ds_rem_shock_low)

##### High reminder shock ------------------

reminder_shock_high <- reminder_shock %>%
  drop_na() %>% 
  filter(Shock == "h")

#early exploratory anova
reminder_shock_lm <- lm(data = reminder_shock_high, reminder_day1_shock ~ Sex * Stress * Condition)
summary(reminder_shock_lm)
aov_reminder_shock <- aov(reminder_shock_lm)
summary(aov_reminder_shock)

#individual t tests conducted assess each group 
# M ELS
m_els_ds_rem_shock_high <- reminder_shock_high %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_data1_high <- reminder_shock_high %>% 
  filter(Condition == 2)  
m_els_data2_high <- reminder_shock_high %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_rem_shock_high <- reminder_shock_high %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_high <- reminder_shock_high %>% 
  filter(Condition == 2)  
m_ns_data2_high <- reminder_shock_high %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_rem_shock_high <- reminder_shock_high %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_high <- reminder_shock_high %>% 
  filter(Condition == 2)  
f_els_data2_high <- reminder_shock_high %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_rem_shock_high <- reminder_shock_high %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_high <- reminder_shock_high %>% 
  filter(Condition == 2)  
f_ns_data2_high <- reminder_shock_high %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day1_shock ~ Condition, data = m_els_ds_rem_shock_high)
t.test(reminder_day1_shock ~ Condition, data = m_ns_ds_rem_shock_high)
t.test(reminder_day1_shock ~ Condition, data = f_els_ds_rem_shock_high)
t.test(reminder_day1_shock ~ Condition, data = f_ns_ds_rem_shock_high)


#### Low reminder recall -------------------

reminder_recall_low <- reminder_recall %>%
  drop_na() %>% 
  filter(Shock == "l")

#early exploratory anova
reminder_recall_lm <- lm(data = reminder_recall_low, reminder_day2 ~ Sex * Stress * Condition)
summary(reminder_recall_lm)
aov_reminder_recall <- aov(reminder_recall_lm)
summary(aov_reminder_recall)

posthocPairwiseT(aov_reminder_recall)

#individual t tests conducted assess each group 
# M ELS
m_els_ds_rem_recall_low <- reminder_recall_low %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_data1_low <- reminder_recall_low %>% 
  filter(Condition == 2)  
m_els_data2_low <- reminder_recall_low %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_rem_recall_low <- reminder_recall_low %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_low <- reminder_recall_low %>% 
  filter(Condition == 2)  
m_ns_data2_low <- reminder_recall_low %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_rem_recall_low <- reminder_recall_low %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_low <- reminder_recall_low %>% 
  filter(Condition == 2)  
f_els_data2_low <- reminder_recall_low %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_rem_recall_low <- reminder_recall_low %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_low <- reminder_recall_low %>% 
  filter(Condition == 2)  
f_ns_data2_low <- reminder_recall_low %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day2 ~ Condition, data = m_els_ds_rem_recall_low)
t.test(reminder_day2 ~ Condition, data = m_ns_ds_rem_recall_low)
t.test(reminder_day2 ~ Condition, data = f_els_ds_rem_recall_low)
t.test(reminder_day2 ~ Condition, data = f_ns_ds_rem_recall_low)


#### high reminder recall -------------------

reminder_recall_high <- reminder_recall %>%
  drop_na() %>% 
  filter(Shock == "h")

#early exploratory anova
reminder_recall_lm <- lm(data = reminder_recall_high, reminder_day2 ~ Sex * Stress * Condition)
summary(reminder_recall_lm)
aov_reminder_recall <- aov(reminder_recall_lm)
summary(aov_reminder_recall)

#removeNA values
reminder_recall_high <- na.omit(reminder_recall_high)

means_to_check_recall <- reminder_recall_high %>% 
  group_by(Stress) %>% 
  summarise(mean = mean(reminder_day2))
#individual t tests conducted assess each group 
# M ELS
m_els_ds_rem_recall_high <- reminder_recall_high %>% 
  filter(Sex == "Male" & Stress == "ELS")

m_els_data1_high <- reminder_recall_high %>% 
  filter(Condition == 2)  
m_els_data2_high <- reminder_recall_high %>% 
  filter(Condition == 10)

# M NS
m_ns_ds_rem_recall_high <- reminder_recall_high %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1_high <- reminder_recall_high %>% 
  filter(Condition == 2)  
m_ns_data2_high <- reminder_recall_high %>% 
  filter(Condition == 10)

# F ELS
f_els_ds_rem_recall_high <- reminder_recall_high %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1_high <- reminder_recall_high %>% 
  filter(Condition == 2)  
f_els_data2_high <- reminder_recall_high %>% 
  filter(Condition == 10)

# F NS
f_ns_ds_rem_recall_high <- reminder_recall_high %>% 
  filter(Sex == "Female" & Stress == "NS")

f_ns_data1_high <- reminder_recall_high %>% 
  filter(Condition == 2)  
f_ns_data2_high <- reminder_recall_high %>% 
  filter(Condition == 10)

# Check the t-tests 
t.test(reminder_day2 ~ Condition, data = m_els_ds_rem_recall_high)
t.test(reminder_day2 ~ Condition, data = m_ns_ds_rem_recall_high)
t.test(reminder_day2 ~ Condition, data = f_els_ds_rem_recall_high)
t.test(reminder_day2 ~ Condition, data = f_ns_ds_rem_recall_high)


#####Repeated measures reminder data ###########################################
####Low
#combine the datasets for the model 
recall_1_low$index <- seq_len(nrow(recall_1_low))
reminder_shock_low$index <- seq_len(nrow(reminder_shock_low)) 
reminder_shock_low_rmIVS <- reminder_shock_low %>% 
  select(-c(Sex,Stress, Condition))

reminder_recall_low$index <- seq_len(nrow(reminder_recall_low))
reminder_recall_low_rmIVS <- reminder_recall_low %>% 
  select(-c(Sex,Stress, Condition))

#wide format
combined_timepoints_low <- merge(merge(recall_1_low, reminder_shock_low_rmIVS, by = "index"), reminder_recall_low_rmIVS, by = "index") %>% 
  select(-timepoint)

#long format
combined_timepoints_long_low <- combined_timepoints_low %>% 
  pivot_longer(cols = c(extinction_recall,reminder_day1_shock, reminder_day2), names_to = "timepoint", values_to = "freezing_percentage")


#think for PWC need to remove the NA values. 
combined_timepoints_low_rem_na <- combined_timepoints_low[complete.cases(combined_timepoints_low), ]
combined_timepoints_long_low_rem_na <- combined_timepoints_low_rem_na %>% 
  pivot_longer(cols = c(extinction_recall,reminder_day1_shock, reminder_day2), names_to = "timepoint", values_to = "freezing_percentage")

combined_timepoints_long_low_rem_na$timepoint <- as.factor(combined_timepoints_long_low_rem_na$timepoint)

reminder_rm_low <- anova_test(data = combined_timepoints_long_low, dv = freezing_percentage, wid = index, within = timepoint, between = c("Sex", "Stress", "Condition"))
get_anova_table(reminder_rm)

# pairwise comparisons
pwc_reminder_low <- combined_timepoints_long_low_rem_na %>%
  pairwise_t_test(
    freezing_percentage ~ Sex, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc_reminder_low
#### High 

#combine the datasets for the model 
recall_1_high$index <- seq_len(nrow(recall_1_high))
recall_1_high <- recall_1_high %>% 
  select(-c(Shock, extinction_recallPLusone, extinction_recall_log, extinction_recall_sqrt))
reminder_shock_high$index <- seq_len(nrow(reminder_shock_high)) 
reminder_shock_high_rmIVS <- reminder_shock_high %>% 
  select(-c(Sex,Stress, Condition, Shock))

reminder_recall_high$index <- seq_len(nrow(reminder_recall_high))
reminder_recall_high_rmIVS <- reminder_recall_high %>% 
  select(-c(Sex,Stress, Condition, Shock))

#wide format
combined_timepoints_high <- merge(merge(recall_1_high, reminder_shock_high_rmIVS, by = "index"), reminder_recall_high_rmIVS, by = "index")

#long format
combined_timepoints_long_high <- combined_timepoints_high %>% 
  pivot_longer(cols = c(extinction_recall,reminder_day1_shock, reminder_day2), names_to = "timepoint", values_to = "freezing_percentage")


#think for PWC need to remove the NA values. 
combined_timepoints_high_rem_na <- combined_timepoints_high[complete.cases(combined_timepoints_high), ]
combined_timepoints_long_high_rem_na <- combined_timepoints_high_rem_na %>% 
  pivot_longer(cols = c(extinction_recall,reminder_day1_shock, reminder_day2), names_to = "timepoint", values_to = "freezing_percentage")

combined_timepoints_long_high_rem_na$timepoint <- as.factor(combined_timepoints_long_high_rem_na$timepoint)

reminder_rm_high <- anova_test(data = combined_timepoints_long_high, dv = freezing_percentage, wid = index, within = timepoint, between = c("Sex", "Stress", "Condition"))
get_anova_table(reminder_rm)

# pairwise comparisons
pwc_reminder_high <- combined_timepoints_long_high_rem_na %>%
  pairwise_t_test(
    freezing_percentage ~ Sex, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc_reminder_high


###### Extinction reminder_shock reminder recall figures #######################

#### Low#################

#split the 2 and 10 group 
combined_timepoints_long_low_rem_na_2 <- combined_timepoints_long_low_rem_na %>% 
  filter(Condition == 2)
combined_timepoints_long_low_2_rem_na <- combined_timepoints_long_low_rem_na_2 %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE)
combined_timepoints_long_low_2_rem_na_desc <- combined_timepoints_long_low_rem_na_2 %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  group_by(sex_stress, timepoint) %>% 
  summarize(mean_freezing = mean(freezing_percentage), sem_freezing = sd(freezing_percentage) / sqrt(n()))



combined_timepoints_long_low_rem_na_10 <- combined_timepoints_long_low_rem_na %>% 
  filter(Condition == 10)
#figures for the above statistics
combined_timepoints_long_low_10_rem_na <- combined_timepoints_long_low_rem_na_10 %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE)
combined_timepoints_long_low_10_rem_na_desc <- combined_timepoints_long_low_rem_na_10 %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  group_by(sex_stress, timepoint) %>% 
  summarize(mean_freezing = mean(freezing_percentage), sem_freezing = sd(freezing_percentage) / sqrt(n()))


  
# extinction curve figure #### figure out how to make the lines dashed or solid
# Define line types for each factor level
#line_types <- c("dashed", "solid", "dashed", "solid")

reminder_figure_low_rem_na_2 <- ggplot(data = combined_timepoints_long_low_2_rem_na_desc, aes(x = timepoint, y = mean_freezing, group = sex_stress, color = sex_stress)) + 
  geom_line(size = 2)+
  geom_line(data = combined_timepoints_long_low_2_rem_na, aes(x = timepoint, y = freezing_percentage, group = index, color = sex_stress), alpha = 0.3)+
  geom_errorbar(aes(ymin = mean_freezing - sem_freezing, ymax = mean_freezing + sem_freezing), width = 0.2)+
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0,0))

reminder_figure_low_rem_na_2 <- reminder_figure_low_rem_na_2 + scale_x_discrete(labels = c("extinction_recall" = "Extinction recall", "reminder_day1_shock"= "Reminder session", "reminder_day2" = "Reminder recall"))
reminder_figure_low_rem_na_2 <- reminder_figure_low_rem_na_2 + scale_color_manual(labels = c("Female_ELS" = "Female ELS", "Female_NS" = "Female non-stressed", "Male_ELS" = "Male ELS", "Male_NS" = "Male Non-stressed"),values=c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))
reminder_figure_low_rem_na_2 <- reminder_figure_low_rem_na_2 + labs(x = "Timepoint", y = "Freezing percentage", group = "Group", color = "Group")
reminder_figure_low_rem_na_2 <- reminder_figure_low_rem_na_2 + blank_figure_theme
reminder_figure_low_rem_na_2

reminder_figure_low_rem_na_10 <- ggplot(data = combined_timepoints_long_low_10_rem_na_desc, aes(x = timepoint, y = mean_freezing, group = sex_stress, color = sex_stress)) + 
  geom_line(size = 2)+
  geom_line(data = combined_timepoints_long_low_10_rem_na, aes(x = timepoint, y = freezing_percentage, group = index, color = sex_stress), alpha = 0.3)+
  geom_errorbar(aes(ymin = mean_freezing - sem_freezing, ymax = mean_freezing + sem_freezing), width = 0.2)+
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0,0))

reminder_figure_low_rem_na_10 <- reminder_figure_low_rem_na_10 + scale_x_discrete(labels = c("extinction_recall" = "Extinction recall", "reminder_day1_shock"= "Reminder session", "reminder_day2" = "Reminder recall"))
reminder_figure_low_rem_na_10 <- reminder_figure_low_rem_na_10 + scale_color_manual(labels = c("Female_ELS" = "Female ELS", "Female_NS" = "Female non-stressed", "Male_ELS" = "Male ELS", "Male_NS" = "Male Non-stressed"),values=c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))
reminder_figure_low_rem_na_10 <- reminder_figure_low_rem_na_10 + labs(x = "Timepoint", y = "Freezing percentage", group = "Group", color = "Group")
reminder_figure_low_rem_na_10 <- reminder_figure_low_rem_na_10 + blank_figure_theme
reminder_figure_low_rem_na_10

reminder_rm_combined_figure_low <- ggarrange(reminder_figure_low_rem_na_2, reminder_figure_low_rem_na_10 + rremove("ylab"), 
                                          labels = c("2 minute", "10 minute"),
                                          ncol = 2, nrow = 1)

reminder_rm_combined_figure_low + scale_y_continuous(expand = c(0, 0), limits = c(0, 80))
reminder_rm_combined_figure_low

# ANALYSIS FOR KERRIES EMAIL 25/08/23 ########
complete_ds_2_low
complete_ds_2_high
complete_ds_10_low
complete_ds_10_high

# dataset_selected <- complete_ds_2_low %>% 
#   select("Sex", "Stress", "recall_1","extinction_recall", "reminder_day1_shock", "reminder_day2")
# 
# factor_cols <- c("Sex", "Stress")
# dataset_selected[factor_cols] <- lapply(dataset_selected[factor_cols], factor)
# dataset_longformat <- dataset_selected %>% 
#   pivot_longer(cols = c("recall_1","extinction_recall", "reminder_day1_shock", "reminder_day2"), names_to = "timepoint", values_to = "freezing_percentage")
# 
# #add the ID column
# dataset_longformat$id <- rep(1:nrow(complete_ds_2_low), each = 4)
# aov_rm_model <- anova_test(data = dataset_longformat,dv = freezing_percentage, wid = id, within = timepoint, between = c(Sex, Stress))
# aov_rm_model

make_longitudinal <- function(dataset, isConditionsplit = FALSE, isRecallOnly = FALSE){
  #factor_cols <- c("Sex", "Stress")
  #dataset_selected[factor_cols] <- lapply(dataset_selected[factor_cols], factor)
  
  if (isRecallOnly == TRUE) {
    dataset_selected <- dataset %>% 
      select("Sex", "Stress", "recall_1","extinction_recall")
    dataset_longformat <- dataset_selected %>% 
      pivot_longer(cols = c("recall_1","extinction_recall"), names_to = "timepoint", values_to = "freezing_percentage")
    dataset_longformat$id <- rep(1:nrow(dataset), each = 2)
    
  }
  else if (isRecallOnly == FALSE){
    dataset_selected <- dataset %>% 
      select("Sex", "Stress", "recall_1","extinction_recall", "reminder_day1_shock", "reminder_day2")
    dataset_longformat <- dataset_selected %>% 
      pivot_longer(cols = c("recall_1","extinction_recall", "reminder_day1_shock", "reminder_day2"), names_to = "timepoint", values_to = "freezing_percentage")
    dataset_longformat$id <- rep(1:nrow(dataset), each = 4)
  }
  return(dataset_longformat)
}



analyse_freezing_longitudinal <- function(dataset, isConditionsplit = FALSE, isRecallOnly = FALSE){
  
  long_dataset <- make_longitudinal(dataset)
  #aov_rm_model <- anova_test(data = dataset_longformat,dv = freezing_percentage, wid = id, within = timepoint, between = c(Sex, Stress))
  aov_rm_model <- aov(freezing_percentage ~ timepoint*Sex*Stress + Error(id/(timepoint*Sex*Stress)), data = long_dataset)
  return(aov_rm_model)
}

complete_ds_low_combined_recall_remshock_remrecall <- complete_ds_low %>% 
  select(c("Stress","Sex", "Condition", "recall_1", "extinction_recall", "reminder_day1_shock", "reminder_day2")) 
complete_ds_low_combined_recall_remshock_remrecall_noNA <- complete_ds_low_combined_recall_remshock_remrecall[complete.cases(complete_ds_low_combined_recall_remshock_remrecall), ]


complete_ds_high_combined_recall_remshock_remrecall <- complete_ds_high %>% 
  select(c("Stress","Sex", "Condition", "recall_1", "extinction_recall", "reminder_day1_shock", "reminder_day2")) 
complete_ds_high_combined_recall_remshock_remrecall_noNA <- complete_ds_high_combined_recall_remshock_remrecall[complete.cases(complete_ds_high_combined_recall_remshock_remrecall), ]

#Generate the 2 and 10 datasets LOW
complete_ds_low_combined_recall_remshock_remrecall_2 <- complete_ds_low_combined_recall_remshock_remrecall %>% 
  filter(Condition == 2)
complete_ds_low_combined_recall_remshock_remrecall_10 <- complete_ds_low_combined_recall_remshock_remrecall %>% 
  filter(Condition == 10)

#Generate the 2 and 10 datasets HIGH
complete_ds_high_combined_recall_remshock_remrecall_2 <- complete_ds_high_combined_recall_remshock_remrecall %>% 
  filter(Condition == 2)
complete_ds_high_combined_recall_remshock_remrecall_10 <- complete_ds_high_combined_recall_remshock_remrecall %>% 
  filter(Condition == 10)


# Run the anovas again LOW
low_aov_2 <- analyse_freezing_longitudinal(complete_ds_low_combined_recall_remshock_remrecall_2, isConditionsplit = TRUE)
low_aov_2
get_anova_table(low_aov_2)

low_aov_10 <- analyse_freezing_longitudinal(complete_ds_low_combined_recall_remshock_remrecall_10, isConditionsplit = TRUE)
low_aov_10
get_anova_table(low_aov_10)
# Here there is a stress by timepoint interaction 
#data = dataset_longformat,dv = freezing_percentage, wid = id, within = timepoint, between = c(Sex, Stress)
#lme(freezing_percentage ~ timepoint, random = ~1|id/timepoint, data=Mice)

    
    # Run the anovas again HIGH
high_aov_2 <- analyse_freezing_longitudinal(complete_ds_high_combined_recall_remshock_remrecall_2, isConditionsplit = TRUE)
high_aov_2
get_anova_table(high_aov_2)

high_aov_10 <- analyse_freezing_longitudinal(complete_ds_high_combined_recall_remshock_remrecall_10, isConditionsplit = TRUE)
high_aov_10
get_anova_table(high_aov_10)



#TRY ANOVAS WHERE ONLY LOOKING AT THE FIRST TWO TIMEPOINTS IE RECALL AND EXT RECALL



high_aov_2_recallonly <- analyse_freezing_longitudinal(complete_ds_high_combined_recall_remshock_remrecall_2, isConditionsplit = TRUE, isRecallOnly = TRUE)
high_aov_2_recallonly
summary(high_aov_2_recallonly)

high_aov_10_recallonly <- analyse_freezing_longitudinal(complete_ds_high_combined_recall_remshock_remrecall_10, isConditionsplit = TRUE, isRecallOnly = TRUE)
high_aov_10_recallonly
summary(high_aov_10_recallonly)






# Assuming your original dataset is named `dataset_longformat`
M_ELS_data <- make_longitudinal(subset(complete_ds_high_combined_recall_remshock_remrecall_10, Sex == "Male" & Stress == "ELS"))
M_NS_data <- make_longitudinal(subset(complete_ds_high_combined_recall_remshock_remrecall_10, Sex == "Male" & Stress == "NS"))
F_ELS_data <- make_longitudinal(subset(complete_ds_high_combined_recall_remshock_remrecall_10, Sex == "Female" & Stress == "ELS"))
F_NS_data <- make_longitudinal(subset(complete_ds_high_combined_recall_remshock_remrecall_10, Sex == "Female" & Stress == "NS"))

M_ELS_aov <- aov(freezing_percentage ~ timepoint + Error(id/(timepoint)), data = M_ELS_data)
summary(M_ELS_aov)
M_NS_aov <-  aov(freezing_percentage ~ timepoint + Error(id/(timepoint)), data = M_NS_data)
summary(M_NS_aov)
F_ELS_aov <- aov(freezing_percentage ~ timepoint + Error(id/(timepoint)), data = F_ELS_data)
summary(F_ELS_aov)
F_NS_aov <-  aov(freezing_percentage ~ timepoint + Error(id/(timepoint)), data = F_NS_data)
summary(F_NS_aov)
