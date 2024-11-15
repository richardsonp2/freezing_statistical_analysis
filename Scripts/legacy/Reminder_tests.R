#this isnt working yet!
#lapply(split_datasets, t.test(extinction_recall ~ Condition))

#### BELOW TO MAYBE REMOVE _------------------------------
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

# After discussion with my supervisor Kerrie Thomas 25/08/23, it was agreed to present longitudinal figures. 
# Functions below are to generate the inferential statistics to examine results shown in these figures.

# complete_ds_2_low
# complete_ds_2_high
# complete_ds_10_low
# complete_ds_10_high

#' Transform dataset into long format for longitudinal analysis of freezing behavior
#'
#' This function reshapes a dataset into long format for analyzing freezing behavior
#' across different time points. It allows for optional selection of only recall-related
#' variables or all variables, including shock reminders.
#'
#' @param dataset A data frame containing the variables `Sex`, `Stress`, `recall_1`, 
#'   `extinction_recall`, `reminder_day1_shock`, and `reminder_day2`.
#' @param isConditionsplit Logical; if `TRUE`, allows splitting by additional conditions (not used in current code).
#' @param isRecallOnly Logical; if `TRUE`, includes only recall-related columns (`recall_1` and `extinction_recall`) 
#'   for transformation. If `FALSE`, includes all available timepoints (`recall_1`, `extinction_recall`, 
#'   `reminder_day1_shock`, `reminder_day2`).
#'
#' @return A data frame in long format with columns:
#'   - `Sex`: Factor for sex of the subject.
#'   - `Stress`: Factor for stress condition of the subject.
#'   - `timepoint`: Name of the timepoint variable (e.g., "recall_1", "extinction_recall").
#'   - `freezing_percentage`: Freezing percentage value for each timepoint.
#'   - `id`: Unique identifier for each subject across timepoints.
#'
#' @examples
#' # Reshape dataset including only recall-related columns
#' make_longitudinal(data, isRecallOnly = TRUE)
#'
#' # Reshape dataset including all columns
#' make_longitudinal(data, isRecallOnly = FALSE)
#'
#' @export
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

#inferentials from other script

# Check 2 minute group from LTM1 bars vs 10 minute group: first 2 minutes.
fem_non_stress_2 <- two_min_group_only %>%
  filter(Stress == "NS", Sex == "Female") %>%
  select(Stress, Sex, Condition, recall_1)

fem_non_stress10_2min <- ten_minute_extinction %>%
  filter(Stress == "NS", Sex == "Female") %>%
  select(Stress, Sex, Condition, ext1_curve) %>%
  rename(recall_1 = ext1_curve)



#Combine the datasets
combined210_test <- bind_rows(fem_non_stress_2, fem_non_stress10_2min)
t_result <- t.test(recall_1 ~ Condition, data = combined210_test)
t_result
