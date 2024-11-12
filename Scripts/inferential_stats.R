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
library(lmerTest)


# Begin by loading the dataset 
file_path <- "./Datasets/high_low_combined.csv"

# I will use these throughout the script to select by different columns
factor_cols <- c("Shock", "Stress", "Sex", "Condition")
num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", 
              "ext3_curve", "ext4_curve", "ext5_curve", 
              "extinction_recall", "reminder_day1_shock", "reminder_day2")


# Function to read and clean the dataset
clean_dataset <- function(file_path) {
  ds <- read.csv(file_path, na.strings = c(".", "#DIV/0!"))
  ds[ds == "#DIV/0!"] <- NA
  
  # Fix space issue in the 'Stress' variable
  ds$Stress[ds$Stress == " NS"] <- "NS"
  
  # Remove entries with 'x' in 'Sex'
  ds <- ds %>%
    filter(Sex != "x")
  
  return(ds)
}

# Function to convert columns to appropriate data types
convert_columns <- function(ds) {
  factor_cols <- c("Shock", "Stress", "Sex", "Condition")
  num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", 
                "ext3_curve", "ext4_curve", "ext5_curve", 
                "extinction_recall", "reminder_day1_shock", "reminder_day2")
  
  ds[factor_cols] <- lapply(ds[factor_cols], function(x) as.factor(x))
  ds[num_cols] <- lapply(ds[num_cols], as.numeric)
  
  return(ds)
}
# Function to clean empty strings in factor columns
clean_factors <- function(ds) {
  factor_cols <- c("Shock", "Stress", "Sex", "Condition")
  
  for (col in factor_cols) {
    ds[[col]][ds[[col]] == ""] <- NA
    ds[[col]] <- droplevels(ds[[col]])
  }
  
  # Reverse the factor levels for 'Condition'
  ds$Condition <- fct_rev(ds$Condition)
  
  
  
  return(ds)
}

# Pre-processing script execution

complete_ds <- clean_dataset(file_path)
complete_ds <- convert_columns(complete_ds)
complete_ds <- clean_factors(complete_ds)
str(complete_ds)

# I have inputted Male, M, Female and F for sex accidentally during the freezing analysis
# Fix that here, bit of a complex workaround but I was having issues with dplyr 
complete_ds$Sex <- as.character(complete_ds$Sex)
complete_ds$Sex[complete_ds$Sex == "M"] <- "Male"
complete_ds$Sex[complete_ds$Sex == "F"] <- "Female"

# Optionally, can convert it back to a factor if needed:
complete_ds$Sex <- as.factor(complete_ds$Sex)
# Display the structure of the cleaned dataset
str(complete_ds)

#' Subset for shock intensity 
#'
#' @param dataset A data frame containing the data to be subset.
#' @param intensity A character string specifying the shock intensity. Valid options are:
#'   * "Low" - Low shock intensity 0.5mA
#'   * "High" - High shock intensity 0.7mA
#' @return The subsetted shock intensity data frame. 
#' @export
subset_by_shock_intensity <- function(dataset, intensity){
  
  if (intensity == "Low"){
    subset <- dataset %>% 
      filter (Shock == "l")
  }
  else if (intensity =="High"){
    subset <- dataset %>% 
      filter (Shock == "h")
  }
  return (subset)
}

complete_ds_low <- subset_by_shock_intensity(complete_ds, "Low")
complete_ds_high <- subset_by_shock_intensity(complete_ds, "High")

#' Generate a subset of the dataset for a specified timepoint
#'
#' This function creates a subset of the input dataset, selecting relevant columns based on the specified timepoint.
#' The function includes timepoint-specific columns and retains the specified factor columns across all timepoints.
#'
#' @param dataset A data frame containing the dataset to be subset.
#' @param timepoint A character string specifying the timepoint for subsetting the dataset. 
#'   Valid options are:
#'   * "acquisition" - selects columns "Pre", "Post", and the factor columns.
#'   * "recall_combined" - selects "recall_1" and the factor columns.
#'   * "recall_2only" - selects "recall_1" and the factor columns, filtering rows with Condition == 2.
#'   * "extinction" - selects extinction-related columns and the factor columns.
#'   * "extinction_recall" - selects "extinction_recall" and the factor columns.
#'   * "reminder_shock" - selects "reminder_day1_shock" and the factor columns.
#'   * "reminder_recall" - selects "reminder_day2" and the factor columns.
#' @return A data frame subset according to the specified timepoint, including the relevant columns.
#' @details The factor columns retained for each timepoint include "Shock", "Stress", "Sex", and "Condition".
#'   For the "extinction" timepoint, additional columns representing extinction curves are also included.
#' @examples
#' # Subset dataset for acquisition timepoint
#' acquisition_subset <- select_dataset_timepoint(my_data, "acquisition")
#' 
#' # Subset dataset for recall_combined timepoint
#' recall_combined_subset <- select_dataset_timepoint(my_data, "recall_combined")
#' 
#' # Subset dataset for recall_2only timepoint, with Condition == 2
#' recall_2only_subset <- select_dataset_timepoint(my_data, "recall_2only")
#' @export
select_dataset_timepoint <- function(dataset, timepoint) {
  valid_choices <- c("acquisition", "recall_combined", "recall_2only", "extinction", "extinction_recall", "reminder_shock", "reminder_recall")
  
  # Check if the timepoint is one of the valid choices
  if (!(timepoint %in% valid_choices)) {
    # Raise an exception with a custom error message
    stop(sprintf("Invalid input: '%s'. Valid options are: %s", 
                 timepoint, paste(valid_choices, collapse = ", ")))
  }
  
  # Factor columns will be retained no matter which timepoint is selected
  factor_cols <- c("Shock", "Stress", "Sex", "Condition")
  extinction_cols <- c("ext1_curve", "ext2_curve", "ext3_curve", "ext4_curve", "ext5_curve")
  
  subset_dataset <- switch(
    timepoint,
    "acquisition" = dataset %>% select(Pre, Post, all_of(factor_cols)),
    "recall_combined" = dataset %>% select(recall_1, all_of(factor_cols)),
    "recall_2only" = dataset %>% select(recall_1, all_of(factor_cols)) %>% filter(Condition == 2),
    "extinction" = dataset %>% select(all_of(extinction_cols), all_of(factor_cols)),
    "extinction_recall" = dataset %>% select(extinction_recall, all_of(factor_cols)),
    "reminder_shock" = dataset %>% select(reminder_day1_shock, all_of(factor_cols)),
    "reminder_recall" = dataset %>% select(reminder_day2, all_of(factor_cols))
  )
  
  # Return the subset dataset
  return(subset_dataset)
}

# Generate datasets for use below in generating figures and statistics

# For "low" category datasets
low_acquisition <- select_dataset_timepoint(complete_ds_low, "acquisition")
low_recall_combined <- select_dataset_timepoint(complete_ds_low, "recall_combined")
low_recall_2only <- select_dataset_timepoint(complete_ds_low, "recall_2only")
low_extinction <- select_dataset_timepoint(complete_ds_low, "extinction")
low_extinction_recall <- select_dataset_timepoint(complete_ds_low, "extinction_recall")
low_reminder_shock <- select_dataset_timepoint(complete_ds_low, "reminder_shock")
low_reminder_recall <- select_dataset_timepoint(complete_ds_low, "reminder_recall")

# For "high" category datasets
high_acquisition <- select_dataset_timepoint(complete_ds_high, "acquisition")
high_recall_combined <- select_dataset_timepoint(complete_ds_high, "recall_combined")
high_recall_2only <- select_dataset_timepoint(complete_ds_high, "recall_2only")
high_extinction <- select_dataset_timepoint(complete_ds_high, "extinction")
high_extinction_recall <- select_dataset_timepoint(complete_ds_high, "extinction_recall")
high_reminder_shock <- select_dataset_timepoint(complete_ds_high, "reminder_shock")
high_reminder_recall <- select_dataset_timepoint(complete_ds_high, "reminder_recall")


#### Inferential analysis

#### Begin taken from other script ---------------------------------------------
#inferential stats prepost high#####
#add ID column
freezing_acquisition_high$ID <- seq.int(nrow(freezing_acquisition_high))
freezing_acquisition_long_high <- freezing_acquisition_high %>%
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")

freezing_acquisition_long_high$Timepoint <- as.factor(freezing_acquisition_long_high$Timepoint)
prepost_aov_high <- aov(data = freezing_acquisition_long_high, Percentage ~ Sex*Stress*Timepoint)
summary(prepost_aov_high)

TukeyHSD(prepost_aov_high)

##### END taken from other script ----------------------------------------------

#PrePost
collect_pre_post <- function(dataset) {
  # Add a unique identifier for each subject
  dataset$Subject <- seq_along(dataset[,1])
  
  # Pivot the data to long format
  freezing_acquisition <- dataset %>% 
    pivot_longer(cols = c("Pre", "Post"), 
                 names_to = "Pre_Post", 
                 values_to = "Percentage_freezing")
  
  return(freezing_acquisition)
}

freezing_acquisition_low_long <- collect_pre_post(low_acquisition)
freezing_acquisition_high_long <- collect_pre_post(high_acquisition)


# Pre and post are repeated measures -> will run a repeated measures linear model here.
pre_post_rm_lme4 <- function(dataset) {
  # Subject here is the animal ID assigned when pivot longer 
  # Fit the mixed-effects model
  rm_lme4_model <- lmer(Percentage_freezing ~ Pre_Post * Sex * Stress + (1|Subject), data = dataset)
  summary(rm_lme4_model)
  
  #plot(reside(rm_lme4_model))
  plot(rm_lme4_model)
  return(rm_lme4_model)
}

rm_lme4_model <- lmer(Percentage_freezing ~ Pre_Post * Sex * Stress + (1|Subject), data = freezing_acquisition_low_long)
plot(rm_lme4_model)
qqnorm(resid(rm_lme4_model))
qqline(resid(rm_lme4_model))
hist(resid(rm_lme4_model))

# Apply the function 
rm_lme4_model_low <- pre_post_rm_lme4(freezing_acquisition_low_long)
summary(rm_lme4_model_low)
rm_lme4_model_high <- pre_post_rm_lme4(freezing_acquisition_high_long)
summary(rm_lme4_model_high)





check_histograms <- function(dataset){
  hist(dataset$residuals)
}


#Low
hist(rm_lme4_model_low$residuals)

#High
hist(rm_lme4_model_high$residuals)


# HIGH 2 minute extinction if I want to only look at 2 minute group


#LOW 2 minute extinction if I want to only look at 2 minute group
#### RECALL ##################################


# No need to filter by 2 as recall is for every animal
filter_recall_group <- function(dataset){
  recall_filtered_dataset <- dataset %>% 
    select(c(1:4),recall_1)
  return(recall_filtered_dataset)
}

recall_group_2_low <- filter_recall_group(complete_ds_low)
recall_group_2_high <- filter_recall_group(complete_ds_high)

recall_group_2_low_count <- recall_group_2_low %>% 
  group_by(Sex, Stress) %>% 
  count()


recall_anova_test <- function(dataset){
  recall_lm <- lm(data = dataset, recall_1 ~ Sex * Stress * Condition)
  #summary(recall_lm_high)
  anova_result <- anova(recall_lm)
  return(anova_result)
}
recall_anova_low <- recall_anova_test(recall_group_2_low)
recall_anova_high <- recall_anova_test(recall_group_2_high)



#### Extinction ----


filter_extinction_group <- function(dataset){
  ten_minute_extinction <- dataset %>% 
    select(c(1:4),c(8:12)) %>% 
    filter(Condition ==10)
  return(ten_minute_extinction)
}
ten_minute_pivot_function <- function (dataset){
  dataset$Subject <- seq_along(dataset[,1])
  ten_minute_extinction_long <- dataset %>% 
    pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
    droplevels()
  ten_minute_extinction_long$timepoint <- as.factor(ten_minute_extinction_long$timepoint)
  return(ten_minute_extinction_long)
}
  


ten_minute_low <- filter_extinction_group(complete_ds_low)
ten_minute_high <- filter_extinction_group(complete_ds_high)

ten_minute_pivot_low <- ten_minute_pivot_function(ten_minute_low)
ten_minute_pivot_high <- ten_minute_pivot_function(ten_minute_high)


##### What model should I use here.
# I think this is the best model to use
# How do I see if there is an influence of timepoint, only considering the difference between 2 and 10 (1st and last timepoint).
extinction_timecourse_model <- function(dataset) {
  # Assuming 'Subject' is your subject identifier in the dataset
  # Fit the mixed-effects model
  rm_lme4_model <- lmer(percentage ~ timepoint * Sex * Stress + (1|Subject), data = dataset)
  summary(rm_lme4_model)
  
  return(rm_lme4_model)
}
extinction_timecourse_results_low <- extinction_timecourse_model(ten_minute_pivot_low)
summary(extinction_timecourse_results_low)
extinction_timecourse_results_high <- extinction_timecourse_model(ten_minute_pivot_high)
summary(extinction_timecourse_results_high)
# 
# # MODEL 1: Sex + Stress + Sex * Stress + timepoint <- very simple model
# low_ten_min_extinction_model <- lm(data = ten_minute_extinction_low_long, percentage ~ Stress + Sex + Sex * Stress * timepoint)
# 
# summary(low_ten_min_extinction_model)
# Anova(low_ten_min_extinction_model)
# 
# #### HIGH Inferential 10 minute (ten_minute_extinction) ----
# ten_minute_extinction_high <- complete_ds_high %>% 
#   select(c(1:4),c(8:12)) %>% 
#   filter(Condition ==10)
# 
# #ten_minute_ds <- list(ten_minute_extinction$ext1_curve, ten_minute_extinction$ext2_curve, ten_minute_extinction$ext3_curve, ten_minute_extinction$ext4_curve, ten_minute_extinction$ext5_curve)
# 
# ten_minute_extinction_high_long <- ten_minute_extinction_high %>% 
#   pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
#   droplevels()
# ten_minute_extinction_high_long$timepoint <- as.factor(ten_minute_extinction_high_long$timepoint)
# 
# # MODEL 1: Sex + Stress + Sex * Stress + timepoint <- very simple model
# high_ten_min_extinction_model <- lm(data = ten_minute_extinction_high_long, percentage ~ Stress + Sex + Sex * Stress * timepoint)
# 
# summary(high_ten_min_extinction_model)
# Anova(high_ten_min_extinction_model)



#### Individual 10 minute RM examinations: 

filter_pivot_individual_function <- function(dataset, stress_condition = "ELS", sex = "Male"){
  filtered_stress_sex <- dataset %>% 
    filter(Stress == stress_condition & Sex == sex)
  
  filtered_stress_sex_long <- filtered_stress_sex %>% 
    pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
    droplevels()
  return(filtered_stress_sex_long)
}
# Generate the long datasets with the function above
# long_ten_ELS_Male_low <- filter_pivot_individual_function(ten_minute_extinction_low, "ELS", "Male")
# long_ten_NS_Male_low <- filter_pivot_individual_function(ten_minute_extinction_low, "NS", "Male")
# long_ten_ELS_Female_low <- filter_pivot_individual_function(ten_minute_extinction_low, "ELS", "Female")
# long_ten_NS_Female_low <- filter_pivot_individual_function(ten_minute_extinction_low, "NS", "Female")
# 
# long_ten_ELS_Male_low <- filter_pivot_individual_function(ten_minute_extinction_low, "ELS", "Male")
# long_ten_NS_Male_low <- filter_pivot_individual_function(ten_minute_extinction_low, "NS", "Male")
# long_ten_ELS_Female_low <- filter_pivot_individual_function(ten_minute_extinction_low, "ELS", "Female")
# long_ten_NS_Female_low <- filter_pivot_individual_function(ten_minute_extinction_low, "NS", "Female")
# A more concise loop way to do the above
stress_conditions <- c("ELS", "NS")
sexes <- c("Male", "Female")
datasets <- list(ten_minute_extinction_low = ten_minute_extinction_low,
                 ten_minute_extinction_high = ten_minute_extinction_high)

for (data_name in names(datasets)) {
  for (stress in stress_conditions) {
    for (sex in sexes) {
      variable_name <- paste("long_ten", stress, sex, gsub("ten_minute_extinction_", "", data_name), sep = "_")
      assign(variable_name, filter_pivot_individual_function(datasets[[data_name]], stress, sex))
    }
  }
}


long_individual_ten_analysis_function <- function(dataset){
  ten_min_extinction_model <- lm(data = dataset, percentage ~ timepoint)
  summary(ten_min_extinction_model)
  anova_value <- Anova(ten_min_extinction_model)
  return(anova_value)
}
# Keep like this, even though copy paste I prefer this for inferential analysis.
low_indiv_extinction_analysis_ELS_M <- long_individual_ten_analysis_function(long_ten_ELS_Male_low)
low_indiv_extinction_analysis_NS_M <- long_individual_ten_analysis_function(long_ten_NS_Male_low)
low_indiv_extinction_analysis_ELS_F <- long_individual_ten_analysis_function(long_ten_ELS_Female_low)
low_indiv_extinction_analysis_NS_F <- long_individual_ten_analysis_function(long_ten_NS_Female_low)

high_indiv_extinction_analysis_ELS_M <- long_individual_ten_analysis_function(long_ten_ELS_Male_high)
high_indiv_extinction_analysis_NS_M <- long_individual_ten_analysis_function(long_ten_NS_Male_high)
high_indiv_extinction_analysis_ELS_F <- long_individual_ten_analysis_function(long_ten_ELS_Female_high)
high_indiv_extinction_analysis_NS_F <- long_individual_ten_analysis_function(long_ten_NS_Female_high)

# try analysing 4 and 6 minutes 
four_or_six_extinction_dataset <- function(dataset, extinction_time = 4){
  
  if (extinction_time == 4) {
   dataset_4or6 <- dataset %>% 
     select(c(1:6)) %>% 
     pivot_longer(cols = c(5:6), names_to = "timepoint", values_to = "percentage") %>% 
     droplevels()
   num_rows <- nrow(dataset_4or6)
   dataset_4or6$Subject <- rep(1:(num_rows/2), each=2)
  }
  else if (extinction_time == 6) {
    dataset_4or6 <- dataset %>% 
      select(c(1:7)) %>% 
      pivot_longer(cols = c(5:7), names_to = "timepoint", values_to = "percentage") %>% 
      droplevels()
    num_rows <- nrow(dataset_4or6)
    dataset_4or6$Subject <- rep(1:(num_rows/3), each=3)
  }
  return(dataset_4or6)
}
# Generate the datasets 
four_minute_dataset_low <- four_or_six_extinction_dataset(ten_minute_extinction_low, extinction_time = 4)
six_minute_dataset_low <- four_or_six_extinction_dataset(ten_minute_extinction_low, extinction_time = 6)

four_minute_dataset_high <- four_or_six_extinction_dataset(ten_minute_extinction_high, extinction_time = 4)
six_minute_dataset_high <- four_or_six_extinction_dataset(ten_minute_extinction_high, extinction_time = 6)

# run the statistics
four_minute_dataset_results_low <- extinction_timecourse_model(four_minute_dataset_low)
six_minute_dataset_results_low <- extinction_timecourse_model(six_minute_dataset_low)

four_minute_dataset_results_high <- extinction_timecourse_model(four_minute_dataset_high)
six_minute_dataset_results_high <- extinction_timecourse_model(six_minute_dataset_high)

# see the summaries <= This shows what Kerrie suggests. But we already see extinction at this point.
summary(four_minute_dataset_results_low)
summary(six_minute_dataset_results_low)

summary(four_minute_dataset_results_high)
summary(six_minute_dataset_results_high)



#### HIGH extinction recall (recall_1) 
recall_filter_function <- function(dataset){
  recall_1 <- dataset %>% 
    select(c(1:4),"extinction_recall")
}
recall_1_low <- recall_filter_function(complete_ds_low)
recall_1_high <- recall_filter_function(complete_ds_high)

# 
# hist(recall_1_high$extinction_recall)
# 
# #log transform
# recall_1_high$extinction_recallPLusone <- recall_1_high$extinction_recall + 1
# recall_1_high$extinction_recall_log <- log(recall_1_high$extinction_recallPLusone)
# 
# #sqrt transform
# recall_1_high$extinction_recall_sqrt <- sqrt(recall_1_high$extinction_recall)
# 
# #box cox - doesnt work 
# recall_1_high$extinction_recall_box <- boxCox(recall_1_high$extinction_recall)
# 
# hist(recall_1_high$extinction_recall)
# hist(recall_1_high$extinction_recall_log)
# shapiro.test(recall_1_high$extinction_recall)
# shapiro.test(recall_1_high$extinction_recall_sqrt)

recall_inferential_test_function <- function(dataset){
  ext_recall_lm <- lm(data = dataset, extinction_recall ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition)
  summary(ext_recall_lm)
  anova_test <- Anova(ext_recall_lm)
  return (anova_test)
}
ext_recall_low_anova <- recall_inferential_test_function(recall_1_low)
ext_recall_high_anova <- recall_inferential_test_function(recall_1_high)
#try a possion dist <- not sure that this is correct
# poisson_model_high_glm <- glm(extinction_recall ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition, data = recall_1_high, family = poisson(link = "log"))
# summary(poisson_model_high_glm)
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
