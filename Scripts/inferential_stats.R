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


# TODO where interactions are present, pull the required tests into the main script. Remove the copied and pasted stuff.
#### Data import and cleaning --------------------------------------------------

# Begin by loading the dataset 
file_path <- "./Datasets/high_low_combined.csv"

# I will use these throughout the script to both convert the datatype of the col appropriately and
# select by different columns
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
    "extinction" = dataset %>% select(all_of(extinction_cols), all_of(factor_cols)) %>% filter(Condition == 10),
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

#### Acquisition ---------------------------------------------------------------
#add ID column
# freezing_acquisition_high$ID <- seq.int(nrow(freezing_acquisition_high))
# freezing_acquisition_long_high <- freezing_acquisition_high %>%
#   pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")
# 
# freezing_acquisition_long_high$Timepoint <- as.factor(freezing_acquisition_long_high$Timepoint)
# prepost_aov_high <- aov(data = freezing_acquisition_long_high, Percentage ~ Sex*Stress*Timepoint)
# summary(prepost_aov_high)
# 
# TukeyHSD(prepost_aov_high)

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

# Check over normality of residuals. Was the model correct to use? 
# Residuals look normally distributed, if a little kurtotic.
# QQ plot shows some deviation from normality, but not too bad (is this expected with the pre-post element of the data?)
check_model_assumptions <- function(model) {
  plot(model)
  qqnorm(resid(model))
  qqline(resid(model))
  hist(resid(model))
}
# Apply the function 
rm_lme4_model_low <- pre_post_rm_lme4(freezing_acquisition_low_long)
summary(rm_lme4_model_low)
rm_lme4_model_high <- pre_post_rm_lme4(freezing_acquisition_high_long)
summary(rm_lme4_model_high)

check_model_assumptions(rm_lme4_model_low)
check_model_assumptions(rm_lme4_model_high)

#### Recall  --------------------------------------------------------------------
# Use low_recall_combined and high_recall_combined


# No need to use repeated models here, only one time point 
recall_anova_test <- function(dataset){
  recall_lm <- lm(data = dataset, recall_1 ~ Sex * Stress * Condition)
  #summary(recall_lm_high)
  anova_result <- anova(recall_lm)
  return(anova_result)
}

# Check over normality of residuals. Was the model correct to use? 
# Residuals 
# QQ plot 
# low
recall_lm_model_low <- lm(data = low_recall_combined, recall_1 ~ Sex * Stress * Condition)
plot(recall_lm_model_low)
qqnorm(resid(recall_lm_model_low))
qqline(resid(recall_lm_model_low))
hist(resid(recall_lm_model_low))

# high
recall_lm_model_high <- lm(data = high_recall_combined, recall_1 ~ Sex * Stress * Condition)
plot(recall_lm_model_high)
qqnorm(resid(recall_lm_model_high))
qqline(resid(recall_lm_model_high))
hist(resid(recall_lm_model_high))


# Presentation of ANOVA results 
# Perhaps remove
recall_anova_low <- recall_anova_test(low_recall_combined)
recall_anova_high <- recall_anova_test(high_recall_combined)

#### Extinction ----------------------------------------------------------------
# Use low_extinction and high_extinction

ten_minute_pivot_function <- function (dataset){
  print(head(dataset))
  dataset$Subject <- seq_along(dataset[,1])
  ten_minute_extinction_long <- dataset %>% 
    pivot_longer(cols = c(1:5), names_to = "timepoint", values_to = "percentage") %>% 
    droplevels()
  ten_minute_extinction_long$timepoint <- as.factor(ten_minute_extinction_long$timepoint)
  return(ten_minute_extinction_long)
}
  
ten_minute_pivot_low <- ten_minute_pivot_function(low_extinction)
ten_minute_pivot_high <- ten_minute_pivot_function(high_extinction)

# Building a model that compares timepoint, sex and stress with subject as nested vairable
# This is a mixed effects model
# This is a repeated measures model
# This is a model that will be used to compare the timecourse of extinction between groups
extinction_timecourse_model <- function(dataset) {
  # Subject is the identifier in the dataset
  # Fit the mixed-effects model
  rm_lme4_model <- lmer(percentage ~ timepoint * Sex * Stress + (1|Subject), data = dataset)
  summary(rm_lme4_model)
  
  return(rm_lme4_model)
}
extinction_timecourse_results_low <- extinction_timecourse_model(ten_minute_pivot_low)
summary(extinction_timecourse_results_low)
extinction_timecourse_results_high <- extinction_timecourse_model(ten_minute_pivot_high)
summary(extinction_timecourse_results_high)

# Check over normality of residuals. Was the model correct to use?
# Residuals look fairly normal. Low is right skewed a little.
# QQ plot looks good here
check_model_assumptions(extinction_timecourse_results_low)
check_model_assumptions(extinction_timecourse_results_high)


#### Extinction recall ---------------------------------------------------------

# Building a model that will test the effects of sex, stress and condition on extinction recall. 
# This is a mixed effects model that will look at if there is an effecti between any of the factors.
recall_inferential_test_function <- function(dataset){
  ext_recall_lm <- lm(data = dataset, extinction_recall ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition)
  summary(ext_recall_lm)
  anova_test <- Anova(ext_recall_lm)
  return (anova_test)
}
ext_recall_low_anova <- recall_inferential_test_function(low_extinction_recall)
ext_recall_high_anova <- recall_inferential_test_function(high_extinction_recall)

# Individual t tests conducted assess each combined factor pair, for example M ELS and M NS (male early life stress and non stressed respectively) 
# I do this as ad hoc I knew that these are the results I was most intersted in. 
# Futhermore, these tests are completely independant and dont need to be corrected for multiple comparisons. 

filter_sex_stress_condition <- function(dataset, sex = "Male", stress = "ELS", condition = 2){
  filtered_set <- dataset %>% 
    filter(Sex == sex, Stress == stress, Condition == condition)
  return(filtered_set)
}
# low
# I know that the below could perhaps go into a loop, but for readability I think this looks better.
# I still like to have the datasets so I can check things with the environment viewer and open the file to show people etc.
m_els_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "ELS", condition = 2)
m_els_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "ELS", condition = 10)
m_els_combined_low <- filter_sex_stress_condition()

m_ns_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "NS", condition = 2)
m_ns_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "NS", condition = 10)

f_els_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "ELS", condition = 2)
f_els_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "ELS", condition = 10)

f_ns_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "NS", condition = 2)
f_ns_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "NS", condition = 10)

# high
m_els_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "ELS", condition = 2)
m_els_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "ELS", condition = 10)

m_ns_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "NS", condition = 2)
m_ns_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "NS", condition = 10)

f_els_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "ELS", condition = 2)
f_els_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "ELS", condition = 10)

f_ns_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "NS", condition = 2)
f_ns_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "NS", condition = 10)

# Check the t-tests 
t.test(extinction_recall ~ Condition, data = m_els_ds_high)
t.test(extinction_recall ~ Condition, data = m_ns_ds_high)
t.test(extinction_recall ~ Condition, data = f_els_ds_high)
t.test(extinction_recall ~ Condition, data = f_ns_ds_high)


