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

#### Data import and cleaning --------------------------------------------------
# This is the exact same as the EDA.R and data_visualisation.R script (up to Inferential Analysis). I would really like to make this a package. 
# The dataset contains some artifacts which were in place to make manually scoring behaivour easier. For example putting in X's where freezing was absent. 
# The dataset also contains artifacts accidently put in during the manual scoring of the behaviour. 
# Many rows will have NA values due to either not recording the freezing on that day, or being a 10 minute point in the 2 minute group. 
# I am thinking of making this into a package for any future freezing analysis needs. This is why most of the code is commented with Roxygen comments.
# Begin by loading the dataset 
file_path <- "./Datasets/high_low_combined.csv"

# I will use these throughout the script to both convert the datatype of the col appropriately and
# select by different columns
factor_cols <- c("Shock", "Stress", "Sex", "Condition")
num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", 
              "ext3_curve", "ext4_curve", "ext5_curve", 
              "extinction_recall", "reminder_day1_shock", "reminder_day2")

#' Read and Clean Raw Data
#'
#' Helper function to load a CSV file and replace specified values with `NA`.
#'
#' @param file_path A string specifying the path to the CSV file.
#' @param na_strings A character vector of strings to be treated as `NA`.
#' @return A data frame with specified values replaced by `NA`.
#' @importFrom utils read.csv
read_and_clean_raw_data <- function(file_path, na_strings = c(".", "#DIV/0!")) {
  ds <- read.csv(file_path, na.strings = na_strings)
  ds[ds == "#DIV/0!"] <- NA
  return(ds)
}

#' Clean Factor Columns
#'
#' Helper function to clean and drop unused levels in specified factor columns.
#'
#' @param ds A data frame containing the dataset.
#' @param factor_cols A character vector specifying the factor columns to clean.
#' @return A data frame with cleaned factor columns.
#' @importFrom base droplevels
clean_factors <- function(ds, factor_cols) {
  for (col in factor_cols) {
    
    # Convert to factor if not already
    if (!is.factor(ds[[col]])) {
      ds[[col]] <- as.factor(ds[[col]])
    }
    
    # Replace empty strings with NA
    ds[[col]][ds[[col]] == ""] <- NA
    # Drop unused factor levels
    ds[[col]] <- droplevels(ds[[col]])
  }
  return(ds)
}

# Function to convert columns to appropriate data types: factors and numerics
convert_columns <- function(ds) {
  factor_cols <- c("Shock", "Stress", "Sex", "Condition")
  num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", 
                "ext3_curve", "ext4_curve", "ext5_curve", 
                "extinction_recall", "reminder_day1_shock", "reminder_day2")
  
  ds[factor_cols] <- lapply(ds[factor_cols], function(x) as.factor(x))
  ds[num_cols] <- lapply(ds[num_cols], as.numeric)
  
  return(ds)
}

#' Perform Main Cleaning
#'
#' Main function to clean a dataset by combining raw data cleaning and factor column processing.
#'
#' @param file_path A string specifying the path to the CSV file.
#' @return A cleaned data frame.
#' @importFrom dplyr filter
#' @importFrom forcats fct_rev
#' @export
clean_dataset <- function(file_path) {
  # Step 1: Read and clean raw data
  ds <- read_and_clean_raw_data(file_path)
  
  # Step 2: Fix space issue in the 'Stress' variable
  ds$Stress[ds$Stress == " NS"] <- "NS"
  
  # Step 3: Remove invalid entries in 'Sex'
  ds <- ds %>%
    filter(Sex != "x")
  
  # Step 4: Clean factor columns
  ds <- convert_columns(ds)
  
  return(ds)
}




#' Reverse Factor Levels
#'
#' This function reverses the order of levels in a specified factor column within a dataset.
#'
#' @param ds A data frame containing the dataset.
#' @param factor_col A string specifying the name of the factor column in the dataset whose levels
#'   should be reversed.
#'
#' @details This function modifies the specified factor column by reversing its levels using
#'   the `fct_rev` function from the `forcats` package.
#'
#' @return A data frame with the specified factor column updated to have reversed levels.
#'
#' @examples
#' # Example dataset
#' ds <- data.frame(
#'   Condition = factor(c("Control", "Treatment", "Placebo"))
#' )
#'
#' # Reverse the factor levels of the 'Condition' column
#' reversed_ds <- reverse_factor_levels(ds, "Condition")
#'
#' @importFrom forcats fct_rev
#' @export
reverse_factor_levels <- function(ds, factor_col){
  ds$factor_col <- fct_rev(ds$factor_col)
  return(ds)
}

# Pre-processing script execution

complete_ds <- clean_dataset(file_path)
str(complete_ds)

#' Standardize Sex Column Values
#'
#' This function standardizes the values in the `Sex` column of a dataset by converting 
#' short forms ("M" and "F") to full forms ("Male" and "Female").
#'
#' @param ds A data frame containing a `Sex` column to standardize. The `Sex` column 
#'   can be either a character or a factor.
#'
#' @details The function converts the `Sex` column to a character vector (if it is not 
#'   already), replaces "M" with "Male" and "F" with "Female", and ensures the column
#'   is returned in a standardized format.
#'
#' @return A data frame with the `Sex` column standardized to "Male" and "Female".
#'
#' @examples
#' # Example dataset
#' ds <- data.frame(
#'   Sex = c("M", "F", "Male", "Female", "M"),
#'   Value = 1:5
#' )
#'
#' # Standardize the Sex column
#' standardized_ds <- standardize_sex_column(ds)
#'
#' @export
standardise_sex_column <- function(ds, convert_back_factor = TRUE) {
  # Convert Sex column to character if not already
  ds$Sex <- as.character(ds$Sex)
  
  # Replace short forms with full forms
  ds$Sex[ds$Sex == "M"] <- "Male"
  ds$Sex[ds$Sex == "F"] <- "Female"
  
  if (convert_back_factor) {
    ds$Sex <- as.factor(ds$Sex)
  }
  
  return(ds)
}

# I have inputted Male, M, Female and F for sex accidentally during the freezing analysis
# Fix that here, bit of a complex workaround but I was having issues with dplyr 
complete_ds <- standardise_sex_column(complete_ds, convert_back_factor = TRUE)

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
  
  # Instead of a big long loop of if statements, a switch statement is used here
  subset_dataset <- switch(
    timepoint,
    "acquisition" = dataset %>% select(Pre, Post, all_of(factor_cols)),
    "recall_combined" = dataset %>% select(recall_1, all_of(factor_cols)),
    "recall_2only" = dataset %>% select(recall_1, all_of(factor_cols)) %>% filter(Condition == 2),
    "extinction" = dataset %>%
      select(all_of(extinction_cols), all_of(factor_cols)) %>%
      filter(Condition == 10) %>%
      mutate(across(all_of(extinction_cols), as.numeric)),
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


#### Inferential analysis ------------------------------------------------------

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
  
  # Convert columns to proper types
  ten_minute_extinction_long <- ten_minute_extinction_long %>%
    mutate(
      timepoint = as.factor(timepoint),
      percentage = as.numeric(percentage)
    )
  
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

filter_sex_stress <- function(dataset, sex = "Male", stress = "ELS"){
  filtered_set <- dataset %>% 
    filter(Sex == sex, Stress == stress)
}

filter_sex_stress_condition <- function(dataset, sex = "Male", stress = "ELS", condition = 2){
  filtered_set <- dataset %>% 
    filter(Sex == sex, Stress == stress, Condition == condition)
  return(filtered_set)
}
# low
# I know that the below could perhaps go into a loop, but for readability I think this "looks" better.
# I still like to have the datasets so I can check things with the environment viewer and open the file to show people etc.
m_els_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "ELS", condition = 2)
m_els_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "ELS", condition = 10)
m_els_combined_low <- filter_sex_stress(low_extinction_recall, sex = "Male", stress = "ELS")

m_ns_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "NS", condition = 2)
m_ns_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Male", stress = "NS", condition = 10)
m_ns_combined_low <- filter_sex_stress(low_extinction_recall, sex = "Male", stress = "NS")

f_els_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "ELS", condition = 2)
f_els_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "ELS", condition = 10)
f_els_combined_low <- filter_sex_stress(low_extinction_recall, sex = "Female", stress = "ELS")

f_ns_2_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "NS", condition = 2)
f_ns_10_low <- filter_sex_stress_condition(low_extinction_recall, sex = "Female", stress = "NS", condition = 10)
f_ns_combined_low <- filter_sex_stress(low_extinction_recall, sex = "Female", stress = "NS")

# high
m_els_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "ELS", condition = 2)
m_els_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "ELS", condition = 10)
m_els_combined_high <- filter_sex_stress(high_extinction_recall, sex = "Male", stress = "ELS")

m_ns_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "NS", condition = 2)
m_ns_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Male", stress = "NS", condition = 10)
m_ns_combined_high <- filter_sex_stress(high_extinction_recall, sex = "Male", stress = "NS")

f_els_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "ELS", condition = 2)
f_els_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "ELS", condition = 10)
f_els_combined_high <- filter_sex_stress(high_extinction_recall, sex = "Female", stress = "ELS")

f_ns_2_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "NS", condition = 2)
f_ns_10_high <- filter_sex_stress_condition(high_extinction_recall, sex = "Female", stress = "NS", condition = 10)
f_ns_combined_high <- filter_sex_stress(high_extinction_recall, sex = "Female", stress = "NS")

# Check the t-tests 
# Low
t.test(extinction_recall ~ Condition, data = m_els_combined_low)
t.test(extinction_recall ~ Condition, data = m_ns_combined_low)
t.test(extinction_recall ~ Condition, data = f_els_combined_low)
t.test(extinction_recall ~ Condition, data = f_ns_combined_low)
# High
t.test(extinction_recall ~ Condition, data = m_els_combined_high)
t.test(extinction_recall ~ Condition, data = m_ns_combined_high)
t.test(extinction_recall ~ Condition, data = f_els_combined_high)
t.test(extinction_recall ~ Condition, data = f_ns_combined_high)
