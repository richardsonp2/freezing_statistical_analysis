library(tidyverse)
library(ggsignif)
library(ggpubr)
library(car)
library(ggpattern)
library(rstatix)
library(cowplot)
library(svglite)
library(grid)
library(emmeans)
library(GGally)

# Exploratory Data Analysis (EDA) Overview



# TODO where interactions are present, pull the required tests into the main script. Remove the copied and pasted stuff.
#### Data import and cleaning --------------------------------------------------
# This is the exact same as the EDA.R and data_visualisation.R script (up to Inferential Analysis). I would really like to make this a package. 
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
  factor_cols <- c("Shock", "Stress", "Sex", "Condition")
  ds <- clean_factors(ds, factor_cols)
  
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


#### Data Overview and Structure -----------------------------------------------
# - Load the data and review its structure (columns, data types, missing values).
# - Summarise categorical variables (e.g., Sex, Stress, Condition) and examine class distributions.
# - Identify continuous vs. categorical variables to guide the choice of visualisations.
# TODO might want to move the counts to here. 
# Counts are present in the visualisation script 

# Group the data by stress and by sex and show a histogram of the pre (and post) scores across the different stress levels, and then another figure with the differences across sex 

# Remove NA's from the dataset as a subset 
acquisition_ds_na_remove <- complete_ds %>% 
  select(Pre, Post, factor_cols) %>%
  na.omit()


pre_stress <- ggplot(acquisition_ds_na_remove, aes(x = Pre, fill = Stress)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Stress) +
  labs(title = "Distribution of Pre Scores by Stress Level",
       x = "Pre Score",
       y = "Frequency",
       fill = "Stress Level") +
  theme_minimal()

pre_sex <- ggplot(acquisition_ds_na_remove, aes(x = Pre, fill = Sex)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Sex) +
  labs(title = "Distribution of Pre Scores by Sex",
       x = "Pre Score",
       y = "Frequency",
       fill = "Sex") +
  theme_minimal()

pre_stress
pre_sex

# As expected the values for pre are highly zero inflated. 

post_stress <- ggplot(acquisition_ds_na_remove, aes(x = Post, fill = Stress)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Stress) +
  labs(title = "Distribution of Post Scores by Stress Level",
       x = "Post Score",
       y = "Frequency",
       fill = "Stress Level") +
  theme_minimal()

post_sex <- ggplot(acquisition_ds_na_remove, aes(x = Post, fill = Sex)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Sex) +
  labs(title = "Distribution of Post Scores by Sex",
       x = "Post Score",
       y = "Frequency",
       fill = "Sex") +
  theme_minimal()

post_stress
post_sex

# The distribution becomes more left skewed for the post scores. Which of course makes sense.



#### Check for and address any potential outliers or inconsistencies in the data. ---------------
# - Use boxplots or violin plots to identify potential outliers or extreme values. However, due to the min max being 0 and 100 extreme values dont really apply here. 
complete_ds %>%
  select(num_cols) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot of Continuous Variables",
       x = "Variable",
       y = "Value") +
  theme_minimal()

# Can see that most of the data is showing freezing levels below 50 %.

# How much has the shock intensity changed the freezing levels? 
# This is a bit of a complex plot, but it shows the change in freezing levels across the different shock intensities.
# Low
low_boxplots <- complete_ds %>% 
  filter(Shock == "l") %>%
  select(num_cols) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "LOW Boxplot of Continuous Variables",
       x = "Variable",
       y = "Value") +
  theme_minimal()

low_boxplots

high_boxplots <- complete_ds %>% 
  filter(Shock == "h") %>%
  select(num_cols) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "HIGH Boxplot of Continuous Variables",
       x = "Variable",
       y = "Value") +
  theme_minimal()

high_boxplots

# Show the plots side by side
plot_grid(low_boxplots, high_boxplots)

# High shock intensity has brought the levels up, but most freezing is still well below 50%. 

#### Univariate Analysis of factoral variables.---------------------------------
#   - For categorical variables (e.g., Sex, Stress, Condition), use pie charts to show proportions.

# Function to produce pie charts for categorical variables
create_pie_charts <- function(data, factor_columns) {
  data %>% 
    select(all_of(factor_columns)) %>%
    gather(key = "variable", value = "value") %>%
    filter(!is.na(value)) %>%  # Exclude NA values
    count(variable, value) %>%
    ggplot(aes(x = "", y = n, fill = value)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    facet_wrap(~variable) +
    labs(
      title = "Proportion of Categorical Variables",
      fill = "Value"
    ) +
    scale_fill_manual(
      values = c("ELS" = "green", "NS" = "red", "Male" = "blue", "Female" = "orange", 
                 "2" = "yellow", "10" = "pink", "l" = "brown", "h" = "beige"),
      labels = c("ELS" = "ELS", "NS" = "NS", "Male" = "Male", "Female" = "Female", 
                 "2" = "Extinction Control", "10" = "Extinction trained", "l" = "Low", "h" = "High")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
}

# Can present the pie charts for each timepoint. However be aware some pies will not make much sense (eg Condition at acquisition)
pie_charts_acquisition_low <- create_pie_charts(low_acquisition, factor_cols)
# Acquisition


#   - For continuous variables, create histograms or density plots to assess spread, skewness, and central tendency.

# 4. Bivariate Analysis
# - Explore relationships between pairs of variables:
#   - Use boxplots or violin plots to visualise the distribution of continuous variables across categories.
#   - Cross-tabulations and mosaic plots to show interactions between categorical variables.

# 5. Multivariate Analysis and Interactions
# - Investigate interactions between multiple variables:
#   - Use faceted plots to examine differences across combinations of variables, such as Sex and Stress on Condition.

#   - Pair plots to identify relationships.
complete_ds %>%
  select(num_cols) %>%
  ggpairs() +
  labs(title = "Correlation Matrix of Numerical Variables") +
  theme_minimal()
