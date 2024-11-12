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

# Exploratory Data Analysis (EDA) Overview


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

# 1. Data Overview and Structure
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

# The distribution becomes more left skewed for the post scores.



# 2. Data Cleaning and Preprocessing
# - Check for missing values and handle them appropriately (e.g., imputation or removal).
# - Convert any columns to the appropriate data types (factors for categorical data).
# - Look for and address any potential outliers or inconsistencies in the data.

# 3. Univariate Analysis
# - Analyze each variable individually to understand distributions:
#   - For categorical variables (e.g., Sex, Stress, Condition), use bar plots or pie charts to show proportions.
#   - For continuous variables, create histograms or density plots to assess spread, skewness, and central tendency.

# 4. Bivariate Analysis
# - Explore relationships between pairs of variables:
#   - Use boxplots or violin plots to visualise the distribution of continuous variables across categories.
#   - Cross-tabulations and mosaic plots to show interactions between categorical variables.

# 5. Multivariate Analysis and Interactions
# - Investigate interactions between multiple variables:
#   - Use faceted plots to examine differences across combinations of variables, such as Sex and Stress on Condition.
#   - Pair plots or correlation plots (if there are multiple continuous variables) to identify relationships.

# 6. Initial Observations and Summary of Patterns
# - Record any patterns, clusters, or notable trends observed in the data.
# - Identify any unexpected findings or potential issues (e.g., large class imbalances, unusual outliers).

# 7. Next Steps and Preparation for Inferential Analysis CHeck for normality across the different variables.
# - Summarise key insights from the EDA that will guide the inferential analysis.
# - Document any decisions made during EDA (e.g., data cleaning steps, transformations applied).
# - Ensure that all insights align with the assumptions and design of your planned statistical tests.

