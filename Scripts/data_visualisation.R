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

# The dataset contains some artifacts which were in place to make manually scoring behaivour easier. For example putting in X's where freezing was absent. 
# The dataset also contains artifacts accidently put in during the manual scoring of the behaviour. 
# Many rows will have NA values due to either not recording the freezing on that day, or being a 10 minute point in the 2 minute group. 
# I am thinking of making this into a package for any future freezing analysis needs. This is why most of the code is commented with Roxygen comments.
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
################

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

# Optionally, you can convert it back to a factor if needed:
complete_ds$Sex <- as.factor(complete_ds$Sex)
# Display the structure of the cleaned dataset
str(complete_ds)


#' Count observations in a dataset based on grouping type
#'
#' This function counts the number of observations in a dataset with optional grouping by factors like "Sex," "Stress," 
#' "Condition," and "Shock." The grouping can be specified with the `type` argument.
#'
#' @param dataset A data frame containing the data to be counted.
#' @param type A character string specifying the grouping for the count. Valid options are:
#'   * "overall" - no grouping, just counts all observations
#'   * "sex" - groups by the `Sex` column
#'   * "sexstress" - groups by `Sex` and `Stress` columns
#'   * "allfactors" - groups by `Sex`, `Stress`, `Condition`, and `Shock` columns
#' @return A data frame with counts of observations based on the specified grouping.
#' @examples
#' # Count all observations
#' count_n(my_data, type = "overall")
#' @export
count_n <- function(dataset, type = "overall"){
  valid_choices <- c("overall", "sex", "sexstress", "allfactors")
  
  # Check if the input_string is one of the valid choices
  if (!(type %in% valid_choices)) {
    # Raise an exception with a custom error message
    stop(sprintf("Invalid input: '%s'. Valid options are: %s", 
                 type, paste(valid_choices, collapse = ", ")))
    }
  if (type == "overall"){
    n_dataset <- dataset %>%
      count()
  }
  else if (type == "sex"){
    n_dataset <- dataset %>% 
      group_by(Sex) %>% 
      count()
  }
  else if (type == "sexstress"){
    n_dataset <- dataset %>%
      group_by(Sex, Stress) %>%
      count()
    }
  else if (type == "allfactors"){
    n_dataset <- dataset %>% 
      group_by(Sex, Stress, Condition, Shock) %>% 
    count()
  }
  return (n_dataset)
}

##### Main count run -----------------------------------------------------------
overall_count <- count_n(complete_ds, "overall")
sex_count <- count_n(complete_ds, "sex")
sex_stress_count <- count_n(complete_ds, "sexstress")
all_factors_count <- count_n(complete_ds, "allfactors")


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

#### Counts for Low and High ---------------------------------------------------

# Low counts 
overall_count_low <- count_n(complete_ds_low, "overall")
sex_count_low <- count_n(complete_ds_low, "sex")
sex_stress_count_low <- count_n(complete_ds_low, "sexstress")
all_factors_count_low <- count_n(complete_ds_low, "allfactors")

# High counts
overall_count_high <- count_n(complete_ds_high, "overall")
sex_count_high <- count_n(complete_ds_high, "sex")
sex_stress_count_high <- count_n(complete_ds_high, "sexstress")
all_factors_count_high <- count_n(complete_ds_high, "allfactors")

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
#### Dataset generation for each timepoint -------------------------------------

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


# TODO convert this to a config file for better management of themes 
#### Figure Theme ##############################################################
blank_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            legend.position = "bottom",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            axis.text.x = element_text(size = 10, color = "#000000")
                            )

#### Poster figure theme -------------------------------------------------------
poster_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            legend.position = "none",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
)

annotate_figure_function <- function(figure,title_text, color = "red", face = "bold", size = 14)
{
  annotate_figure(p = figure, top = text_grob(title_text, color = color, face = face, size = size))
}

#### Four class colours --------------------------------------------------------
four_class <- c("#ff1f5b", "#00CD6C", "#009ADE", "#AF58BE")
orange_blue <- c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff")
orange_blue_bar <- c("#b3b4ff","#1c20fc", "#ffc182","#ff870f")
color_pall <- c("#b3b4ff","#b3b4ff","#1c20fc", "#1c20fc", "#ffc182", "#ffc182","#ff870f","#ff870f")


#### Two class colours ---------------------------------------------------------
extinction_colors <- c("#2f58fe", "#c6a605")

#### I have titles here that I will reuse a lot so defining them as variables
y_title <- "Freezing percentage"
#pre extinction, the figure for pre and post is in a different order so use a different key.
x_labels <- c("Male_ELS" = "ELS", "Male_NS" =  "Non-stressed", "Female_ELS" = "ELS", "Female_NS" =  "Non-stressed")
line_label<-c("Female ELS", "Female NS", "Male ELS", "Male NS")

#Key labels
key_label <-  c("Male NS", "Male ELS","Female NS", "Female ELS")




##### Acquisition ---------------------------------------------------------
#' Generate descriptive statistics for pre- and post-shock during CFC acquistion
#'
#' This function calculates the mean and standard error of the mean (SEM) for 
#' pre- and post-shock , grouped by `Sex` and `Stress` variables. 
#' The results can optionally be written to a CSV file, with separate files 
#' for low and high shock intensities (in the most up to date version 10/11/24, low shock is 0.5mA, high is
#' 0.7mA.
#'
#' @param dataset A data frame containing the variables `Sex`, `Stress`, `Pre`, and `Post` 
#'   representing the factors and freezing measurements (before and after treatment).
#' @param shock_intensity A character string specifying the shock intensity level 
#'   ("Low" or "High"). Determines the output file location.
#' @param write_file Logical; if `TRUE`, writes the summary statistics to a CSV file 
#'   based on the specified shock intensity.
#'
#' @return A data frame with the mean and SEM of freezing behavior pre- and post-treatment, 
#'   grouped by `Sex` and `Stress`.
#'
#' @examples
#' # Generate descriptives and save the output to a CSV file
#' make_prepost_descriptives(data, shock_intensity = "Low", write_file = TRUE)
#'
#' @export
make_prepost_descriptives <- function(dataset, shock_intensity, write_file = TRUE) {
  acquisition_descriptives <- dataset %>%
    group_by(Sex, Stress) %>%
    summarize(
      mean_freezing_pre = mean(Pre, na.rm = TRUE), 
      sem_freezing_pre = sd(Pre, na.rm = TRUE) / sqrt(length(Pre)), 
      mean_freezing_post = mean(Post, na.rm = TRUE), 
      sem_freezing_post = sd(Post, na.rm = TRUE) / sqrt(length(Post))
    )
  
  if (write_file == TRUE) {
    if (shock_intensity == "Low") {
      write.csv(acquisition_descriptives, file = "./Low/Descriptives/freezing_prepost_descriptives_low.csv")
    } else {
      write.csv(acquisition_descriptives, file = "./High/Descriptives/freezing_prepost_descriptives_high.csv")
    }
  }
  
  return(acquisition_descriptives)
}

freezing_prepost_descriptives_low <- make_prepost_descriptives(low_acquisition, "Low", write_file = TRUE)
freezing_prepost_descriptives_high <- make_prepost_descriptives(high_acquisition, "High", write_file = TRUE)


# For the kind of figure that I want to generate I need to unite Sex and Stress together as one variable.

make_figure_prepost_ds <- function(dataset){
  prepost_figure_ds <-  dataset %>%
    unite(sex_stress, c(Sex, Stress), remove=TRUE) %>%
    pivot_longer(cols = c(mean_freezing_pre, mean_freezing_post, sem_freezing_pre, sem_freezing_post), names_to = c(".value","prepost"), names_pattern = "(.*?)_(.*)") %>%
    mutate(prepost = fct_reorder(prepost, desc(prepost)))
  return(prepost_figure_ds)
  
}
prepost_figure_ds_low <- make_figure_prepost_ds(freezing_prepost_descriptives_low)
prepost_figure_ds_high <- make_figure_prepost_ds(freezing_prepost_descriptives_high)

#' Generate a line chart figure with error bars for pre- and post-conditions
#'
#' This function creates a line chart from the freezing input dataset containing both pre and post shock,
#' plotting the mean values across a prepost variable (in long format) with groups defined by sex and stress factors.
#' The y-axis limit can be customized, and error bars are added to represent the standard error of the mean.
#'
#' @param dataset A data frame containing the data to be plotted. It should include the following columns:
#'   * `prepost` - x-axis values for the pre- and post-conditions, should be in long format.
#'   * `mean` - mean values to plot on the y-axis.
#'   * `sex_stress` - a factor defining groups by sex and stress for color differentiation.
#'   * `sem` - standard error of the mean for error bars.
#' @param y_axis_limit A numeric value specifying the upper limit of the y-axis. Default is 100.
#' @return A ggplot object representing the line chart with error bars.
#' @examples
#' # Generate a line chart figure with the default y-axis limit
#' linechart_figure_function(my_data)
#'
#' # Generate a line chart figure with a custom y-axis limit
#' linechart_figure_function(my_data, y_axis_limit = 80)
#' @export
linechart_prepost_figure_function <- function(dataset, y_axis_limit = 100){
  linechart_prepost_high <- ggplot(dataset, aes(x= prepost, y = mean, group = sex_stress, colour = sex_stress))+
    geom_line(size = 1.0)+
    coord_cartesian(ylim = c(0, y_axis_limit)) +
    scale_y_continuous(breaks=seq(0,y_axis_limit,20), expand = c(0,0))+
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                  position=position_dodge(0.05))+
    scale_y_continuous(breaks=seq(0,y_axis_limit,20))
}

linechart_prepost_low <- linechart_prepost_figure_function(prepost_figure_ds_low, y_axis_limit = 100)
linechart_prepost_high <- linechart_prepost_figure_function(prepost_figure_ds_high, y_axis_limit = 100)

# Needed for a few of the downstream analyses
count_simple_function <- function(dataset){
 df_fearcond_count <- dataset %>%
   group_by(Sex, Stress) %>%
   count()
 return(df_fearcond_count)
}


prepost_count_function <- function(dataset){
  
  df_fearcond_count <- count_n(dataset, type = "sexstress") 
  
  count_prepost = list()
  for (i in df_fearcond_count$n)
  {
    count_prepost <- append(count_prepost,i)
  }  
  return (count_prepost)
}

low_count_acquisition <- prepost_count_function(low_acquisition)
high_count_acquisition <- prepost_count_function(high_acquisition)

low_string = "Low intensity shock"
high_string = "High intensity shock"

linechart_count_and_titles <- function(figure, count_list, shock_intensity = low_string){
  figure <- figure + scale_color_manual(labels = c("Female_ELS" = sprintf("Female ELS\n n = %s", count_list[1]), "Female_NS" =  sprintf("Female NS\n n = %s", count_list[2]), "Male_ELS" = sprintf("Male ELS\n n = %s", count_list[3]), "Male_NS" =  sprintf("Male NS\n n = %s", count_list[4])) ,values = orange_blue)
  figure <- figure + labs(title=shock_intensity, x = NULL, y = NULL, colour = "Sex and Stress type")
  figure <- figure + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )
  
  figure <- figure + blank_figure_theme
  return (figure)
  
}

low_figure <- linechart_count_and_titles(linechart_prepost_low, low_count_acquisition, shock_intensity = low_string)
high_figure <- linechart_count_and_titles(linechart_prepost_high, high_count_acquisition, shock_intensity = high_string)

#Have to think of how to add the n's
combined_linechart_prepost<- ggarrange(low_figure, high_figure, ncol=2, nrow=1, labels = "auto", common.legend = FALSE, legend="bottom")
combined_linechart_prepost <- annotate_figure(combined_linechart_prepost, left = textGrob(y_title, rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
combined_linechart_prepost
ggsave("combined_linechart_prepost.png", plot =combined_linechart_prepost, path = "./Combined/")


#### Recall --------------------------------------------------------------------
filter_2min_recall_function <- function(dataset){
  df_2minext <- dataset %>%
    select(all_of(factor_cols), "recall_1") %>%
    na.omit() %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex))) %>%
    mutate(figures_stress = fct_reorder(Stress, desc(Stress))) %>%
    unite(sex_stress, c(Sex, Stress), remove=FALSE) %>%
    mutate(fig_sex_stress = factor(sex_stress, levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS")))
  return(df_2minext)
}
df_2minext_low <- filter_2min_recall_function(complete_ds_low)
df_2minext_high <- filter_2min_recall_function(complete_ds_high)


two_minute_descriptives_function <- function(dataset){
  
  two_minute_descriptives <- dataset %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
    group_by(figures_sex, Stress)%>%
    summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))
  #write.csv(two_minute_descriptives, file = "./High/Descriptives/two_minute_descriptives.csv")
  
}
low_two_minute_descriptives <- two_minute_descriptives_function(df_2minext_low)
high_two_minute_descriptives <- two_minute_descriptives_function(df_2minext_high)

two_minute_extinction_low_count <- count_simple_function(df_2minext_low)
two_minute_extinction_high_count <- count_simple_function(df_2minext_high)



# Is this needed? 
# two_minute_indivpoints <- two_minute_extinction %>%
#   mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
#   unite(sex_stress, c(Sex, Stress), remove=FALSE)
# two_minute_indivpoints$sex_stress <- factor(two_minute_indivpoints$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

recall_results <- two_minute_descriptives %>%
  unite(sex_stress, c(figures_sex, Stress), remove = FALSE)
recall_results$sex_stress <- factor(recall_results$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS","Female_ELS"))


generate_recall_2min_figure <- function(dataset, shock_intensity = low_string){
  
  #generate count for the figures
  two_minute_recall_count <- count_simple_function(dataset)

  ext2min_figure <- ggplot(dataset, aes(x = fig_sex_stress, y = recall_1,  fill = fig_sex_stress))
  ext2min_figure <- ext2min_figure +
    geom_boxplot()+
    #stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
    #stat_summary(fun.data = mean_se, geom = "errorbar")+
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
    geom_point(aes(x = fig_sex_stress, y = recall_1), position =
                 position_jitterdodge(jitter.width = 0.4, jitter.height=0.1,
                                      dodge.width=0.9), alpha = 0.6)+
    stat_summary(aes(x = fig_sex_stress, y = recall_1), fun = mean, geom = "point", shape = 21, size = 4, fill = "#FF00FF", color = "black", position = position_dodge(width = 0.9))

  # add n's to figure
  # n loop
  count_2min = list()
  for (i in two_minute_recall_count$n)
  {
    count_2min <- append(count_2min,i)
  }

  ext2min_figure <- ext2min_figure + scale_x_discrete(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", count_2min[3]), "Male_NS" =  sprintf("Male NS\n n = %s", count_2min[4]), "Female_ELS" = sprintf("Female ELS\n n = %s", count_2min[1]), "Female_NS" =  sprintf("Female NS\n n = %s", count_2min[2])))
  ext2min_figure <-ext2min_figure + scale_fill_manual(labels =c("Male NS","Male ELS", "Female NS", "Female ELS"),values= c("#b3b4ff","#1c20fc", "#ffc182", "#ff870f"))
  ext2min_figure <-ext2min_figure + labs(title= shock_intensity, x = NULL, y = NULL, fill = NULL)
  ext2min_figure <-ext2min_figure + blank_figure_theme

  return(ext2min_figure)
}
ext_2min_figure_low <- generate_recall_2min_figure(df_2minext_low, shock_intensity = low_string)
ext_2min_figure_high <- generate_recall_2min_figure(df_2minext_high, shock_intensity = high_string)

# Show the figures side by side with the themes applied
combined_2min_bar <- ggarrange(ext_2min_figure_low, ext_2min_figure_high, ncol=2, nrow=1, labels = "auto", common.legend = TRUE, legend="bottom")
combined_2min_bar <- combined_2min_bar <- annotate_figure_function(figure = combined_2min_bar, title_text = NULL)
combined_2min_bar <- annotate_figure(combined_2min_bar, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_2min_bar
ggsave("combined_barchart_2min.png", plot =combined_2min_bar, path = "./Combined/")


# TODO remove this
#inferentials
#Low
recall_lm_low <- lm(data = df_2minext_low, recall_1 ~ Sex*Stress)
low_recall_lm_aov <- aov(recall_lm_low)
summary(low_recall_lm_aov)

#High
recall_lm_high <- lm(data = df_2minext_high, recall_1 ~ Sex*Stress)
high_recall_lm_aov <- aov(recall_lm_high)
summary(high_recall_lm_aov)

#testing 0 responders
low_2min_0responders <- df_2minext_low %>%
  group_by(Sex,Stress) %>%
  filter(recall_1 == 0) %>%
  count()

low_2min <- df_2minext_low %>%
  group_by(Sex,Stress) %>%
  count()

high_2min_0responders <- df_2minext_high %>%
  group_by(Sex,Stress) %>%
  filter(recall_1 == 0) %>%
  count()
high_2min <- df_2minext_high %>%
  group_by(Sex,Stress) %>%
  count()


#### Extinction ##################################################

extinction_descriptives <- function(dataset){
  
  # Without removing the na values the figure wont be able to be generated
  dataset <- dataset %>% 
    na.omit()
  
  extinction_long_descriptives <- dataset %>%
    unite(sex_stress, c(Sex, Stress), remove=TRUE)%>%
    group_by(sex_stress) %>%
    summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
              sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve))) %>%
    pivot_longer(cols = c(mean_tencurve1, mean_tencurve2, mean_tencurve3, mean_tencurve4, mean_tencurve5, sem_tencurve1, sem_tencurve2, sem_tencurve3, sem_tencurve4, sem_tencurve5),
                 names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)")
  
  # Make sure these stay as a factor
  extinction_long_descriptives$sex_stress <- as.factor(extinction_long_descriptives$sex_stress)
  extinction_long_descriptives$timepoint <- as.factor(extinction_long_descriptives$timepoint)
  return (extinction_long_descriptives)
}
extinction_descriptives_low <- extinction_descriptives(low_extinction)
extinction_descriptives_high <- extinction_descriptives(high_extinction)


#count
# Note have to remove the na values 

low_extinction_wo_na <- low_extinction %>% 
  na.omit()
high_extinction_wo_na <- high_extinction %>% 
  na.omit()


low_count_extinction <- count_n(low_extinction_wo_na,type = "sexstress")
high_count_extinction <- count_n(high_extinction_wo_na,type = "sexstress")

extinction_figure <- function(dataset, intensity, count_dataset){
  extinction_curve_figure <- ggplot(dataset, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) +
    geom_line() +
    geom_point() +
    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(breaks=seq(0,100,25), expand = c(0,0))+
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)
  
  # Count values for each group
  male_els <- count_dataset %>% filter(Sex == "Male", Stress == "ELS") %>% pull(n)
  male_ns <- count_dataset %>% filter(Sex == "Male", Stress == "NS") %>% pull(n)
  female_els <- count_dataset %>% filter(Sex == "Female", Stress == "ELS") %>% pull(n)
  female_ns <- count_dataset %>% filter(Sex == "Female", Stress == "NS") %>% pull(n)
  
  
  extinction_curve_figure <- extinction_curve_figure + scale_x_discrete(labels = c("tencurve1" = "2 mins", "tencurve2"= "4 mins", "tencurve3" = "6 mins", "tencurve4" = "8 mins", "tencurve5" = "10 mins"))
  extinction_curve_figure <- extinction_curve_figure + scale_color_manual(labels = c(
        "Male_ELS" = sprintf("Male ELS\n n = %s", male_els),
        "Male_NS" = sprintf("Male NS\n n = %s", male_ns),
        "Female_ELS" = sprintf("Female ELS\n n = %s", female_els),
        "Female_NS" = sprintf("Female NS\n n = %s", female_ns)
      ),values = orange_blue)
  if (intensity == "Low"){
    extinction_curve_figure <- extinction_curve_figure + labs(title="Low intensity shock", x = NULL, y = NULL, group = "Group", color = "Group")
  }
  else if (intensity == "High"){
    extinction_curve_figure <- extinction_curve_figure + labs(title="High intensity shock", x = NULL, y = NULL, group = "Group", color = "Group")
  }

  extinction_curve_figure <- extinction_curve_figure + blank_figure_theme
  extinction_curve_figure <- extinction_curve_figure + theme(legend.position = c(0.8, 0.7))
  return (extinction_curve_figure)
}

extinction_figure_low <- extinction_figure(extinction_descriptives_low, intensity = "Low", count_dataset = low_count_extinction)
extinction_figure_high <- extinction_figure(extinction_descriptives_high, intensity = "High", count_dataset = high_count_extinction)

#Combined
combined_extinction_figure <- ggarrange(extinction_figure_low, extinction_figure_high, ncol=2, nrow=1, labels = "auto", common.legend = FALSE)
combined_extinction_figure <- combined_extinction_figure <- annotate_figure_function(figure = combined_extinction_figure, title_text = NULL)
combined_extinction_figure <- annotate_figure(combined_extinction_figure, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_extinction_figure



#### Extinction recall----------------------------------------------------------

prepare_extinction_recall_data <- function(data) {
  data <- data %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex))) %>%
    unite(sex_stress, c(Sex, Stress), remove = FALSE) %>%
    unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)
  
}

low_extinction_recall_data <- prepare_extinction_recall_data(low_extinction_recall)
high_extinction_recall_data <- prepare_extinction_recall_data(high_extinction_recall)

low_extinction_recall_count <- count_n(low_extinction_recall_data, type = "allfactors")
high_extinction_recall_count <- count_n(high_extinction_recall_data, type = "allfactors")


three_factor_barplot_figure <- function(dataset, intensity = "Low", timepoint = "Extinction_recall"){
  extinction_recall_figure <- ggplot(dataset, aes(x = sex_stress, y = extinction_recall,  fill = Condition))
  
  extinction_recall_count <- count_n(dataset,type = "allfactors")
  
  # 2 min 
  male_els_2 <- extinction_recall_count %>% filter(Sex == "Male", Stress == "ELS", Condition == "2") %>% pull(n)
  male_ns_2 <- extinction_recall_count %>% filter(Sex == "Male", Stress == "NS", Condition == "2") %>% pull(n)
  female_els_2 <- extinction_recall_count %>% filter(Sex == "Female", Stress == "ELS", Condition == "2") %>% pull(n)
  female_ns_2 <- extinction_recall_count %>% filter(Sex == "Female", Stress == "NS", Condition == "2") %>% pull(n)
  
  # 10 min 
  male_els_10 <- extinction_recall_count %>% filter(Sex == "Male", Stress == "ELS", Condition == "10") %>% pull(n)
  male_ns_10 <- extinction_recall_count %>% filter(Sex == "Male", Stress == "NS", Condition == "10") %>% pull(n)
  female_els_10 <- extinction_recall_count %>% filter(Sex == "Female", Stress == "ELS", Condition == "10") %>% pull(n)
  female_ns_10 <- extinction_recall_count %>% filter(Sex == "Female", Stress == "NS", Condition == "10") %>% pull(n)
  
  
  extinction_recall_figure <- extinction_recall_figure +
    geom_boxplot(outlier.shape = NA)+
    scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
    coord_cartesian(ylim = c(0, 100)) +
    geom_point(aes(x = sex_stress, y = extinction_recall), position =
                 position_jitterdodge(jitter.width = 0.4, jitter.height=0.1,
                                      dodge.width=0.9), alpha = 0.6, set.seed(42)) +
    facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
    stat_summary(aes(x = sex_stress, y = extinction_recall, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "#FF00FF", color = "black", position = position_dodge(width = 0.9))
  
  # Count
  # Define each label with the correct format
  extinction_recall_figure <- extinction_recall_figure + 
    scale_x_discrete(
      labels = c(
        "Male_ELS" = sprintf("ELS\n n = %s        n = %s", male_els_2, male_els_10),
        "Male_NS" = sprintf("NS\n n = %s        n = %s", male_ns_2, male_ns_10),
        "Female_ELS" = sprintf("ELS\n n = %s        n = %s", female_els_2, female_els_10),
        "Female_NS" = sprintf("NS\n n = %s        n = %s", female_ns_2, female_ns_10)
      )
    )
  extinction_recall_figure <- extinction_recall_figure + scale_fill_manual(values = extinction_colors)
  if (intensity == "Low"){
    extinction_recall_figure <- extinction_recall_figure + labs(title = "Low intensity shock", x = NULL, y = NULL, fill = "Condition")
 
  }
  else if (intensity == "High"){
    extinction_recall_figure <- extinction_recall_figure + labs(title = "High intensity shock", x = NULL, y = NULL, fill = "Condition")
    
  }
    extinction_recall_figure <- extinction_recall_figure + blank_figure_theme
  
  
  return (extinction_recall_figure)
}

extinction_recall_low_figure <- three_factor_barplot_figure(low_extinction_recall_data, intensity = "Low", timepoint = "Extinction_recall")
extinction_recall_high_figure <- three_factor_barplot_figure(high_extinction_recall_data, intensity = "High", timepoint = "Extinction_recall")


combined_extinction_recall <- ggarrange(extinction_recall_low_figure, extinction_recall_high_figure, ncol=2, nrow=1, labels = "auto", common.legend = TRUE,legend = "bottom")
combined_extinction_recall <- combined_extinction_recall <- annotate_figure_function(figure = combined_extinction_recall, title_text = NULL)
combined_extinction_recall <- annotate_figure(combined_extinction_recall, left = textGrob(y_title, rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_extinction_recall

ggsave("Extinction_recall_chart.png", plot = combined_extinction_recall, path = "./combined")


#### Reminder Shock-------------------------------------------------------------
# As this figure will be very similar to the extinction recall figuire I think I will adjust that function and have an argument to change the title,
# The dataset will be different but the presentation will be exactly the same for Extinction recall, reminder shock and reminder recall
reminder_shock_figure <- function(database){
  #' here database is either reminder_shock_low or reminder_shock_high
  #' 
  reminder_shock_descr <- database %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
    unite(sex_stress, c(Sex, Stress), remove=FALSE) %>%
    unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)
  
  reminder_shock_indivpoints <- database %>%
    drop_na() %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
    unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>%
    unite(sex_stress, c(Sex, Stress), remove = FALSE)
  
  reminder_shock_descr$sex_stress <- as.factor(reminder_shock_descr$sex_stress)
  reminder_shock_descr$reminder_day1_shock <- as.numeric(reminder_shock_descr$reminder_day1_shock)
  reminder_shock_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_indivpoints$reminder_day1_shock)
  reminder_shock_descr$sex_stress <- factor(reminder_shock_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))
  
  #reminder_shock_descr <- reminder_shock_descr %>%
  #  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>%
  #  summarize(mean_reminder_shock_low = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock_low = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
  
  # to find the n's for each group
  reminder_shock_count <- database %>%
    drop_na() %>% 
    group_by(Sex,Stress,Condition) %>%
    count()
  print(reminder_shock_indivpoints)
  
  reminder_shock_indivpoints$sex_stress <- factor(reminder_shock_indivpoints$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))
  reminder_shock_indivpoints$sex_stress <- factor(reminder_shock_indivpoints$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))
  
  
  
  bar_chart_reminder_shock <- ggplot(reminder_shock_indivpoints, aes(x = sex_stress, y = reminder_day1_shock,  fill = Condition))
  bar_chart_reminder_shock <- bar_chart_reminder_shock +
    geom_boxplot(outlier.shape = NA) +  # Remove outliers
    #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
    scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
    coord_cartesian(ylim = c(0, 100)) +
    geom_point(aes(x = sex_stress, y = reminder_day1_shock), position =
                 position_jitterdodge(jitter.width = 0.4, jitter.height=0.1,
                                      dodge.width=0.9), alpha = 0.6, set.seed(42)) +
    #scale_fill_manual(values = c('black','black'))+
    facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
    stat_summary(aes(x = sex_stress, y = reminder_day1_shock, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "#FF00FF", color = "black", position = position_dodge(width = 0.9))
  
  
  bar_chart_reminder_shock <- bar_chart_reminder_shock + scale_x_discrete(labels = c("Male_ELS" = sprintf("ELS\n n = %s        n = %s", reminder_shock_count[5,4],reminder_shock_count[6,4]), "Male_NS" = sprintf( "NS\n n = %s        n = %s", reminder_shock_count[7,4],reminder_shock_count[8,4]),"Female_ELS" = sprintf("ELS\n n = %s        n = %s", reminder_shock_count[1,4],reminder_shock_count[2,4]), "Female_NS"= sprintf( "NS\n n = %s        n = %s", reminder_shock_count[3,4],reminder_shock_count[4,4])))
  bar_chart_reminder_shock <- bar_chart_reminder_shock + scale_fill_manual(values = extinction_colors)
  bar_chart_reminder_shock <- bar_chart_reminder_shock + blank_figure_theme + labs(y=NULL)
  return(bar_chart_reminder_shock)
}
low_reminder_shock_figure <- reminder_shock_figure(low_reminder_shock)
high_reminder_shock_figure <- reminder_shock_figure(high_reminder_shock)
low_reminder_shock_figure
high_reminder_shock_figure
ggsave("reminder_shock_high.png", plot = bar_chart_reminder_shock_high, path = "./High/Figures/")

combined_reminder_shock <- ggarrange(low_reminder_shock_figure, high_reminder_shock_figure, ncol=2, nrow=1, labels = "auto", common.legend = TRUE,legend = "bottom")
combined_reminder_shock <- combined_reminder_shock <- annotate_figure_function(figure = combined_reminder_shock, title_text = NULL)
combined_reminder_shock <- annotate_figure(combined_reminder_shock, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_reminder_shock


#### Reminder recall ##########################################################
descriptives_function <- function(dataset){
  reminder_recall_descr <- dataset %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
    unite(sex_stress, c(Sex, Stress), remove=FALSE) %>%
    unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)
  return(reminder_recall_descr)
} 

individual_points_function <- function(dataset){
  reminder_recall_indivpoints <- dataset %>%
    drop_na() %>%
    mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
    unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>%
    unite(sex_stress, c(Sex, Stress), remove = FALSE)
  return(reminder_recall_low_indivpoints)
}

reminder_recall_figure <- function(figure, shock_intensity, count_dataset = dataset){
  bar_chart_reminder_recall<- figure +
    geom_boxplot()+
    #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
    scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
    coord_cartesian(ylim = c(0, 100)) +
    geom_point(aes(x = sex_stress, y = reminder_day2), position =
                 position_jitterdodge(jitter.width = 0.4, jitter.height=0.1,
                                      dodge.width=0.9), alpha = 0.6, set.seed(42)) +
    #scale_fill_manual(values = c('black','black'))+
    facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
    stat_summary(aes(x = sex_stress, y = reminder_day2, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "#FF00FF", color = "black", position = position_dodge(width = 0.9))
  
  bar_chart_reminder_recall <- bar_chart_reminder_recall + scale_x_discrete(labels = c("Male_ELS" = sprintf("ELS\n n = %s        n = %s", count_dataset[5,3],count_dataset[6,3]), "Male_NS" = sprintf( "NS\n n = %s        n = %s", count_dataset[7,3],count_dataset[8,3]),"Female_ELS" = sprintf("ELS\n n = %s        n = %s", count_dataset[1,3],count_dataset[2,3]), "Female_NS"= sprintf( "NS\n n = %s        n = %s", count_dataset[3,3],count_dataset[4,3])))
  bar_chart_reminder_recall <- bar_chart_reminder_recall + scale_fill_manual(values = extinction_colors)
  bar_chart_reminder_recall <- bar_chart_reminder_recall + labs(title = shock_intensity, x = NULL, y = NULL, fill = "Condition")
  bar_chart_reminder_recall <- bar_chart_reminder_recall + blank_figure_theme
  return(bar_chart_reminder_recall)
}

combined_reminder_recall_function <- function(dataset, shock_intensity = low_string){
  #' dataset here is reminder_recall_low or reminder_recall_high
  reminder_recall_descr <- descriptives_function(dataset)
  
  reminder_recall_indivpoints <- individual_points_function(dataset)
  
  reminder_recall_descr$sex_stress <- as.factor(reminder_recall_descr$sex_stress)
  reminder_recall_descr$sex_stress <- factor(reminder_recall_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))
  
  reminder_recall_descr <- reminder_recall_descr %>%
    group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>%
    summarize(mean_reminder_recall_high = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall_high = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
  
  reminder_recall_count <- dataset %>%
    unite(sex_stress, c(Sex, Stress), remove = FALSE) %>%
    group_by(sex_stress,Condition) %>%
    count()
  
  
  reminder_recall_indivpoints$sex_stress <- factor(reminder_recall_indivpoints$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))
  
  bar_chart_reminder_recall <- ggplot(reminder_recall_indivpoints, aes(x = sex_stress, y = reminder_day2,  fill = Condition))
  
  bar_chart_reminder_recall <- reminder_recall_figure(figure = bar_chart_reminder_recall, shock_intensity, count_dataset = reminder_recall_count)
  return(bar_chart_reminder_recall)
  
  }

low_reminder_recall_figure <- combined_reminder_recall_function(reminder_recall_low, shock_intensity = low_string)
high_reminder_recall_figure <- combined_reminder_recall_function(reminder_recall_high, shock_intensity = high_string)

combined_reminder_recall <- ggarrange(low_reminder_recall_figure, high_reminder_recall_figure, ncol=2, nrow=1, common.legend = TRUE,legend = "bottom")
combined_reminder_recall <- combined_reminder_recall <- annotate_figure_function(figure = combined_reminder_recall, title_text = NULL)
combined_reminder_recall <- annotate_figure(combined_reminder_recall, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_reminder_recall

#### Recall and extinction split -----------------------------------------------
# try spliting the High intensity shock dataset into 2 across the 2 minute and 10 minute group
complete_ds_2 <- complete_ds %>%
  filter(Condition == "2")
write.csv(complete_ds_2, "./Datasets/completeds2.csv")

complete_ds_2_low <- complete_ds_2 %>% 
  filter(Shock == "l")

complete_ds_2_high <- complete_ds_2 %>% 
  filter(Shock == "h")


complete_ds_10 <- complete_ds %>%
  filter(Condition == "10")
write.csv(complete_ds_10, "./Datasets/completeds10.csv")

complete_ds_10_low <- complete_ds_10 %>% 
  filter(Shock == "l")

complete_ds_10_high <- complete_ds_10 %>% 
  filter(Shock == "h")


# write.csv(two_minute_recall_seperated_descriptives, "./Datasets/twoMinuteRecall.csv")
# write.csv(two_minute_extinctionRecall_seperated_descriptives, "./Datasets/twoMinuteExtinctionRecall.csv")
# write.csv(two_minute_reminderShock_seperated_descriptives, "./Datasets/twoMinuteReminderShock.csv")
# write.csv(two_minute_reminderRecall_seperated_descriptives, "./Datasets/twoMinuteReminderRecall.csv")

# two_minute_combined_descriptives <- read.csv("./Datasets/twoMinuteCombinedDescriptives.csv")


two_minute_combined_descriptives <- two_minute_combined_descriptives %>%
  unite(sex_stress, c(figures_sex, Stress), remove=FALSE)

factor_cols <- c("Timepoint", "sex_stress")
two_minute_combined_descriptives[factor_cols] <- lapply(two_minute_combined_descriptives[factor_cols], factor)

two_minute_combined_descriptives$Timepoint <- two_minute_combined_descriptives$Timepoint %>%
  factor(levels = c("Recall1", "ExtinctionRecall", "ReminderShock", "Reminder_Recall"))


longplot2 <- ggplot(two_minute_combined_descriptives, aes(x = Timepoint, y = mean_recall, group = sex_stress, color =sex_stress))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2, alpha = 0.7)+
  ylim(0,80)

longplot2 <- longplot2 + scale_color_manual(labels = c( "Male_ELS" = "Male ELS",  "Male_NS" = "Male non-stressed" , "Female_ELS" = "Female ELS",  "Female_NS"= "Female non-stressed"),values=c(Male_ELS = "#1c20fc", Male_NS = "#b3b4ff", Female_ELS = "#ff870f", Female_NS ="#ffc182"))
longplot2 <- longplot2 + scale_x_discrete(labels = c("Recall1" = "(Shock recall)", "ExtinctionRecall"= "Extinction recall", "ReminderShock" = "Reminder Shock", "Reminder_Recall" = "Reminder Recall"))
longplot2 <- longplot2 + labs(x = "Timepoint", y = "Freezing percentage", color = "Condition", title = "2 minute group")
#longplot2 <- longplot2 + theme(legend.position = "none")
longplot2

ggsave("Longplot2.png", plot =longplot2, path = "./High/Figures/", width = 950, height = 732, unit = "px")

# now for the 10 minute group
ten_minute_extinction_descriptives <- complete_ds_10 %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>%
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))


ten_minute_extinctionRecall_seperated_descriptives <- complete_ds_10 %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>%
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recallext1 = mean(ext1_curve, na.rm = T), sem_recallext1 = sd(ext1_curve, na.rm = T)/sqrt(length(ext1_curve)),
            mean_recallext2 = mean(ext2_curve, na.rm = T), sem_recallext2 = sd(ext2_curve, na.rm = T)/sqrt(length(ext2_curve)),
            mean_recallext3 = mean(ext3_curve, na.rm = T), sem_recallext3 = sd(ext3_curve, na.rm = T)/sqrt(length(ext3_curve)),
            mean_recallext4 = mean(ext4_curve, na.rm = T), sem_recallext4 = sd(ext4_curve, na.rm = T)/sqrt(length(ext4_curve)),
            mean_recallext5 = mean(ext5_curve, na.rm = T), sem_recallext5 = sd(ext5_curve, na.rm = T)/sqrt(length(ext5_curve)))

ten_minute_reminderShock_seperated_descriptives <- complete_ds_10 %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>%
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(reminder_day1_shock, na.rm = T), sem_recall = sd(reminder_day1_shock, na.rm = T)/sqrt(length(reminder_day1_shock)))

ten_minute_reminderRecall_seperated_descriptives <- complete_ds_10 %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>%
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(reminder_day2, na.rm = T), sem_recall = sd(reminder_day2, na.rm = T)/sqrt(length(reminder_day2)))

write.csv(ten_minute_extinction_descriptives, "./Datasets/tenMinuteRecall.csv")
write.csv(ten_minute_extinctionRecall_seperated_descriptives, "./Datasets/ten_minute_extinctionRecall_seperated_descriptives.csv")
write.csv(ten_minute_reminderShock_seperated_descriptives, "./Datasets/tenMinuteReminderShock.csv")
write.csv(ten_minute_reminderRecall_seperated_descriptives, "./Datasets/tenMinuteReminderRecall.csv")

#where is this coming from?
ten_minute_combined_descriptives <- read.csv("./Datasets/tenMinuteCombinedDataset.csv")


ten_minute_combined_descriptives <- ten_minute_combined_descriptives %>%
  unite(sex_stress, c(figures_sex, Stress), remove=FALSE)

factor_cols <- c("Timepoint", "sex_stress")
ten_minute_combined_descriptives[factor_cols] <- lapply(ten_minute_combined_descriptives[factor_cols], factor)

ten_minute_combined_descriptives$Timepoint <- ten_minute_combined_descriptives$Timepoint %>%
  factor(levels = c("Recall1", "Ext2", "Ext3", "Ext4", "Ext5", "ReminderShock", "Reminder_Recall"))

ten_minute_combined_descriptives <- na.omit(ten_minute_combined_descriptives)

longplot10full <- ggplot(ten_minute_combined_descriptives, aes(x = Timepoint, y = mean_recall, group = sex_stress, color =sex_stress))+
  geom_line(size = 0.8)+
  #I dont think this is right!
  geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2, alpha = 0.7)+
  ylim(0,80)

longplot10full <- longplot10full + scale_color_manual(labels = c( "Male_ELS" = "Male ELS",  "Male_NS" = "Male non-stressed" , "Female_ELS" = "Female ELS",  "Female_NS"= "Female non-stressed"),values=c(Male_ELS = "#1c20fc", Male_NS = "#b3b4ff", Female_ELS = "#ff870f", Female_NS ="#ffc182"))
longplot10full <- longplot10full + scale_x_discrete(labels = c("Recall1" = "LTM1 \n (Shock recall)", "ExtinctionRecall"= "LTM2 \n (Extinction recall)", "ReminderShock" = "LTM3 \n (Long delay with reminder end)", "Reminder_Recall" = "LTM4 \n (Reminder Recall)"))
longplot10full <- longplot10full + labs(x = "Timepoint", y = "Freezing percentage", color = "Condition", title = "10 minute group")
longplot10full


#The figure without the long extinction timepoint
ten_minute_combined_descriptives_short <- read.csv("./Datasets/tenMinuteCombinedDataset_shorter.csv")

ten_minute_combined_descriptives_short <- ten_minute_combined_descriptives_short %>%
  unite(sex_stress, c(figures_sex, Stress), remove=FALSE)

factor_cols <- c("Timepoint", "sex_stress")
ten_minute_combined_descriptives_short[factor_cols] <- lapply(ten_minute_combined_descriptives_short[factor_cols], factor)

ten_minute_combined_descriptives_short$Timepoint <- ten_minute_combined_descriptives_short$Timepoint %>%
  factor(levels = c("Recall1", "Ext1", "ReminderShock", "Reminder_Recall"))


longplot10 <- ggplot(ten_minute_combined_descriptives_short, aes(x = Timepoint, y = mean_recall, group = sex_stress, color =sex_stress))+
  geom_line(size = 0.8)+
  geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2, alpha = 0.7)+
  ylim(0,80)

longplot10 <- longplot10 + scale_color_manual(labels = c( "Male_ELS" = "Male ELS",  "Male_NS" = "Male non-stressed" , "Female_ELS" = "Female ELS",  "Female_NS"= "Female non-stressed"),values=c(Male_ELS = "#1c20fc", Male_NS = "#b3b4ff", Female_ELS = "#ff870f", Female_NS ="#ffc182"))
longplot10 <- longplot10 + scale_x_discrete(labels = c("Recall1" = "Shock recall", "Ext1"= "Extinction recall", "ReminderShock" = "LTM2", "Reminder_Recall" = "Reminder Recall"))
longplot10



plot_grid(longplot2, longplot10full, labels = "AUTO", rel_widths = c(1,1))

#### Kerries suggested figures -------------------------------------------------
# After discussion with my supervisor Kerrie Thomas 25/08/23, it was agreed to present longitudinal figures. 
# This shows the freezing, post acquisition, over time. 
# Functions below are to generate these figures. 

# For longitudinal data need to filter for 2 and 10
complete_ds_2_low <- complete_ds_low %>% 
  filter(Condition == "2")

complete_ds_2_high <- complete_ds_high %>% 
  filter(Condition == "2")

complete_ds_10_low <- complete_ds_low %>% 
  filter(Condition == "10")

complete_ds_10_high <- complete_ds_high %>% 
  filter(Condition == "10")

#' Compute Descriptives
#'
#' This function calculates mean and SEM values for shock recall, extinction recall,
#' reminder test, and reminder recall based on sex and stress groupings.
#'
#' @param dataset Data frame containing recall measurements and relevant grouping variables.
#' @return A data frame with mean and SEM values for each measurement and group.
calculate_descriptives <- function(dataset) {
  dataset %>%
    select(Stress, Sex, recall_1, c(13:15)) %>% 
    mutate(figures_sex = fct_reorder(Sex, desc(Sex))) %>%
    unite(sex_stress, c(figures_sex, Stress), remove = FALSE) %>% 
    group_by(sex_stress) %>% 
    summarize(
      mean_shockrecall = mean(recall_1, na.rm = TRUE), 
      sem_shockrecall = sd(recall_1, na.rm = TRUE) / sqrt(length(recall_1)),
      mean_extrecall = mean(extinction_recall, na.rm = TRUE), 
      sem_extrecall = sd(extinction_recall, na.rm = TRUE) / sqrt(length(extinction_recall)),
      mean_remtest = mean(reminder_day1_shock, na.rm = TRUE), 
      sem_remtest = sd(reminder_day1_shock, na.rm = TRUE) / sqrt(length(reminder_day1_shock)),
      mean_remrecall = mean(reminder_day2, na.rm = TRUE), 
      sem_remrecall = sd(reminder_day2, na.rm = TRUE) / sqrt(length(reminder_day2))
    )
}

#' Format Descriptives Data
#'
#' Reshapes and formats the descriptive statistics data for plotting.
#'
#' @param descriptives Data frame of calculated descriptives.
#' @return A formatted data frame suitable for plotting.
format_descriptives <- function(descriptives) {
  descriptives %>%
    pivot_longer(
      cols = c(mean_shockrecall, mean_extrecall, mean_remtest, mean_remrecall,
               sem_shockrecall, sem_extrecall, sem_remtest, sem_remrecall),
      names_to = c(".value", "Timepoint"), names_pattern = "(.*?)_(.*)"
    ) %>%
    mutate(
      fig_timepoint = factor(Timepoint, levels = c("shockrecall", "extrecall", "remtest", "remrecall"))
    ) %>%
    rename(mean_values = mean, sem_values = sem)
}


#' Create Longplot
#'
#' Generates the ggplot object for the plot based on the formatted data.
#'
#' @param data Data frame with mean values, SEM values, and grouping variables.
#' @return A ggplot object for the longplot.
create_longplot <- function(data) {
  ggplot(data, aes(x = Timepoint, y = mean_values, group = sex_stress, color = sex_stress)) +
    geom_line(size = 0.8) +
    geom_errorbar(aes(ymin = mean_values - sem_values, ymax = mean_values + sem_values), width = .2, alpha = 0.7) +
    ylim(0, 100) +
    blank_figure_theme
}

#' Apply Labels
#'
#' Applies custom labels and colors based on whether labeling is enabled.
#'
#' @param plot The ggplot object to modify.
#' @param isLabeled Logical, if TRUE adds labels to the plot, otherwise hides labels.
#' @return A ggplot object with the appropriate labeling applied.
apply_labels <- function(plot, isLabeled) {
  plot <- plot + scale_color_manual(
    labels = c("Male_ELS" = "Male ELS", "Male_NS" = "Male non-stressed", 
               "Female_ELS" = "Female ELS", "Female_NS" = "Female non-stressed"),
    values = c(Male_ELS = "#1c20fc", Male_NS = "#b3b4ff", 
               Female_ELS = "#ff870f", Female_NS = "#ffc182")
  )
  
  if (isLabeled) {
    plot + scale_x_discrete(labels = c(
      "shockrecall" = "Shock recall", "extrecall" = "Extinction recall",
      "remtest" = "Reminder test", "remrecall" = "Reminder Recall"))
  } else {
    plot + scale_x_discrete(labels = c(
      "shockrecall" = "CFC recall", "extrecall" = "Extinction recall",
      "remtest" = "Reminder test", "remrecall" = "Reminder recall")) +
      labs(x = NULL, y = NULL, fill = NULL, color = NULL) +
      theme(legend.position = "none")
  }
}

#' Long Figure Function
#'
#' Main function to generate a plot of recall statistics with optional labeling.
#'
#' @param dataset Data frame containing recall data.
#' @param isLabeled Logical, if TRUE adds labels to the plot, otherwise hides labels.
#' @return A ggplot object.
long_figure_function <- function(dataset, isLabeled = TRUE) {
  descriptives <- calculate_descriptives(dataset)
  formatted_data <- format_descriptives(descriptives)
  plot <- create_longplot(formatted_data)
  plot <- apply_labels(plot, isLabeled)
  return(plot)
}

low_2extcont_long_figure <- long_figure_function(complete_ds_2_low, isLabeled = FALSE)
high_2extcont_long_figure <- long_figure_function(complete_ds_2_high, isLabeled = FALSE)

low_10exttrain_long_figure <- long_figure_function(complete_ds_10_low, isLabeled = FALSE)
high_10exttrain_long_figure <- long_figure_function(complete_ds_10_high, isLabeled = FALSE)


combined_210_figure <- ggarrange(low_2extcont_long_figure, low_10exttrain_long_figure,
                                 high_2extcont_long_figure,  high_10exttrain_long_figure, 
          labels = c("Low shock,\n Extinction control", "Low shock, \n Extinction trained",
                     "High shock, \n Extinction control", "High shock, \n Extinction trained"),
          ncol = 2, nrow = 2)

combined_210_figure
#### Recall reminder shock and reminder recall #####################################

complete_ds_low_combined_recall_remshock_remrecall <- complete_ds_low %>% 
  select(c("Stress","Sex", "Condition", "extinction_recall", "reminder_day1_shock", "reminder_day2")) 
complete_ds_low_combined_recall_remshock_remrecall_noNA <- complete_ds_low_combined_recall_remshock_remrecall[complete.cases(complete_ds_low_combined_recall_remshock_remrecall), ]


complete_ds_high_combined_recall_remshock_remrecall <- complete_ds_high %>% 
  select(c("Stress","Sex", "Condition", "extinction_recall", "reminder_day1_shock", "reminder_day2")) 
complete_ds_high_combined_recall_remshock_remrecall_noNA <- complete_ds_high_combined_recall_remshock_remrecall[complete.cases(complete_ds_high_combined_recall_remshock_remrecall), ]

filter_two_and_ten_dataset_function <- function(combined_two_and_ten_dataset){
  # Takes in a dataset which has the two different extinction conditions and returns a list 
  # containing the two dataframes
  
  two_dataset <- combined_two_and_ten_dataset %>% 
    filter(Condition == 2)
  ten_dataset <- combined_two_and_ten_dataset %>% 
    filter(Condition == 10)
  
  return(list(two_dataset, ten_dataset))
}
generate_datasets_for_2_10_figures <- function (wide_dataset_noNA){
 
  combined_recall_remshock_remrecall_descr <- wide_dataset_noNA %>%
    unite("sex_stress", c(Sex, Stress)) %>% 
    group_by(sex_stress) %>% 
    summarise(mean_recall = mean(extinction_recall, na.rm = T), sem_recall = sd(extinction_recall, na.rm = T)/sqrt(length(extinction_recall)),
              mean_remshock = mean(reminder_day1_shock, na.rm = T), sem_remshock = sd(reminder_day1_shock, na.rm = T)/sqrt(length(reminder_day1_shock)),
              mean_remrecall = mean(reminder_day2, na.rm = T), sem_remrecall = sd(reminder_day2, na.rm = T)/sqrt(length(reminder_day2)),
    )
  
  combined_recall_remshock_remrecall_descr_long <- combined_recall_remshock_remrecall_descr %>% 
    pivot_longer(cols = c(mean_recall, mean_remshock, mean_remrecall, sem_recall, sem_remshock, sem_remrecall),
                 names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)") %>% 
    mutate(fig_timepoint = factor(timepoint, levels = c("recall", "remshock", "remrecall")))
  
  combined_recall_remshock_remrecall_descr_long$subject <- rep(1:nrow(combined_recall_remshock_remrecall_descr), each = 3)
  combined_recall_remshock_remrecall_descr_long$timepoint <- as.factor(combined_recall_remshock_remrecall_descr_long$timepoint)
  return(combined_recall_remshock_remrecall_descr_long)
}
figure_build_extctionrecall_reminder_remrecall_figure_function <- function(first_dataset, isCombined = TRUE){
 descriptives_dataset <-  generate_datasets_for_2_10_figures(first_dataset)

 count_ds <- first_dataset %>%
   group_by(Sex, Stress) %>%
   count()

recall_reminder_figure_combined <- ggplot(data = descriptives_dataset, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)+
  #TODO make this the limit of the mean + SEM + around 10 more
  coord_cartesian(ylim = c(0, 60)) 

recall_reminder_figure_combined <- recall_reminder_figure_combined + scale_x_discrete(labels = c("recall" = "Extinction recall", "remrecall" = "Reminder session", "remshock" = "Reminder recall"))
recall_reminder_figure_combined <- recall_reminder_figure_combined + scale_colour_manual( values = c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"), labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", count_ds[3,3]), "Male_NS" =  sprintf("Male NS\n n = %s", count_ds[4,3]), "Female_ELS" = sprintf("Female ELS\n n = %s", count_ds[1,3]), "Female_NS" =  sprintf("Female NS\n n = %s", count_ds[2,3])))
#recall_reminder_figure_combined <- recall_reminder_figure_combined + scale_color_manual(labels = c("Female_ELS" = "Female ELS", "Female_NS" = "Female non-stressed", "Male_ELS" = "Male ELS", "Male_NS" = "Male Non-stressed"), values = c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))
#ext2min_figure_high <- ext2min_figure_high + scale_group_discrete(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", count_ds[3,3]), "Male_NS" =  sprintf("Male NS\n n = %s", count_ds[4,3]), "Female_ELS" = sprintf("Female ELS\n n = %s", count_ds[1,3]), "Female_NS" =  sprintf("Female NS\n n = %s", count_ds[2,3])))


if (isCombined == TRUE) {
# Dont add an axis title to either graphs 
  recall_reminder_figure_combined <- recall_reminder_figure_combined + labs(x = NULL, y = NULL, group = "Group", color = "Group")
}
else {
  recall_reminder_figure_combined <- recall_reminder_figure_combined + labs(x = "Timepoint", y = "Freezing percentage", group = "Group", color = "Group")
  
}
recall_reminder_figure_combined <- recall_reminder_figure_combined + blank_figure_theme

return (recall_reminder_figure_combined)
}
generate_two_ten_grid_figure <- function(wide_dataset){
  
  #split the dataset into the two and ten
  filtered_datasets <- filter_two_and_ten_dataset_function(wide_dataset)
  two_dataset <- as.data.frame(filtered_datasets[1])
  ten_dataset <- as.data.frame(filtered_datasets[2])
  
  two_numbers = select(two_dataset, c('extinction_recall','reminder_day1_shock','reminder_day2'))
  ten_numbers = select(ten_dataset, c('extinction_recall','reminder_day1_shock','reminder_day2'))
  
  highest_number_in_y_axis <- max(max(two_numbers),max(ten_numbers))
  
  #generate the plots for both 
  two_plot <- figure_build_extctionrecall_reminder_remrecall_figure_function(two_dataset)
  ten_plot <- figure_build_extctionrecall_reminder_remrecall_figure_function(ten_dataset)
  
  combined_plot <- ggarrange(two_plot, ten_plot, ncol=2, nrow=1, labels = "auto", common.legend = FALSE,legend = "bottom")
  #combined_reminder_shock <- combined_reminder_shock <- annotate_figure_function(figure = combined_reminder_shock, title_text = NULL)
  combined_plot <- annotate_figure(combined_plot, left = textGrob("Freezing percentage", rot = 90, vjust = 0.5,hjust= 0.3, gp = gpar(cex = 1.3)))
  return(combined_plot)
  }




low_combined_recall_reminder_remrecall<- generate_two_ten_grid_figure(complete_ds_low_combined_recall_remshock_remrecall_noNA)
high_combined_recall_reminder_remrecall<- generate_two_ten_grid_figure(complete_ds_high_combined_recall_remshock_remrecall_noNA)

low_combined_recall_reminder_remrecall
high_combined_recall_reminder_remrecall
