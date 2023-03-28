library(MASS)
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

#Read in dataset and remove excel artifacts.
complete_ds <- read.csv("./Datasets/high_low_combined.csv", na.strings=c(".","#DIV/0!"))  
complete_ds[complete_ds == "#DIV/0!"] <- NA
#fix an issue with a space in the var 
complete_ds$Stress[complete_ds$Stress == " NS"] <- "NS"

#remove those which are x (the b cages)
complete_ds <- complete_ds %>% 
  filter(Sex != "x")

#Fix the structure change columns to factor or num
factor_cols <- c("Shock", "Stress", "Sex", "Condition")
num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", "ext3_curve", "ext4_curve", "ext5_curve", "extinction_recall", "reminder_day1_shock", "reminder_day2")
complete_ds[factor_cols] <- lapply(complete_ds[factor_cols], factor)  ## as.factor() could also be used
complete_ds[num_cols] <- lapply(complete_ds[num_cols], as.numeric)  # NA's due to 2 min group not having 10 min timepoints. 


complete_ds$Shock[complete_ds$Shock=='']=NA
complete_ds$Shock = droplevels(complete_ds$Shock) 
complete_ds$Stress[complete_ds$Stress=='']=NA
complete_ds$Stress = droplevels(complete_ds$Stress) 
complete_ds$Sex[complete_ds$Sex=='']=NA
complete_ds$Sex = droplevels(complete_ds$Sex) 
complete_ds$Condition[complete_ds$Condition=='']=NA
complete_ds$Condition = droplevels(complete_ds$Condition) 

complete_ds$Stress <- complete_ds$Stress %>% 
  fct_recode("NS" = " NS")

complete_ds$Sex <- complete_ds$Sex %>% 
  fct_recode("Female" = "F", "Male" = "M")

#This is the only way I know to reverse 2 and 10 as factor levels for the figures. 
complete_ds$Condition <- fct_rev(complete_ds$Condition)

str(complete_ds)

num_of_n <- complete_ds %>% 
  count()
n_split_by_sex <- complete_ds %>% 
  group_by(Sex) %>% 
  count()
nsplit_by_sex_stress <- complete_ds %>% 
  group_by(Sex, Stress) %>% 
  count()

complete_count <- complete_ds %>% 
  group_by(Sex, Stress, Condition, Shock) %>% 
  count()
##subset depending on shock filter for high low
complete_ds_high <- complete_ds %>% 
  filter(Shock == "h") 
complete_ds_high$Shock <- droplevels(complete_ds_high$Shock) #Removes the NA factor


high_shock_count <- complete_ds_high %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

complete_ds_low <- complete_ds %>% 
  filter(Shock == "l")
complete_ds_low$Shock <- droplevels(complete_ds_low$Shock)

low_shock_count <- complete_ds_low %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

#### Dataset generation for each timepoint -------------------------------------

#selecting each timepoint and making it a dataset
freezing_acquisition_high <- complete_ds_high %>%
  select(1:6)
freezing_acquisition_low <- complete_ds_low %>%
  select(1:6)

two_minute_extinction_high <- complete_ds_high %>% #combined 2 and 10 minute group
  select(1:4,"recall_1")
two_minute_extinction_low <- complete_ds_low %>% #combined 2 and 10 minute group
  select(1:4,"recall_1")
#high
#only the two minute in the 2 minute timepoint
two_minute_extinction_only2_high <- two_minute_extinction_high %>% 
  filter(Condition == 2)

#making a copy of the ds for a test of na remove
complete_ds_high_a <- complete_ds_high
complete_ds_high_a1 <- complete_ds_high_a[!is.na(complete_ds_high_a$ext1_curve),]

#Doing it in this order preserves the 
ten_minute_extinction_high <- complete_ds_high_a %>% 
  select(1:4, 8:12)
ten_minute_extinction_high <- ten_minute_extinction_high %>% 
  na.omit()

#low
two_minute_extinction_only2_low <- two_minute_extinction_low %>% 
  filter(Condition == 2)

#making a copy of the ds for a test of na remove
complete_ds_low_a <- complete_ds_low

complete_ds_low_a1 <- complete_ds_low_a[!is.na(complete_ds_low_a$ext1_curve),]

#Doing it in this order preserves the 
ten_minute_extinction_low <- complete_ds_low_a %>% 
  select(1:4, 8:12)
ten_minute_extinction_low <- ten_minute_extinction_low %>% 
  na.omit()

recall_1_high <- complete_ds_high %>% 
  select(1:4, extinction_recall)
recall_1_high[recall_1_high == '#DIV/0!'] <- NA
recall_1_low <- complete_ds_low %>% 
  select(2:4, extinction_recall)
recall_1_low[recall_1_low == '#DIV/0!'] <- NA

reminder_shock_high <- complete_ds_high %>% 
  select(1:4, reminder_day1_shock)
reminder_shock_low <- complete_ds_low %>% 
  select(2:4, reminder_day1_shock)

reminder_recall_high <- complete_ds_high %>%
  select(1:4, reminder_day2)
reminder_recall_low <- complete_ds_low %>%
  select(2:4, reminder_day2)



#### FIGURE THEME #### --------------------------------------------
blank_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(), 
                            legend.position = "bottom",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            axis.text.x = element_text(size = 10, color = "#000000")
                            )
                
#### Poster FIGURE THEME #### --------------------------------------------
poster_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(), 
                            legend.position = "none",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
)  

my_annotate_figure_function <- function(figure,title_text, color = "red", face = "bold", size = 14)
{
  annotate_figure(p = figure, top = text_grob(title_text, color = color, face = face, size = size)) 
}

#### four class colours #### ---------------------------------------------------
four_class <- c("#ff1f5b", "#00CD6C", "#009ADE", "#AF58BE")
orange_blue <- c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff")
orange_blue_bar <- c("#b3b4ff","#1c20fc", "#ffc182","#ff870f")

color_pall <- c("#b3b4ff","#b3b4ff","#1c20fc", "#1c20fc", "#ffc182", "#ffc182","#ff870f","#ff870f")

#### two class colours #### ----------------------------------------------------
extinction_colors <- c("#2f58fe", "#c6a605")

#### Will reuse a lot 
y_title <- "Freezing percentage"
#pre extinction, the figure for pre and post is in a different order so use a different key.
x_labels <- c("Male_ELS" = "ELS", "Male_NS" =  "Non-stressed", "Female_ELS" = "ELS", "Female_NS" =  "Non-stressed")
line_label<-c("Female ELS", "Female NS", "Male ELS", "Male NS")

#Key labels
key_label <-  c("Male NS", "Male ELS","Female NS", "Female ELS")

# Try to design graphs to 

pre_post_function <- function(data, groups, colours)
{
  
}
####HIGH Pre Post ----------------------------------------------------------- 
#check if the 2 min and 10 min vary at this timepoint. 
complete_ds_high_2_10_analysis <- lm(recall_1 ~ Condition, data = complete_ds_high)
summary(complete_ds_high_2_10_analysis)

pre_post_descriptives_high <- freezing_acquisition_high %>% 
  group_by(Sex, Stress)%>% 
  summarize(mean_freezing_pre = mean(Pre, na.rm = T), sem_freezing_pre = sd(Pre, na.rm = T)/sqrt(length(Pre)), mean_freezing_post = mean(Post, na.rm = T), sem_freezing_post = sd(Post, na.rm = T)/sqrt(length(Post)))
write.csv(pre_post_descriptives, file = "./High/Descriptives/pre_post_descriptives.csv")

prepost_figure_ds_high <-  pre_post_descriptives_high %>%
  unite(sex_stress, c(Sex, Stress), remove=TRUE) %>% 
  pivot_longer(cols = c(mean_freezing_pre, mean_freezing_post, sem_freezing_pre, sem_freezing_post), names_to = c(".value","prepost"), names_pattern = "(.*?)_(.*)") %>%  
  mutate(prepost = fct_reorder(prepost, desc(prepost)))


linechart_prepost_high <- ggplot(prepost_figure_ds_high, aes(x= prepost, y = mean, group = sex_stress, colour = sex_stress))+
  geom_line(size = 1.0)+
  coord_cartesian(ylim = c(0, 80)) +
  scale_y_continuous(breaks=seq(0,80,20), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(0.05))+
  scale_y_continuous(breaks=seq(0,80,20))

#Count
df_fearcond_high_count <- freezing_acquisition_high %>% 
  group_by(Sex, Stress) %>% 
  count()

high_count_prepost = list()
for (i in df_fearcond_high_count$n)
{
  high_count_prepost <- append(high_count_prepost,i) 
}

#linechart_prepost_high <- linechart_prepost_high + scale_color_manual(labels = line_label ,values = orange_blue)
linechart_prepost_high <- linechart_prepost_high + scale_color_manual(labels = c("Female_ELS" = sprintf("Female ELS\n n = %s", high_count_prepost[1]), "Female_NS" =  sprintf("Female NS\n n = %s", high_count_prepost[2]), "Male_ELS" = sprintf("Male ELS\n n = %s", high_count_prepost[3]), "Male_NS" =  sprintf("Male NS\n n = %s", high_count_prepost[4])) ,values = orange_blue)
linechart_prepost_high <- linechart_prepost_high + labs(title="High intensity shocK", x = NULL, y = NULL, colour = "Sex and Stress type")
linechart_prepost_high <- linechart_prepost_high + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )

linechart_prepost_high <- linechart_prepost_high + blank_figure_theme
linechart_prepost_high

#Low Prepost
pre_post_descriptives_low <- freezing_acquisition_low %>% 
  group_by(Sex, Stress)%>% 
  summarize(mean_freezing_pre = mean(Pre, na.rm = T), sem_freezing_pre = sd(Pre, na.rm = T)/sqrt(length(Pre)), mean_freezing_post = mean(Post, na.rm = T), sem_freezing_post = sd(Post, na.rm = T)/sqrt(length(Post)))
write.csv(pre_post_descriptives_low, file = "./Low/Descriptives/pre_post_descriptives.csv")

prepost_figure_ds_low <-  pre_post_descriptives_low %>%
  unite(sex_stress, c(Sex, Stress), remove=TRUE) %>% 
  pivot_longer(cols = c(mean_freezing_pre, mean_freezing_post, sem_freezing_pre, sem_freezing_post), names_to = c(".value","prepost"), names_pattern = "(.*?)_(.*)") %>%  
  mutate(prepost = fct_reorder(prepost, desc(prepost)))


linechart_prepost_low <- ggplot(prepost_figure_ds_low, aes(x= prepost, y = mean, group = sex_stress, colour = sex_stress))+
  geom_line(size = 1.0)+
  coord_cartesian(ylim = c(0, 80)) +
  scale_y_continuous(breaks=seq(0,80,20), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(0.05))+
  scale_y_continuous(breaks=seq(0,80,20))

#Count
df_fearcond_low_count <- freezing_acquisition_low %>% 
  group_by(Sex, Stress) %>% 
  count()

low_count_prepost = list()
for (i in df_fearcond_low_count$n)
{
  low_count_prepost <- append(low_count_prepost,i) 
}

#linechart_prepost_low <- linechart_prepost_low + scale_color_manual(labels = line_label ,values = orange_blue)
linechart_prepost_low <- linechart_prepost_low + scale_color_manual(labels = c("Female_ELS" = sprintf("Female ELS\n n = %s", low_count_prepost[1]), "Female_NS" =  sprintf("Female NS\n n = %s", low_count_prepost[2]), "Male_ELS" = sprintf("Male ELS\n n = %s", low_count_prepost[3]), "Male_NS" =  sprintf("Male NS\n n = %s", low_count_prepost[4])) ,values = orange_blue)
linechart_prepost_low <- linechart_prepost_low + labs(title="Low intensity shock", x = NULL, y = NULL, colour = "Sex and Stress type")
linechart_prepost_low <- linechart_prepost_low + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )

linechart_prepost_low <- linechart_prepost_low + blank_figure_theme
linechart_prepost_low

ggsave("PrePost_low.emf", plot = linechart_prepost_low, path = "./Low/Figures/")

#Have to think of how to add the n's 
combined_linechart_prepost<- ggarrange(linechart_prepost_low, linechart_prepost_high, ncol=2, nrow=1, labels = "auto", common.legend = FALSE, legend="bottom")
combined_linechart_prepost <- annotate_figure(combined_linechart_prepost, left = textGrob(y_title, rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
combined_linechart_prepost
ggsave("combined_linechart_prepost.png", plot =combined_linechart_prepost, path = "./Combined/")

write.csv(freezing_acquisition_long_high, file = "./High/")


#inferential stats prepost high#####
#add ID column 
freezing_acquisition_high$ID <- seq.int(nrow(freezing_acquisition_high))
freezing_acquisition_long_high <- freezing_acquisition_high %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")

freezing_acquisition_long_high$Timepoint <- as.factor(freezing_acquisition_long_high$Timepoint)
prepost_aov_high <- aov(data = freezing_acquisition_long_high, Percentage ~ Sex*Stress*Timepoint)
summary(prepost_aov_high)

TukeyHSD(prepost_aov_high)


###### Recall bar plots ===================================================
df_2minext_high <- complete_ds_high %>% 
  select(all_of(factor_cols), "recall_1") %>%
  na.omit() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex))) %>% 
  mutate(figures_stress = fct_reorder(Stress, desc(Stress))) %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  mutate(fig_sex_stress = factor(sex_stress, levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS")))
df_2minext_low <- complete_ds_low %>% 
  select(all_of(factor_cols), "recall_1") %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex))) %>% 
  mutate(figures_stress = fct_reorder(Stress, desc(Stress))) %>% 
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  mutate(fig_sex_stress = factor(sex_stress, levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS")))

two_minute_descriptives_high <- two_minute_extinction_high %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))
  write.csv(two_minute_descriptives, file = "./High/Descriptives/two_minute_descriptives.csv")

  
two_minute_extinction_high_count <- two_minute_extinction_high %>% 
  group_by(Sex, Stress) %>% 
  count()
two_minute_extinction_low_count <- two_minute_extinction_low %>% 
  group_by(Sex, Stress) %>% 
  count()

two_minute_indivpoints <- two_minute_extinction %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) 
two_minute_indivpoints$sex_stress <- factor(two_minute_indivpoints$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

recall_results <- two_minute_descriptives %>% 
  unite(sex_stress, c(figures_sex, Stress), remove = FALSE)
recall_results$sex_stress <- factor(recall_results$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS","Female_ELS"))

#### HIGH LTM1 bars ------------------------------------------------------------
ext2min_figure_high <- ggplot(df_2minext_high, aes(x = fig_sex_stress, y = recall_1,  fill = fig_sex_stress))
ext2min_figure_high <- ext2min_figure_high +
  geom_boxplot()+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
  #stat_summary(fun.data = mean_se, geom = "errorbar")+
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
  geom_point(aes(x = fig_sex_stress, y = recall_1), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6)+
  stat_summary(aes(x = fig_sex_stress, y = recall_1), fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black", position = position_dodge(width = 0.9))

# add n's to figure
# n loop
high_count_2min = list()
for (i in two_minute_extinction_high_count$n)
{
  high_count_2min <- append(high_count_2min,i) 
}

ext2min_figure_high <- ext2min_figure_high + scale_x_discrete(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", high_count_2min[3]), "Male_NS" =  sprintf("Male NS\n n = %s", high_count_2min[4]), "Female_ELS" = sprintf("Female ELS\n n = %s", high_count_2min[1]), "Female_NS" =  sprintf("Female NS\n n = %s", high_count_2min[2])))
ext2min_figure_high <-ext2min_figure_high + scale_fill_manual(labels =c("Male NS","Male ELS", "Female NS", "Female ELS"),values= c("#b3b4ff","#1c20fc", "#ffc182", "#ff870f"))
ext2min_figure_high <-ext2min_figure_high + labs(title="High intensity shock", x = NULL, y = NULL, fill = NULL)
ext2min_figure_high <-ext2min_figure_high + blank_figure_theme

ext2min_figure_high

#Recall bars low -------------------------------------------

ext2min_figure_low <- ggplot(df_2minext_low, aes(x = fig_sex_stress, y = recall_1,  fill = fig_sex_stress))
ext2min_figure_low <- ext2min_figure_low +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks=seq(0,100,25), expand = c(0,0))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
  geom_point(aes(x = fig_sex_stress, y = recall_1), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6)  +
  stat_summary(aes(x = fig_sex_stress, y = recall_1), fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black", position = position_dodge(width = 0.9))

# add n's to figure
# n loop
low_count_2min = list()
for (i in two_minute_extinction_low_count$n)
{
  low_count_2min <- append(low_count_2min,i) 
}

ext2min_figure_low <- ext2min_figure_low + scale_x_discrete(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", high_count_2min[3]), "Male_NS" =  sprintf("Male NS\n n = %s", high_count_2min[4]), "Female_ELS" = sprintf("Female ELS\n n = %s", high_count_2min[1]), "Female_NS" =  sprintf("Female NS\n n = %s", high_count_2min[2])))
# ideally make this automatic? 
ext2min_figure_low <-ext2min_figure_low + scale_fill_manual(labels =c("Male NS","Male ELS", "Female NS", "Female ELS"),values= c("#b3b4ff","#1c20fc", "#ffc182", "#ff870f"))
ext2min_figure_low <-ext2min_figure_low + labs(title="Low intensity shock", x = NULL, y = NULL, fill = NULL)
ext2min_figure_low <-ext2min_figure_low + blank_figure_theme

ext2min_figure_low

###### Combined ------------------------------------------------
combined_2min_bar <- ggarrange(ext2min_figure_low, ext2min_figure_high, ncol=2, nrow=1, labels = "auto", common.legend = TRUE, legend="bottom")
combined_2min_bar <- combined_2min_bar <- my_annotate_figure_function(figure = combined_2min_bar, title_text = NULL)
combined_2min_bar <- annotate_figure(combined_2min_bar, left = textGrob(y_title, rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_2min_bar
ggsave("combined_barchart_2min.png", plot =combined_2min_bar, path = "./Combined/")

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

# bar_chart_recall1 <- ggplot(two_minute_indivpoints, aes(x = sex_stress, y = recall_1,  fill = sex_stress))
# 
# 
# bar_chart_recall1 <- bar_chart_recall1 + geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean')+
#   facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
#   coord_cartesian(ylim = c(0, 110)) +
#   scale_y_continuous(breaks=seq(0,110,5), expand = c(0,0))+
#   geom_point(aes(x = sex_stress, y = recall_1), position = 
#                position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
#                                     dodge.width=0.9), alpha = 0.6) + 
#   geom_errorbar(stat = 'summary', position = position_dodge(width = 0.9), width = 0.5)
#                 position=position_dodge(.9)
# 
# bar_chart_recall1 <- bar_chart_recall1 + scale_x_discrete(labels = x_labels)
# bar_chart_recall1 <- bar_chart_recall1 + scale_fill_manual(labels =c("Male ELS","Male NS", "Female ELS", "Female NS"),values= c("#1c20fc", "#b3b4ff","#ff870f", "#ffc182"))
# bar_chart_recall1 <- bar_chart_recall1 + labs(title="High intensity shock", x = NULL, y = NULL, fill = "Sex and stress type")
# bar_chart_recall1 <- bar_chart_recall1 + blank_figure_theme
# 
# bar_chart_recall1
# #High intensity shock
# ggsave("Recall.png", plot = bar_chart_recall_1, path = "./High/Figures/")

####HIGH 10 minute extinction ------------------------------------------------
extinction_time_groups_descr <- ten_minute_extinction_high %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve))) %>% 
  pivot_longer(cols = c(mean_tencurve1, mean_tencurve2, mean_tencurve3, mean_tencurve4, mean_tencurve5, sem_tencurve1, sem_tencurve2, sem_tencurve3, sem_tencurve4, sem_tencurve5),
               names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)")  

extinction_time_groups_descr$sex_stress <- as.factor(extinction_time_groups_descr$sex_stress)
extinction_time_groups_descr$timepoint <- as.factor(extinction_time_groups_descr$timepoint)
#count 

ten_minute_extinction_high_count <- ten_minute_extinction_high %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

# add n's to figure
# n loop
high_count_10min = list()
for (i in ten_minute_extinction_high_count$n)
{
  high_count_10min <- append(high_count_10min,i) 
}

extinction_curve_figure_high <- ggplot(data = extinction_time_groups_descr, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks=seq(0,100,25), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)

extinction_curve_figure_high <- extinction_curve_figure_high + scale_x_discrete(labels = c("tencurve1" = "2 mins", "tencurve2"= "4 mins", "tencurve3" = "6 mins", "tencurve4" = "8 mins", "tencurve5" = "10 mins"))
extinction_curve_figure_high <- extinction_curve_figure_high + scale_color_manual(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s", high_count_10min[3]), "Male_NS" =  sprintf("Male NS\n n = %s", high_count_10min[4]), "Female_ELS" = sprintf("Female ELS\n n = %s", high_count_10min[1]), "Female_NS" =  sprintf("Female NS\n n = %s", high_count_10min[2])), values= orange_blue)
extinction_curve_figure_high <- extinction_curve_figure_high + labs(title="High intensity shock", x = NULL, y = NULL, group = "Group", color = "Group")
extinction_curve_figure_high <- extinction_curve_figure_high + blank_figure_theme
extinction_curve_figure_high <- extinction_curve_figure_high + theme(legend.position = c(0.8, 0.7))

extinction_curve_figure_high

ggsave("Extinction_lines_high.png", plot = extinction_curve_figure, path = "./High/Figures/")

#Low intensity shock 
write_csv(ten_minute_extinction_low, "./Datasets/ten_minute_extinction_low.csv")
extinction_time_groups_descr_low <- ten_minute_extinction_low %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve))) %>% 
  pivot_longer(cols = c(mean_tencurve1, mean_tencurve2, mean_tencurve3, mean_tencurve4, mean_tencurve5, sem_tencurve1, sem_tencurve2, sem_tencurve3, sem_tencurve4, sem_tencurve5),
               names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)")  

extinction_time_groups_descr_low$sex_stress <- as.factor(extinction_time_groups_descr_low$sex_stress)
extinction_time_groups_descr_low$timepoint <- as.factor(extinction_time_groups_descr_low$timepoint)
#count 

ten_minute_extinction_low_count <- ten_minute_extinction_low %>% 
  group_by(Sex, Stress, Condition) %>% 
  count()

# add n's to figure
# n loop
low_count_10min = list()
for (i in ten_minute_extinction_low_count$n)
{
  low_count_10min <- append(low_count_10min,i) 
}

extinction_curve_figure_low <- ggplot(data = extinction_time_groups_descr_low, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks=seq(0,100,25), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)

extinction_curve_figure_low <- extinction_curve_figure_low + scale_x_discrete(labels = c("tencurve1" = "2 mins", "tencurve2"= "4 mins", "tencurve3" = "6 mins", "tencurve4" = "8 mins", "tencurve5" = "10 mins"))
extinction_curve_figure_low <- extinction_curve_figure_low + scale_color_manual(labels = c("Male_ELS" = sprintf("Male ELS\n n = %s\n", low_count_10min[3]), "Male_NS" =  sprintf("Male NS\n n = %s\n", low_count_10min[4]), "Female_ELS" = sprintf("Female ELS\n n = %s\n", low_count_10min[1]), "Female_NS" =  sprintf("Female NS\n n = %s\n", low_count_10min[2])), values= orange_blue)
extinction_curve_figure_low <- extinction_curve_figure_low + labs(title="Low intensity shock", x = NULL, y = NULL, group = "Group", color = "Group")
extinction_curve_figure_low <- extinction_curve_figure_low + blank_figure_theme
extinction_curve_figure_low <- extinction_curve_figure_low + theme(legend.position = c(0.8, 0.7))
extinction_curve_figure_low

ggsave("Extinction_lines_low.png", plot = extinction_curve_figure, path = "./Low/Figures/")

#Combined
combined_10min_line <- ggarrange(extinction_curve_figure_low, extinction_curve_figure_high, ncol=2, nrow=1, labels = "auto", common.legend = FALSE)
combined_10min_line <- combined_10min_line <- my_annotate_figure_function(figure = combined_10min_line, title_text = NULL)
combined_10min_line <- annotate_figure(combined_10min_line, left = textGrob("Freezing Percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_10min_line

#inferentials 

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
####HIGH LTM2 Extinction recall-------------------------------------------------

recall_1_high <- recall_1_high %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

recall_1_indivpoints_high <- recall_1_high %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

recall_1_high$sex_stress <- as.factor(recall_1_high$sex_stress)
recall_1_high$sex_stress_condition <- as.factor(recall_1_high$sex_stress_condition)
recall_1_high$extinction_recall <- as.numeric(recall_1_high$extinction_recal)
recall_1_indivpoints_high$extinction_recall <- as.numeric(recall_1_indivpoints_high$extinction_recal)
recall_1_high$sex_stress <- factor(recall_1_high$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

recall_1_descr_high <- recall_1_high %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_extRecall = mean(extinction_recall, na.rm = TRUE), sem_extRecall = sd(extinction_recall, na.rm = TRUE)/sqrt(length(extinction_recall)))
write.csv(recall_1_descr, file = "./High/Descriptives/recall_1_descr.csv")

recall_1_high_count <- recall_1_high %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

high_count_recall = list()
for (i in recall_1_high_count$n)
{
  high_count_recall <- append(high_count_recall,i) 
}

#change levels to have non stressed first
recall_1_high$sex_stress <- factor(recall_1_high$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

chart_retval1_high <- ggplot(recall_1_high, aes(x = sex_stress, y = extinction_recall,  fill = Condition))

chart_retval1_high <- chart_retval1_high + 
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = extinction_recall), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  
# Count 
chart_retval1_high <- chart_retval1_high + scale_x_discrete(labels = c("Male_ELS" = sprintf("ELS\n n = %s        n = %s", high_count_recall[5],high_count_recall[6]), "Male_NS" = sprintf( "NS\n n = %s        n = %s", high_count_recall[7],high_count_recall[8]),"Female_ELS" = sprintf("ELS\n n = %s        n = %s", high_count_recall[1],high_count_recall[2]), "Female_NS"= sprintf( "NS\n n = %s        n = %s", high_count_recall[3],high_count_recall[4])))
chart_retval1_high <- chart_retval1_high + scale_fill_manual(values = extinction_colors)
chart_retval1_high <- chart_retval1_high + labs(title = "High intensity shock", x = NULL, y = NULL, fill = "Condition")
chart_retval1_high <- chart_retval1_high + blank_figure_theme
chart_retval1_high

ggsave("Retrieval_bars.png", plot = bar_chart_retval1, path = "./High/Figures/")

#Low 

recall_1_low <- recall_1_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

recall_1_indivpoints_low <- recall_1_low %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

recall_1_low$sex_stress <- as.factor(recall_1_low$sex_stress)
recall_1_low$sex_stress_condition <- as.factor(recall_1_low$sex_stress_condition)
recall_1_low$extinction_recall <- as.numeric(recall_1_low$extinction_recal)
recall_1_indivpoints_low$extinction_recall <- as.numeric(recall_1_indivpoints_low$extinction_recal)
recall_1_low$sex_stress <- factor(recall_1_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

recall_1_descr_low <- recall_1_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_extRecall = mean(extinction_recall, na.rm = TRUE), sem_extRecall = sd(extinction_recall, na.rm = TRUE)/sqrt(length(extinction_recall)))
write.csv(recall_1_descr, file = "./Low/Descriptives/recall_1_descr.csv")

recall_1_low_count <- recall_1_low %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

low_count_recall = list()
for (i in recall_1_low_count$n)
{
  low_count_recall <- append(low_count_recall,i) 
}

#change levels to have non stressed first
recall_1_low$sex_stress <- factor(recall_1_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

chart_retval1_low <- ggplot(recall_1_low, aes(x = sex_stress, y = extinction_recall,  fill = Condition))

chart_retval1_low <- chart_retval1_low + 
  geom_boxplot()+
  #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = extinction_recall), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
  stat_summary(aes(x = sex_stress, y = extinction_recall, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black", position = position_dodge(width = 0.9))

# Count 
chart_retval1_low <- chart_retval1_low + scale_x_discrete(labels = c("Male_ELS" = sprintf("ELS\n n = %s        n = %s", low_count_recall[5],low_count_recall[6]), "Male_NS" = sprintf( "NS\n n = %s        n = %s", low_count_recall[7],low_count_recall[8]),"Female_ELS" = sprintf("ELS\n n = %s        n = %s", low_count_recall[1],low_count_recall[2]), "Female_NS"= sprintf( "NS\n n = %s        n = %s", low_count_recall[3],low_count_recall[4])))
chart_retval1_low <- chart_retval1_low + scale_fill_manual(values = extinction_colors)
chart_retval1_low <- chart_retval1_low + labs(title = "Low intensity shock", x = NULL, y = NULL, fill = "Condition")
chart_retval1_low <- chart_retval1_low + blank_figure_theme
chart_retval1_low

ggsave("Retrieval_bars.png", plot = bar_chart_retval1, path = "./Low/Figures/")

combined_extinction_recall <- ggarrange(chart_retval1_low, chart_retval1_high, ncol=2, nrow=1, labels = "auto", common.legend = TRUE,legend = "bottom")
combined_extinction_recall <- combined_extinction_recall <- my_annotate_figure_function(figure = combined_extinction_recall, title_text = NULL)
combined_extinction_recall <- annotate_figure(combined_extinction_recall, left = textGrob(y_title, rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_extinction_recall


ggsave("Extinction_recall_chart.png", plot = combined_extinction_recall, path = "./combined")
#HIGH Reminder with shock -----

reminder_shock_high <- reminder_shock %>% 
  filter(Shock == "h")

reminder_shock_high_descr <- reminder_shock_high %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_shock_high_indivpoints <- reminder_shock_high %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)

reminder_shock_high_descr$sex_stress <- as.factor(reminder_shock_high_descr$sex_stress)
reminder_shock_high_descr$reminder_day1_shock <- as.numeric(reminder_shock_high_descr$reminder_day1_shock)
reminder_shock_high_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_high_indivpoints$reminder_day1_shock)
reminder_shock_high_descr$sex_stress <- factor(reminder_shock_high_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_shock_high_descr <- reminder_shock_high_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_shock_high = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock_high = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
write.csv(reminder_shock_high_descr, file = "./High/Descriptives/reminder_shock_high_descr.csv")

# to find the n's for each group
reminder_shock_high_count <- reminder_shock_high %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

bar_chart_reminder_shock_high <- ggplot(reminder_shock_high_indivpoints, aes(x = sex_stress, y = reminder_day1_shock,  fill = Condition))

bar_chart_reminder_shock_high <- bar_chart_reminder_shock_high +
  geom_boxplot()+
  #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day1_shock), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  
bar_chart_reminder_shock_high <- bar_chart_reminder_shock_high + scale_x_discrete(labels = x_labels)
bar_chart_reminder_shock_high <- bar_chart_reminder_shock_high + scale_fill_manual(values = extinction_colors)
bar_chart_reminder_shock_high <- bar_chart_reminder_shock_high + labs(title = "High intensity shock", x = NULL, y = NULL, fill = "Condition")
bar_chart_reminder_shock_high <- bar_chart_reminder_shock_high + blank_figure_theme
bar_chart_reminder_shock_high
ggsave("reminder_shock_high.png", plot = bar_chart_reminder_shock_high, path = "./High/Figures/")


#Low reminder with shock ###############################

reminder_shock_low <- reminder_shock %>% 
  filter(Shock == "l")

reminder_shock_low_descr <- reminder_shock_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_shock_low_indivpoints <- reminder_shock_low %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)

reminder_shock_low_descr$sex_stress <- as.factor(reminder_shock_low_descr$sex_stress)
reminder_shock_low_descr$reminder_day1_shock <- as.numeric(reminder_shock_low_descr$reminder_day1_shock)
reminder_shock_low_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_low_indivpoints$reminder_day1_shock)
reminder_shock_low_descr$sex_stress <- factor(reminder_shock_low_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_shock_low_descr <- reminder_shock_low_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_shock_low = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock_low = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
write.csv(reminder_shock_low_descr, file = "./Low/Descriptives/reminder_shock_low_descr.csv")

# to find the n's for each group
reminder_shock_low_count <- reminder_shock_low %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

bar_chart_reminder_shock_low <- ggplot(reminder_shock_low_indivpoints, aes(x = sex_stress, y = reminder_day1_shock,  fill = Condition))

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + 
  geom_boxplot()+
  #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day1_shock), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_x_discrete(labels = x_labels)
bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_fill_manual(values = extinction_colors)
bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + labs(title = "Low intensity shock", x = NULL, y = NULL, fill = "Condition")
bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + blank_figure_theme
bar_chart_reminder_shock_low


# combined reminder shock figure#####################################
combined_reminder_shock <- ggarrange(bar_chart_reminder_shock_low, bar_chart_reminder_shock_high, ncol=2, nrow=1, labels = "auto", common.legend = TRUE,legend = "bottom")
combined_reminder_shock <- combined_reminder_shock <- my_annotate_figure_function(figure = combined_reminder_shock, title_text = NULL)
combined_reminder_shock <- annotate_figure(combined_reminder_shock, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_reminder_shock



#HIGH Reminder recall ------
reminder_recall_high_descr <- reminder_recall_high %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_recall_high_indivpoints <- reminder_recall_high %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)


reminder_recall_high_descr$sex_stress <- as.factor(reminder_recall_high_descr$sex_stress)
reminder_recall_high_descr$sex_stress <- factor(reminder_recall_high_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_recall_high_descr <- reminder_recall_high_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_recall_high = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall_high = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
write.csv(reminder_recall_high_descr, file = "./High/Descriptives/reminder_recall_high_descr.csv")


bar_chart_reminder_recall_high <- ggplot(reminder_recall_high_indivpoints, aes(x = sex_stress, y = reminder_day2,  fill = Condition))

bar_chart_reminder_recall_high<- bar_chart_reminder_recall_high +
  geom_boxplot()+
  #geom_errorbar(stat = "summary", position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day2), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
stat_summary(aes(x = sex_stress, y = reminder_day2, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "red", color = "black", position = position_dodge(width = 0.9))


bar_chart_reminder_recall_high <- bar_chart_reminder_recall_high + scale_x_discrete(labels = x_labels)
bar_chart_reminder_recall_high <- bar_chart_reminder_recall_high + scale_fill_manual(values = extinction_colors)
bar_chart_reminder_recall_high <- bar_chart_reminder_recall_high + labs(title = "High intensity shock", x = NULL, y = NULL, fill = "Condition")
bar_chart_reminder_recall_high <- bar_chart_reminder_recall_high + blank_figure_theme
bar_chart_reminder_recall_high
ggsave("reminder_recall_high.png", plot = bar_chart_reminder_recall_high, path = "./High/Figures/")



#low Reminder recall ------
reminder_recall_low_descr <- reminder_recall_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_recall_low_indivpoints <- reminder_recall_low %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)


reminder_recall_low_descr$sex_stress <- as.factor(reminder_recall_low_descr$sex_stress)
reminder_recall_low_descr$sex_stress <- factor(reminder_recall_low_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_recall_low_descr <- reminder_recall_low_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_recall_low = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall_low = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
write.csv(reminder_recall_low_descr, file = "./low/Descriptives/reminder_recall_low_descr.csv")


bar_chart_reminder_recall_low <- ggplot(reminder_recall_low_indivpoints, aes(x = sex_stress, y = reminder_day2,  fill = Condition))

bar_chart_reminder_recall_low<- bar_chart_reminder_recall_low +
  geom_boxplot()+
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day2), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6, set.seed(42)) + 
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") +
  stat_summary(aes(x = sex_stress, y = reminder_day2, group = Condition), fun = mean, geom = "point", shape = 21, size = 4, fill = "red", color = "black", position = position_dodge(width = 0.9))


bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_x_discrete(labels = x_labels)
bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_fill_manual(values = extinction_colors)
bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + labs(title = "Low intensity shock", x = NULL, y = NULL, fill = "Condition")
bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + blank_figure_theme
bar_chart_reminder_recall_low
ggsave("reminder_recall_low.png", plot = bar_chart_reminder_recall_low, path = "./Low/Figures/")


combined_reminder_recall <- ggarrange(bar_chart_reminder_recall_low, bar_chart_reminder_recall_high, ncol=2, nrow=1, labels = "auto", common.legend = TRUE,legend = "bottom")
combined_reminder_recall <- combined_reminder_recall <- my_annotate_figure_function(figure = combined_reminder_recall, title_text = NULL)
combined_reminder_recall <- annotate_figure(combined_reminder_recall, left = textGrob("Freezing percentage", rot = 90, vjust = 1,hjust= 0.3, gp = gpar(cex = 1.3)))
combined_reminder_recall



# HIGH combined figure 


#needs to be wide to include all three plots 
ggsave("Combined_reminder_figure.png", plot =reminder_combined_figure, path = "./High/Figures/", width = 5050, height = 1000, unit = "px")


#### Low intensity shock ----------------------------------------------------------------
#filter for low

freezing_acquisition_low <- complete_ds_low %>%
  select(1:6)

two_minute_extinction_low <- complete_ds_low %>% 
  select(2:4,"recall_1")

ten_minute_extinction_low <- complete_ds_low %>% 
  na.omit() %>% 
  select(2:4, 8:12)

recall_1_low <- complete_ds_low %>% 
  select(2:4, extinction_recall)
recall_1[recall_1 == '#DIV/0!'] <- NA

reminder_shock_low <- complete_ds_low %>% 
  select(2:4, reminder_day1_shock)

reminder_recall_low <- complete_ds_low %>%
  select(2:4, reminder_day2)

low_pre_post_descriptives <- freezing_acquisition_low %>% 
  group_by(Sex, Stress)%>% 
  summarize(mean_freezing_pre = mean(Pre, na.rm = T), sem_freezing_pre = sd(Pre, na.rm = T)/sqrt(length(Pre)), mean_freezing_post = mean(Post, na.rm = T), sem_freezing_post = sd(Post, na.rm = T)/sqrt(length(Post)))
write.csv(low_pre_post_descriptives, file = "./Low/Descriptives/pre_post_descriptives.csv")

#LOW prePost ####################

# low 2 minute recall plots and data 

two_minute_descriptives_low <- two_minute_extinction_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))
write.csv(two_minute_descriptives_low, file = "./Low/Descriptives/two_minute_descriptives.csv")

two_minute_indivpoints_low <- two_minute_extinction_low %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex,Stress))
two_minute_indivpoints_low$sex_stress <- factor(two_minute_indivpoints_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

recall_results_low <- two_minute_descriptives_low %>% 
  unite(sex_stress, c(figures_sex, Stress), remove = FALSE)
recall_results_low$sex_stress <- factor(recall_results_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS","Female_ELS"))

#LOW LTM1 bar #####

bar_chart_recall1_low <- ggplot(two_minute_indivpoints_low, aes(x = sex_stress, y = recall_1,  fill = sex_stress))


bar_chart_recall1_low <- bar_chart_recall1_low + geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean')+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")+
  coord_cartesian(ylim = c(0, 110)) +
  scale_y_continuous(breaks=seq(0,110,5), expand = c(0,0))+
  geom_point(aes(x = sex_stress, y = recall_1), position = 
               position_jitterdodge(jitter.width = 0.9, jitter.height=0.3, 
                                    dodge.width=0.9), alpha = 0.6) + 
  geom_errorbar(stat = 'summary', position = position_dodge(width = 0.9), width = 0.5)



# bar_chart_recall_1 <- ggplot(recall_results, aes(x= sex_stress, y = mean_recall, fill = sex_stress))+
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_point(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1), position = position_jitter(.1))+
#   #geom_violin(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1, alpha = 0.6))+
#   scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
#   coord_cartesian(ylim = c(0, 100)) +
#   geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2,
#                 position=position_dodge(.9))


#to add significance markers 
#geom_signif(comparisons = list(c("Male_ELS", "Male_NS")), annotations = "******", y_position = 65)+
#geom_signif(comparisons = list(c("Female_ELS", "Female_NS")) , annotations = "NS", y_position = 65) + 
#geom_signif(comparisons = list(c("Male_NS" , "Female_ELS")), annotations = "Blah",  y_position = 55 )




bar_chart_recall1_low <- bar_chart_recall1_low + scale_x_discrete(labels = x_labels)
bar_chart_recall1_low <- bar_chart_recall1_low + scale_fill_manual(labels = key_label,values = orange_blue_bar)
bar_chart_recall1_low <- bar_chart_recall1_low + labs(title="Low intensity shock", x = NULL, y = y_title, fill = "Sex and stress type")
bar_chart_recall1_low <- bar_chart_recall1_low + blank_figure_theme
bar_chart_recall1_low
ggsave("Recall_low.png", plot = bar_chart_recall1_low, path = "./Low/Figures/")


#LOW LTM1 10 minutes ####

extinction_time_groups_descr_low <- ten_minute_extinction_low %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve)))  
write.csv(extinction_time_groups_descr_low, file = "./Low/Descriptives/10_minute_descriptives.csv")

extinction_time_groups_descr_low <- ten_minute_extinction_low %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve))) %>% 
  pivot_longer(cols = c(mean_tencurve1, mean_tencurve2, mean_tencurve3, mean_tencurve4, mean_tencurve5, sem_tencurve1, sem_tencurve2, sem_tencurve3, sem_tencurve4, sem_tencurve5),
               names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)")  
#pivot_longer(cols = starts_with("mean"), names_to = ("timepoint"), values_to = ("freezing"))


extinction_time_groups_descr_low$sex_stress <- as.factor(extinction_time_groups_descr_low$sex_stress)
extinction_time_groups_descr_low$timepoint <- as.factor(extinction_time_groups_descr_low$timepoint)

extinction_curve_figure_low <- ggplot(data = extinction_time_groups_descr_low, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)

extinction_curve_figure_low <- extinction_curve_figure_low + scale_x_discrete(labels = c("tencurve1" = "2 minutes", "tencurve2"= "4 minutes", "tencurve3" = "6 minutes", "tencurve4" = "8 minutes", "tencurve5" = "10 minutes"))

extinction_curve_figure_low <- extinction_curve_figure_low + scale_color_manual(labels = c("F_ELS" = "Female ELS", "F_NS" = "Female non-stressed", "M_ELS" = "Male ELS", "M_NS" = "Male Non-stressed"),values=c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))

extinction_curve_figure_low <- extinction_curve_figure_low + labs(title="Low intensity shock", x = "Timepoint", y = y_title, group = "Group", color = "Group")

extinction_curve_figure_low <- extinction_curve_figure_low + blank_figure_theme

extinction_curve_figure_low

ggsave("Extinction_lines_low.png", plot = extinction_curve_figure_low, path = "./Low/Figures/")


#LOW LTM2 Retrieval----

recall_1_descr_low <- recall_1_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

recall_1_indivpoints_low <- recall_1_low %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

recall_1_descr_low$sex_stress <- as.factor(recall_1_descr_low$sex_stress)
recall_1_descr_low$sex_stress_condition <- as.factor(recall_1_descr_low$sex_stress_condition)
recall_1_descr_low$extinction_recall <- as.numeric(recall_1_descr_low$extinction_recal)
recall_1_indivpoints$extinction_recall <- as.numeric(recall_1_indivpoints$extinction_recal)
recall_1_descr_low$sex_stress <- factor(recall_1_descr_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

recall_1_descr_low <- recall_1_descr_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_extRecall = mean(extinction_recall, na.rm = TRUE), sem_extRecall = sd(extinction_recall, na.rm = TRUE)/sqrt(length(extinction_recall)))
write.csv(recall_1_descr_low, file = "./Low/Descriptives/recall_1_descr_low.csv")

#ggplot indiv points
recall_1_u_low <- recall_1_low %>%
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) 

recall_1_u_low$sex_stress <- factor(recall_1_u_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

bar_chart_retval1_low <- ggplot(recall_1_u_low, aes(x = sex_stress, y = extinction_recall,  fill = Condition))

bar_chart_retval1_low <- bar_chart_retval1_low + geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean')+
  geom_errorbar(stat = 'summary', position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = extinction_recall), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  

bar_chart_retval1_low <- bar_chart_retval1_low + scale_x_discrete(labels = x_labels)

bar_chart_retval1_low <- bar_chart_retval1_low + labs(title="Low intensity shock", x = NULL, y = y_title, fill = "Condition")

bar_chart_retval1_low <- bar_chart_retval1_low + scale_fill_manual(values = extinction_colors)

bar_chart_retval1_low <- bar_chart_retval1_low + blank_figure_theme
bar_chart_retval1_low

ggsave("Retrieval_bars_low.png", plot = bar_chart_retval1_low, path = "./Low/Figures/")

#LOW LTM3 Reminder with shock -----

reminder_shock_descr_low <- reminder_shock_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_shock_indivpoints_low <- reminder_shock_low %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)

reminder_shock_descr_low$sex_stress <- as.factor(reminder_shock_descr_low$sex_stress)
reminder_shock_descr_low$reminder_day1_shock <- as.numeric(reminder_shock_descr_low$reminder_day1_shock)
reminder_shock_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_indivpoints$reminder_day1_shock)
reminder_shock_descr_low$sex_stress <- factor(reminder_shock_descr_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

reminder_shock_descr_low <- reminder_shock_descr_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_shock = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
write.csv(reminder_shock_descr_low, file = "./Low/Descriptives/reminder_shock_descr_low.csv")

reminder_shock_low_nona_count <- reminder_shock_low_nona %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

#Figure

bar_chart_reminder_shock_low <- ggplot(reminder_shock_indivpoints_low, aes(x = sex_stress, y = reminder_day1_shock,  fill = Condition))

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean')+
  geom_errorbar(stat = 'summary', position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day1_shock), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  


bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_x_discrete(labels = x_labels)

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_fill_manual(values = extinction_colors)

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + labs(title="Low intensity shock", x = NULL, y = y_title, fill = "Condition")
bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + blank_figure_theme
bar_chart_reminder_shock_low

ggsave("Reminder_shock_low.png", plot = bar_chart_reminder_shock_low, path = "./Low/Figures/")

#LTM4 Reminder recall ------
reminder_recall_descr_low <- reminder_recall_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_recall_indivpoints_low <- reminder_recall_low %>% 
  drop_na() %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress_condition, c(Sex,Stress,Condition), remove = FALSE) %>% 
  unite(sex_stress, c(Sex, Stress), remove = FALSE)

reminder_recall_descr_low$sex_stress <- as.factor(reminder_recall_descr_low$sex_stress)
reminder_recall_descr_low$sex_stress <- factor(reminder_recall_descr_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

reminder_recall_descr_low <- reminder_recall_descr_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_recall = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
write.csv(reminder_recall_descr_low, file = "./Low/Descriptives/reminder_recall_descr_low.csv")

#Figure

bar_chart_reminder_recall_low <- ggplot(reminder_recall_indivpoints_low, aes(x = sex_stress, y = reminder_day2,  fill = Condition))

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + geom_bar(position = 'dodge', stat = 'summary', fun.y = 'mean')+
  geom_errorbar(stat = 'summary', position = position_dodge(width = 0.9), width = 0.5) +
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0.025,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(aes(x = sex_stress, y = reminder_day2), position = 
               position_jitterdodge(jitter.width = 0.4, jitter.height=0.1, 
                                    dodge.width=0.9), alpha = 0.6) + 
  #scale_fill_manual(values = c('black','black'))+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x")  


bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_x_discrete(labels = x_labels)

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_fill_manual(values = extinction_colors)

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + labs(title = "Low intensity shock", x = NULL, y = y_title, fill = "Condition")
bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + blank_figure_theme
bar_chart_reminder_recall_low




ggsave("bar_chart_reminder_recall_low.png", plot =bar_chart_reminder_recall_low, path = "./Low/Figures/")
# combined figure 


reminder_combined_figure_low <- ggarrange(bar_chart_retval1_low + rremove("legend") , bar_chart_reminder_shock_low + rremove("ylab") + rremove("legend"), bar_chart_reminder_recall_low + rremove("ylab"), 
          labels = c("Extinction recall", "Reminder Shock", "Reminder recall"),
          ncol = 3, nrow = 1)

ggsave("Combined_reminder_figure_low.png", plot =reminder_combined_figure_low, path = "./Low/Figures/", width = 5050, height = 1000, unit = "px")



#### 2 and 10 minute split -----------------------------------------------------
# try spliting the High intensity shock dataset into 2 across the 2 minute and 10 minute group
complete_ds_2 <- complete_ds %>% 
  filter(Condition == "2")
write.csv(complete_ds_2, "./Datasets/completeds2.csv")

complete_ds_10 <- complete_ds %>% 
  filter(Condition == "10")
write.csv(complete_ds_10, "./Datasets/completeds10.csv")

# try it first with 2 minute dataset 
two_minute_extinction_descriptives <- complete_ds_2 %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))


two_minute_extinctionRecall_seperated_descriptives <- complete_ds_2 %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(extinction_recall, na.rm = T), sem_recall = sd(extinction_recall, na.rm = T)/sqrt(length(extinction_recall)))

two_minute_reminderShock_seperated_descriptives <- complete_ds_2 %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(reminder_day1_shock, na.rm = T), sem_recall = sd(reminder_day1_shock, na.rm = T)/sqrt(length(reminder_day1_shock)))

two_minute_reminderRecall_seperated_descriptives <- complete_ds_2 %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(reminder_day2, na.rm = T), sem_recall = sd(reminder_day2, na.rm = T)/sqrt(length(reminder_day2)))

write.csv(two_minute_recall_seperated_descriptives, "./Datasets/twoMinuteRecall.csv")
write.csv(two_minute_extinctionRecall_seperated_descriptives, "./Datasets/twoMinuteExtinctionRecall.csv")
write.csv(two_minute_reminderShock_seperated_descriptives, "./Datasets/twoMinuteReminderShock.csv")
write.csv(two_minute_reminderRecall_seperated_descriptives, "./Datasets/twoMinuteReminderRecall.csv")

two_minute_combined_descriptives <- read.csv("./Datasets/twoMinuteCombinedDescriptives.csv")


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
longplot2 <- longplot2 + scale_x_discrete(labels = c("Recall1" = "LTM1 \n (Shock recall)", "ExtinctionRecall"= "LTM2 \n (Extinction recall)", "ReminderShock" = "LTM3 \n (Long delay with reminder end)", "Reminder_Recall" = "LTM4 \n (Reminder Recall)"))
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
longplot10 <- longplot10 + labs(x = "Timepoint", y = "Freezing percentage", color = "Condition", title = "10 minute group")
longplot10



plot_grid(longplot2, longplot10full, labels = "AUTO", rel_widths = c(1,1))

#### Building figures for poster 

require(grid) 

linechart_prepost_p <- linechart_prepost + labs(title="Aquisition") 
bar_chart_recall_1_p <- bar_chart_recall_1 + labs(title = "2 minute extinction")
extinction_curve_figure_p <- extinction_curve_figure + labs(title = "10 minute extinction")
bar_chart_retval1_p <- bar_chart_retval1 +labs(title = "Extinction retrieval")


#poster_plot1 <- ggarrange(linechart_prepost_p + rremove("xlab"), NULL, bar_chart_recall_1_p + rremove("ylab") + rremove("xlab"), NULL, extinction_curve_figure_p + rremove("xlab") +rremove("ylab"), widths = c(1, 0.5, 1, 0.5, 1), ncol = 5, nrow = 1, common.legend = TRUE, legend = "top")
poster_plot1 <- ggarrange(linechart_prepost_p + rremove("xlab"), NULL, bar_chart_recall_1_p + rremove("ylab") + rremove("xlab"), NULL, extinction_curve_figure_p + rremove("xlab") +rremove("ylab"), NULL, bar_chart_retval1_p + rremove("ylab") + rremove("xlab"), widths = c(1, 0.3, 1, 0.3, 1, 0.15, 2), ncol = 7, nrow = 1, common.legend = TRUE, legend = "top")

poster_plot1

ggsave("poster_plot1.png", plot =poster_plot1, path = "./Combined/")


#### BUilding the figures for the report ---------------------------------------
####

#Pre post


#acquisition recall LTM1
bar_hilo <- ggarrange(bar_chart_recall1_low, bar_chart_recall1, ncol = 2, nrow=1, labels = "auto", common.legend = TRUE, legend = "bottom")
#try to get this to work. It will save time later
bar_hilo_2 <- my_annotate_figure_function(figure = bar_hilo, title_text = "48 Hours post shock LTM1")
bar_hilo_2
ggsave("bar_hilo_2.png", plot =bar_hilo_2, path = "./Combined/")


# 10 minute curves 
extinction_curve_hilo <- ggarrange(extinction_curve_figure_low, extinction_curve_figure, ncol = 2, nrow=1, labels = "auto", common.legend = TRUE, legend = "bottom")
extinction_curve_hilo <- my_annotate_figure_function(figure = extinction_curve_hilo, title_text = "10 minute extinction curve")
extinction_curve_hilo
ggsave("extinction_curve_hilo.png", plot =extinction_curve_hilo, path = "./Combined/")

#extinction recall LTM2
extinction_bar_hilo <- ggarrange(bar_chart_retval1_low, bar_chart_retval1, ncol = 2, nrow=1, labels = "auto", common.legend = TRUE, legend = "bottom")
extinction_bar_hilo <- my_annotate_figure_function(figure = extinction_bar_hilo, title_text = "Extinction recall LTM2")
extinction_bar_hilo
ggsave("extinction_bar_hilo.png", plot =extinction_bar_hilo, path = "./Combined/")

#reminder shock LTM3
reminder_shock <- ggarrange(bar_chart_reminder_shock_low, bar_chart_reminder_shock, ncol = 2, nrow=1, labels = "auto", common.legend = TRUE, legend = "bottom")
reminder_shock <- my_annotate_figure_function(figure = reminder_shock, title_text = "Reminder with shock LTM3")
reminder_shock
ggsave("reminder_shock_hilo.png", plot =reminder_shock, path = "./Combined/")

#reminder recall LTM4
reminder_recall <- ggarrange(bar_chart_reminder_recall_low, bar_chart_reminder_recall, ncol = 2, nrow=1, labels = "auto", common.legend = TRUE, legend = "bottom")
reminder_recall <- my_annotate_figure_function(figure = reminder_recall, title_text = "Reminder Recall LTM3")
reminder_recall
ggsave("reminder_recall_hilo.png", plot =reminder_recall, path = "./Combined/")

