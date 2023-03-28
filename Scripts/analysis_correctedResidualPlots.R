library(MASS)
library(tidyverse)
library(ggsignif)
library(ggpubr)
library(car)
library(rstatix)
library(cowplot)
library(svglite)

#START FROM HERE 

complete_ds <- read.csv("./Datasets/high_low_combined.csv", na.strings=c(".","#DIV/0!"))  
complete_ds[complete_ds == "#DIV/0!"] <- NA
#fix an issue with a space in the var 
complete_ds$Stress[complete_ds$Stress == " NS"] <- "NS"

#subset depending on shock remove those which are x (the b cages)
#remove x's
complete_ds <- complete_ds %>% 
  filter(Sex != "x")

#Fix the structure change columns to factor or num
factor_cols <- c("Shock", "Stress", "Sex", "Condition")
num_cols <- c("Pre", "Post", "recall_1", "ext1_curve", "ext2_curve", "ext3_curve", "ext4_curve", "ext5_curve", "extinction_recall", "reminder_day1_shock", "reminder_day2")
complete_ds[factor_cols] <- lapply(complete_ds[factor_cols], factor)  ## as.factor() could also be used
complete_ds[num_cols] <- lapply(complete_ds[num_cols], as.numeric)  


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

#change this! This is the only way I know to reverse 2 and 10 as factor levels for the figures. 
complete_ds$Condition <- fct_rev(complete_ds$Condition)

str(complete_ds)

#filter for high low
complete_ds_high <- complete_ds %>% 
  filter(Shock == "h")

completed_ds_low <- complete_ds %>% 
  filter(Shock == "l")

#whole script below written for just complete_ds (use this for high for now!)

#selecting each timepoint and making it a dataset
complete_ds <- complete_ds_high
freezing_acquisition <- complete_ds %>%
  select(1:6)
two_minute_extinction <- complete_ds %>% 
  select(1:4,"recall_1")
ten_minute_extinction <- complete_ds %>% 
  na.omit() %>% 
  select(1:4, 8:12)

#### FIGURE THEME #### --------------------------------------------
  blank_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                              axis.line = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              legend.position = "bottom",
                              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
                              )
bar_chart_retval1 <- bar_chart_retval1 + blank_figure_theme

bar_chart_retval1                           
#------------------------------------------------------------------      

#### Poster FIGURE THEME #### --------------------------------------------
poster_figure_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(), 
                            legend.position = "none",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
)
bar_chart_retval1 <- bar_chart_retval1 + blank_figure_theme

bar_chart_retval1                           
#------------------------------------------------------------------      



#### four class colours #### ---------------------------------------------------
four_class = c("#ff1f5b", "#00CD6C", "#009ADE", "#AF58BE")
orange_blue = c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff")

#### Will reuse a lot 
y_title <- "Freezing %"
#pre extinction
x_labels <- c("Male_ELS" = "ELS", "Male_NS" =  "Non-stressed", "Female_ELS" = "ELS", "Female_NS" =  "Non-stressed")
#Key labels
key_label <-  c("Male NS", "Male ELS","Female NS", "Female ELS")

#think about changing this to extinction_recall instead to eliminate any issues with recall_1 being a dataset and a column
recall_1 <- complete_ds %>% 
  select(1:4, extinction_recall)
recall_1[recall_1 == '#DIV/0!'] <- NA

reminder_shock <- complete_ds %>% 
  select(1:4, reminder_day1_shock)

reminder_recall <- complete_ds %>%
  select(1:4, reminder_day2)



pre_post_descriptives <- freezing_acquisition %>% 
  group_by(Sex, Stress)%>% 
  summarize(mean_freezing_pre = mean(Pre, na.rm = T), sem_freezing_pre = sd(Pre, na.rm = T)/sqrt(length(Pre)), mean_freezing_post = mean(Post, na.rm = T), sem_freezing_post = sd(Post, na.rm = T)/sqrt(length(Post)))
write.csv(pre_post_descriptives, file = "./High/Descriptives/pre_post_descriptives.csv")

#line figure ####################
prepost_figure_ds <-  pre_post_descriptives %>%
  unite(sex_stress, c(Sex, Stress), remove=TRUE) %>% 
  pivot_longer(cols = c(mean_freezing_pre, mean_freezing_post, sem_freezing_pre, sem_freezing_post), names_to = c(".value","prepost"), names_pattern = "(.*?)_(.*)") %>%  
  mutate(prepost = fct_reorder(prepost, desc(prepost)))


linechart_prepost <- ggplot(prepost_figure_ds, aes(x= prepost, y = mean, group = sex_stress, colour = sex_stress))+
  geom_line(size = 1.0)+
  geom_signif(comparisons = list(c("freezing_pre", "freezing_post")), annotations = "**", y_position = 95, colour = "black")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(0.05))+
  scale_y_continuous(breaks=seq(0,100,10))

# linechart_prepost <- linechart_prepost + scale_color_manual(labels = c("Female ELS", "Female NS", "Male ELS", "Male NS"),values = orange_blue)
# linechart_prepost <- linechart_prepost + labs(title="Pre (2 minutes) and post (1 minute)", x = "Timepoint", y = "Freezing percentage", colour = "Sex and Stress type")
# linechart_prepost <- linechart_prepost + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )

#changing here for poster 
linechart_prepost <- linechart_prepost + scale_color_manual(labels = key_label,values = orange_blue)
linechart_prepost <- linechart_prepost + labs(x = "Timepoint", y = "Freezing percentage", colour = "Sex and Stress type")
linechart_prepost <- linechart_prepost + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )


#removes gridlines, makes axis lines black
#linechart_prepost <- linechart_prepost + poster_figure_theme
linechart_prepost

ggsave("PrePost.emf", plot = linechart_prepost, path = "./High/Figures/")

# 2 minute recall plots and data 
two_minute_descriptives <- two_minute_extinction %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))
  write.csv(two_minute_descriptives, file = "./High/Descriptives/two_minute_descriptives.csv")

two_minute_indivpoints <- two_minute_extinction %>%
  unite(sex_stress, c(Sex,Stress))
two_minute_indivpoints$sex_stress <- factor(two_minute_indivpoints$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

recall_results <- two_minute_descriptives %>% 
  unite(sex_stress, c(figures_sex, Stress), remove = FALSE)
recall_results$sex_stress <- factor(recall_results$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS","Female_ELS"))

# bar plot #####
bar_chart_recall_1 <- ggplot(recall_results, aes(x= sex_stress, y = mean_recall, fill = sex_stress))+
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  #geom_signif(comparisons = list(c("freezing_pre", "freezing_post")), annotations = "**", y_position = 95, colour = "black")+
  #geom_point(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1), position = position_jitter(.1))+
  #geom_violin(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1, alpha = 0.6))+
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2,
                position=position_dodge(.9))

bar_chart_recall_1 <- bar_chart_recall_1 + scale_x_discrete(labels = x_labels)
bar_chart_recall_1 <- bar_chart_recall_1 + scale_fill_manual(labels =key_label,values= orange_blue)
bar_chart_recall_1 <- bar_chart_recall_1 + labs(title="48 hours post shock (2 minute recording).", x = "Group", y = "Freezing percentage", fill = "Sex and stress type")
bar_chart_recall_1 <- bar_chart_recall_1 + blank_figure_theme

bar_chart_recall_1

ggsave("Recall.png", plot = bar_chart_recall_1, path = "./High/Figures/")


# 10 minute descriptives ####

extinction_time_groups_descr <- ten_minute_extinction %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve)))  
write.csv(extinction_time_groups_descr, file = "./High/Descriptives/10_minute_descriptives.csv")



# 10 minute extinction curves ####

extinction_time_groups_descr <- ten_minute_extinction %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve))) %>% 
  pivot_longer(cols = c(mean_tencurve1, mean_tencurve2, mean_tencurve3, mean_tencurve4, mean_tencurve5, sem_tencurve1, sem_tencurve2, sem_tencurve3, sem_tencurve4, sem_tencurve5),
               names_to = c(".value","timepoint"), names_pattern = "(.*?)_(.*)")  
#pivot_longer(cols = starts_with("mean"), names_to = ("timepoint"), values_to = ("freezing"))
  

extinction_time_groups_descr$sex_stress <- as.factor(extinction_time_groups_descr$sex_stress)
extinction_time_groups_descr$timepoint <- as.factor(extinction_time_groups_descr$timepoint)


# extinction curve figure ####
extinction_curve_figure <- ggplot(data = extinction_time_groups_descr, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)

extinction_curve_figure <- extinction_curve_figure + scale_x_discrete(labels = c("tencurve1" = "2 minutes", "tencurve2"= "4 minutes", "tencurve3" = "6 minutes", "tencurve4" = "8 minutes", "tencurve5" = "10 minutes"))

extinction_curve_figure <- extinction_curve_figure + scale_color_manual(labels = c("Female_ELS" = "Female ELS", "Female_NS" = "Female NS", "Male_ELS" = "Male ELS", "Male_NS" = "Male NS"),values= orange_blue)

extinction_curve_figure <- extinction_curve_figure + labs(title="10 minute extinction curve", x = "Timepoint", y = "Freezing percentage", group = "Group", color = "Group")

#removes gridlines, makes axis lines black
poster_theme <- theme(plot.title = element_text(hjust = 0.5),
                            axis.line = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(), 
                            legend.position = c(0.8, 0.6),
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
)

extinction_curve_figure <- extinction_curve_figure + poster_theme
extinction_curve_figure

ggsave("Extinction_lines.png", plot = extinction_curve_figure, path = "./High/Figures/")


#retrieval----

recall_1_descr <- recall_1 %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

recall_1_indivpoints <- recall_1 %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

recall_1_descr$sex_stress <- as.factor(recall_1_descr$sex_stress)
recall_1_descr$sex_stress_condition <- as.factor(recall_1_descr$sex_stress_condition)
recall_1_descr$extinction_recall <- as.numeric(recall_1_descr$extinction_recal)
recall_1_indivpoints$extinction_recall <- as.numeric(recall_1_indivpoints$extinction_recal)
recall_1_descr$sex_stress <- factor(recall_1_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

recall_1_descr <- recall_1_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_extRecall = mean(extinction_recall, na.rm = TRUE), sem_extRecall = sd(extinction_recall, na.rm = TRUE)/sqrt(length(extinction_recall)))
write.csv(recall_1_descr, file = "./High/Descriptives/recall_1_descr.csv")

# retrieval figure ----

bar_chart_retval1 <- ggplot(recall_1_descr, aes(x = sex_stress, y = mean_extRecall, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  #must be an easier way to do this? Maybe with lapply or something? 
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 75), expand = FALSE, clip = "off") +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_extRecall-sem_extRecall, ymax=mean_extRecall + sem_extRecall, width=.2), colour = "black", position = position_dodge(width = 0.9))
 
#geom_signif(y_position = c(7.3,25.3),
#            xmin = c(0.8,1.8), 
#            xmax = c(1.2,2.2),
#            annotation = c("NS","*","0.07","NS"),
#            tip_length = 0)+

# geom_signif(y_position = c(45),
#             xmin = c(0.8), 
#             xmax = c(1.2),
#             annotation = c("NS"),
#             tip_length = 0,  
#             manual = T) +
#   geom_signif(y_position = c(65),
#               xmin = c(1.8), 
#               xmax = c(2.2),
#               annotation = c("*"),
#               tip_length = 0,
#               manual = T) +
#   geom_signif(y_position = c(45),
#               xmin = c(2.8), 
#               xmax = c(3.2),
#               annotation = c("p = 0.07"),
#               tip_length = 0, 
#               manual = T) +
#   geom_signif(y_position = c(45),
#               xmin = c(3.8), 
#               xmax = c(4.2),
#               annotation = c("NS"),
#               tip_length = 0,
#               manual = T) +

bar_chart_retval1 <- bar_chart_retval1 + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stressed", "Female_ELS" = "ELS", "Female_NS" = "Non-stressed"))

#bar_chart_retval1 <- bar_chart_retval1 + scale_fill_manual(values = c("#ff870f","#1c20fc" ))

bar_chart_retval1 <- bar_chart_retval1 + labs(x = "Group", y = "Freezing percentage", fill = "Condition")

#adds the space between the sexes
bar_chart_retval1 <- bar_chart_retval1 + blank_figure_theme

bar_chart_retval1

ggsave("Retrieval_bars.png", plot = bar_chart_retval1, path = "./High/Figures/")


#reminder with shock -----

reminder_shock_descr <- reminder_shock %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_shock_indivpoints <- reminder_shock %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

reminder_shock_descr$sex_stress <- as.factor(reminder_shock_descr$sex_stress)
reminder_shock_descr$reminder_day1_shock <- as.numeric(reminder_shock_descr$reminder_day1_shock)
reminder_shock_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_indivpoints$reminder_day1_shock)
reminder_shock_descr$sex_stress <- factor(reminder_shock_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_shock_descr <- reminder_shock_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_shock = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
write.csv(reminder_shock_descr, file = "./High/Descriptives/reminder_shock_descr.csv")


reminder_shock_count <- reminder_shock %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

# reminder shock figure -----

bar_chart_reminder_shock <- ggplot(reminder_shock_descr, aes(x = sex_stress, y = mean_reminder_shock, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 55)) +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_reminder_shock-sem_reminder_shock, ymax=mean_reminder_shock + sem_reminder_shock, width=.2), colour = "black", position = position_dodge(width = 0.9))

bar_chart_reminder_shock <- bar_chart_reminder_shock + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stress", "Female_ELS" = "ELS", "Female_NS" = "Non-stress"))

bar_chart_reminder_shock <- bar_chart_reminder_shock + scale_fill_manual(values = c("#ff870f","#1c20fc" ))

bar_chart_reminder_shock <- bar_chart_reminder_shock + labs(x = "Group", y = "Freezing percentage", fill = "Condition")

bar_chart_reminder_shock <- bar_chart_reminder_shock + theme(panel.spacing = unit(2,"line"))

bar_chart_reminder_shock

ggsave("Reminder_shock.png", plot = bar_chart_reminder_shock, path = "./High/Figures/")

#reminder recall ------
reminder_recall_descr <- reminder_recall %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_recall_indivpoints <- reminder_recall %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

reminder_recall_descr$sex_stress <- as.factor(reminder_recall_descr$sex_stress)
reminder_recall_descr$sex_stress <- factor(reminder_recall_descr$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS", "Female_ELS"))

reminder_recall_descr <- reminder_recall_descr %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_recall = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
write.csv(reminder_recall_descr, file = "./High/Descriptives/reminder_recall_descr.csv")

# reminder recall figure ------------

bar_chart_reminder_recall <- ggplot(reminder_recall_descr, aes(x = sex_stress, y = mean_reminder_recall, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 55)) +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_reminder_recall-sem_reminder_recall, ymax=mean_reminder_recall + sem_reminder_recall, width=.2), colour = "black", position = position_dodge(width = 0.9))

bar_chart_reminder_recall <- bar_chart_reminder_recall + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stress", "Female_ELS" = "ELS", "Female_NS" = "Non-stress"))
bar_chart_reminder_recall <- bar_chart_reminder_recall + scale_fill_manual(values = c("#ff870f","#1c20fc" ))
bar_chart_reminder_recall <- bar_chart_reminder_recall + labs(x = "Group", y = "Freezing percentage", fill = "Condition")
bar_chart_reminder_recall <- bar_chart_reminder_recall + theme(panel.spacing = unit(2,"line"))
bar_chart_reminder_recall


ggsave("Reminder_recall.png", plot = bar_chart_reminder_recall, path = "./High/Figures/")


# combined figure 
reminder_combined_figure <- ggarrange(bar_chart_retval1 + rremove("legend") , bar_chart_reminder_shock + rremove("ylab") + rremove("legend"), bar_chart_reminder_recall + rremove("ylab"), 
         labels = c("Extinction recall", "Reminder Shock", "Reminder recall"),
         ncol = 3, nrow = 1)
reminder_combined_figure

#needs to be wide to include all three plots 
ggsave("Combined_reminder_figure.png", plot =reminder_combined_figure, path = "./High/Figures/", width = 5050, height = 1000, unit = "px")



#### Inferential stats ####-----------------------------------------------------
# Note all the histograms I have been presenting have been completely wrong. 

#pre-post shock ----
#run the model first and then check the residuals?

hist(freezing_acquisition$Pre)
hist(freezing_acquisition$Post)

#how to save this?
#ggsave("Pre-post-hist.png", path = "./High/Figures/")

#qqplot(freezing_acquisition$Pre, plot = last_plot(), freezing_acquisition$Post)


pre_lm <- lm(data = freezing_acquisition, Pre ~ Sex + Stress + Sex*Stress)
summary(pre_lm)

plot(pre_lm)

post_lm <- lm(data = freezing_acquisition, Post ~ Sex + Stress + Sex*Stress)
summary(post_lm)


plot(freezing_acquisition$Sex, rstandard(pre_lm))
hist(pre_lm$residuals)
hist(post_lm$residuals)

freezing_acquisition$Pre1 <- freezing_acquisition$Pre + 1

pre_lm1 <- lm(data = freezing_acquisition, Pre1 ~ Sex * Stress)
b <- boxcox(pre_lm1, lambda = seq(-3,3))
best_lam <- b$x[which(b$y == max(b$y))]

#combined test this was what I used #####
#replace with this 
freezing_acquisition$ID <- 1:nrow(freezing_acquisition) 
freezing_acquisition_long <- freezing_acquisition %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage") %>%
  na.omit()

res.aov <- anova_test(data = freezing_acquisition, dv = Post, wid = ID, within = c(Pre,Post), between = c(Sex,Stress))
get_anova_table(res.aov)


prepost_lm <- lm(data = freezing_acquisition, Post ~ Sex + Stress + Sex:Stress + Pre)
summary(prepost_lm)

#check the residuals as a histogram
hist(prepost_lm_2$residuals)

freezing_acquisition_long <- freezing_acquisition %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage")

prepost_lm_2 <- lm(data = freezing_acquisition_long, Percentage ~ Timepoint + Sex + Stress + Sex:Stress)
summary(prepost_lm_2)

####### Kerrie suggests using a repeated measures here. 


# recall (two_minute_extinction)----
recall_lm <- lm(data = two_minute_extinction, recall_1 ~ Sex + Stress + Sex:Stress)
summary(recall_lm)

anova(recall_lm)

recall_lm_no_int <- lm(data = two_minute_extinction, recall_1 ~ Sex + Stress)
summary(recall_lm_no_int)

anova(recall_lm_no_int)


hist(two_minute_extinction$recall_1)
shapiro.test(two_minute_extinction$recall_1)

# inferential 10 minute (ten_minute_extinction) ----
ten_minute_ds <- list(ten_minute_extinction$ext1_curve, ten_minute_extinction$ext2_curve, ten_minute_extinction$ext3_curve, ten_minute_extinction$ext4_curve, ten_minute_extinction$ext5_curve)

#histograms
par(mfrow = c(3,2))
lapply(ten_minute_ds,hist)
  
#shapiro
shap_ten_minute <- lapply(ten_minute_ds,shapiro.test)

ten_minute_extinction_long <- ten_minute_extinction %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>% 
  droplevels()
ten_minute_extinction_long$timepoint <- as.factor(ten_minute_extinction_long$timepoint)

# MODEL 1: Sex + Stress + Sex * Stress + timepoint <- very simple model
model_1 <- lm(data = ten_minute_extinction_long, percentage ~ Stress + Sex + Sex* Stress + timepoint)

summary(model_1)
  
Anova(model_1)
# MODEL 2: Sex + Stress + Sex * Stress + timepoint + Sex * timepoint + Stress * timepoint + Sex * Stress * timepoint.Here included each factor compared to timepoint
model_2 <- lm(data = ten_minute_extinction_long, percentage ~ Sex + Stress + timepoint + Sex * Stress + Sex * timepoint + Stress * timepoint + Sex * Stress * timepoint)

summary(model_2)

Anova(model_2)
# MODEL3:
#model_3 <- lm(data = ten_minute_extinction, percentage ~ Stress + Sex + Sex* Stress +
#                timepoint)
  
# MODEL 4:
#Add ID column 

ten_minute_extinction <- ten_minute_extinction%>% 
  mutate(ID = row_number())

ten_minute_extinction_long <- ten_minute_extinction %>% 
  pivot_longer(cols = c(5:9), names_to = "timepoint", values_to = "percentage") %>%   
  droplevels()
ten_minute_extinction_long$timepoint <- as.factor(ten_minute_extinction_long$timepoint)
ten_minute_extinction_long$ID <- as.factor(ten_minute_extinction_long$ID)

rm_anova <- ten_minute_extinction_long %>%
  anova_test(dv = percentage, wid = ID, between = c(Sex, Stress), within = timepoint) %>% 
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
rm_anova

rem_try_extinction <- anova_test(
  data = ten_minute_extinction_long, dv = percentage, wid = ID,
  within = c(Sex,Stress, timepoint)
)

write.csv(ten_minute_extinction_long, file = "./Datasets/ten_minute_extinction_long.csv")
write.csv(ten_minute_extinction, file = "./Datasets/ten_minute_extinction_wide.csv")


# breaking this down. Need to check what to test. 
lm_extinction <- lm(data = ten_minute_extinction_long, percentage_log ~ Sex:Stress:timepoint)
#k_test_extinction <- kruskal.test(data = ten_minute_extinction_long, percentage_log ~Sex:Sex:Condition)
summary(lm_extinction)
anova(lm_extinction)

#extinction recall (recall_1) ----
hist(recall_1$extinction_recall)

#log transform
recall_1$extinction_recallPLusone <- recall_1$extinction_recall + 1
recall_1$extinction_recall_log <- log(recall_1$extinction_recallPLusone)

#sqrt transform
recall_1$extinction_recall_sqrt <- sqrt(recall_1$extinction_recall)

#box cox - doesnt work 
recall_1$extinction_recall_box <- boxCox(recall_1$extinction_recall)

hist(recall_1$extinction_recall)
hist(recall_1$extinction_recall_sqrt)
shapiro.test(recall_1$extinction_recall)
shapiro.test(recall_1$extinction_recall_sqrt)


ext_recall_lm <- lm(data = recall_1, extinction_recall_log ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition)
summary(ext_recall_lm)


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

histogram_datasets <- list(m_els_data1$extinction_recall, m_els_data2$extinction_recall,
                           m_ns_data1$extinction_recall, m_ns_data2$extinction_recall,
                           f_els_data1$extinction_recall, f_els_data2$extinction_recall,
                           f_ns_data1$extinction_recall, f_ns_data2$extinction_recall)

lapply(histogram_datasets, hist)



# Check the t-tests 
t.test(extinction_recall ~ Condition, data = m_els_ds)

t.test(extinction_recall ~ Condition, data = m_ns_ds)

t.test(extinction_recall ~ Condition, data = f_els_ds)

t.test(extinction_recall ~ Condition, data = f_ns_ds)


# reminder (reminder_shock) ----

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





#reminder recall (reminder_recall) ----

#individual t tests conducted assess each group 
# M ELS
m_els_ds <- reminder_recall %>% 
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
m_ns_ds <- reminder_recall %>% 
  filter(Sex == "Male" & Stress == "NS")

m_ns_data1 <- m_ns_ds %>% 
  filter(Condition == 2)  
m_ns_data2 <- m_ns_ds %>% 
  filter(Condition == 10)

# F ELS
f_els_ds <- reminder_recall %>% 
  filter(Sex == "Female" & Stress == "ELS")

f_els_data1 <- f_els_ds %>% 
  filter(Condition == 2)  
f_els_data2 <- f_els_ds %>% 
  filter(Condition == 10)

# F NS
f_ns_ds <- reminder_recall %>% 
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
t.test(reminder_day2 ~ Condition, data = m_els_ds)

t.test(reminder_day2 ~ Condition, data = m_ns_ds)

t.test(reminder_day2 ~ Condition, data = f_els_ds)

t.test(reminder_day2 ~ Condition, data = f_ns_ds)








##### Low shock ####------------------------------------------------------------
#change the name of the dataset to use to remove the "d"
complete_ds_low <- completed_ds_low

#filter for low

freezing_acquisition_low <- complete_ds_low %>%
  select(2:6)

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

# low line figure ####################
low_prepost_figure_ds <-  low_pre_post_descriptives %>%
  unite(sex_stress, c(Sex, Stress), remove=TRUE) %>% 
  pivot_longer(cols = c(mean_freezing_pre, mean_freezing_post, sem_freezing_pre, sem_freezing_post), names_to = c(".value","prepost"), names_pattern = "(.*?)_(.*)") %>%  
  mutate(prepost = fct_reorder(prepost, desc(prepost)))


low_linechart_prepost <- ggplot(low_prepost_figure_ds, aes(x= prepost, y = mean, group = sex_stress, colour = sex_stress))+
  geom_line(size = 1.0)+
  geom_signif(comparisons = list(c("freezing_pre", "freezing_post")), annotations = "**", y_position = 95, colour = "black")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(0.05))+
  scale_y_continuous(breaks=seq(0,100,10))

low_linechart_prepost <- low_linechart_prepost + scale_color_manual(labels = c("Female ELS", "Female NS", "Male ELS", "Male NS"),values=c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))

low_linechart_prepost <- low_linechart_prepost + labs(title="Pre (2 minutes) and post (1 minute)", x = "Timepoint", y = "Freezing percentage", colour = "Sex and Stress type")

low_linechart_prepost <- low_linechart_prepost + scale_x_discrete(labels = c("freezing_pre" = "Pre-Shock", "freezing_post" =  "Post-Shock") )

#removes gridlines, makes axis lines black
low_linechart_prepost <- low_linechart_prepost + theme(plot.title = element_text(hjust = 0.5),
                                               axis.line = element_blank(),
                                               panel.grid.minor = element_blank(),
                                               panel.background = element_blank()
)

low_linechart_prepost

ggsave("LowPrePost.png", plot = low_linechart_prepost, path = "./Low/Figures/")

# low 2 minute recall plots and data 

two_minute_descriptives_low <- two_minute_extinction_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  group_by(figures_sex, Stress)%>% 
  #summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T))
  summarize(mean_recall = mean(recall_1, na.rm = T), sem_recall = sd(recall_1, na.rm = T)/sqrt(length(recall_1)))
write.csv(two_minute_descriptives_low, file = "./Low/Descriptives/two_minute_descriptives.csv")



two_minute_indivpoints_low <- two_minute_extinction_low %>%
  unite(sex_stress, c(Sex,Stress))
two_minute_indivpoints_low$sex_stress <- factor(two_minute_indivpoints_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

recall_results_low <- two_minute_descriptives_low %>% 
  unite(sex_stress, c(figures_sex, Stress), remove = FALSE)
recall_results_low$sex_stress <- factor(recall_results_low$sex_stress,levels = c("Male_NS", "Male_ELS", "Female_NS","Female_ELS"))

# low bar plot #####

bar_chart_recall_1_low <- ggplot(recall_results_low, aes(x= sex_stress, y = mean_recall, fill = sex_stress))+
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  #geom_point(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1), position = position_jitter(.1))+
  #geom_violin(data = two_minute_indivpoints, aes(x = sex_stress, y = recall_1, alpha = 0.6))+
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  coord_cartesian(ylim = c(0, 100)) +
  geom_errorbar(aes(ymin=mean_recall-sem_recall, ymax=mean_recall+sem_recall), width=.2,
                position=position_dodge(.9))

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




bar_chart_recall_1_low <- bar_chart_recall_1_low + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS" =  "Non-stressed", "Female_ELS" = "ELS", "Female_NS" =  "Non-stressed"))

bar_chart_recall_1_low <- bar_chart_recall_1_low + scale_fill_manual(labels = c("Male ELS", "Male NS","Female ELS", "Female NS"),values=c("#1c20fc", "#b3b4ff", "#ff870f", "#ffc182"))

bar_chart_recall_1_low <- bar_chart_recall_1_low + labs(title="48 hours post shock (2 minute recording).", x = "Group", y = "Freezing percentage", fill = "Sex and stress type")

bar_chart_recall_1_low <- bar_chart_recall_1_low + blank_figure_theme

bar_chart_recall_1_low

ggsave("Recall_low.png", plot = bar_chart_recall_1_low, path = "./Low/Figures/")


# low 10 minute descriptives ####

extinction_time_groups_descr_low <- ten_minute_extinction_low %>% 
  unite(sex_stress, c(Sex, Stress), remove=TRUE)%>% 
  group_by(sex_stress) %>% 
  summarise(mean_tencurve1 = mean(ext1_curve), mean_tencurve2 = mean(ext2_curve), mean_tencurve3 = mean(ext3_curve), mean_tencurve4 = mean(ext4_curve), mean_tencurve5 = mean(ext5_curve),
            sem_tencurve1 = sd(ext1_curve)/sqrt(length(ext1_curve)), sem_tencurve2 = sd(ext2_curve)/sqrt(length(ext2_curve)),sem_tencurve3 = sd(ext3_curve)/sqrt(length(ext3_curve)),sem_tencurve4 = sd(ext4_curve)/sqrt(length(ext4_curve)), sem_tencurve5 = sd(ext1_curve)/sqrt(length(ext1_curve)))  
write.csv(extinction_time_groups_descr_low, file = "./Low/Descriptives/10_minute_descriptives.csv")



# 10 minute extinction curves ####

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


# extinction curve figure ####
extinction_curve_figure_low <- ggplot(data = extinction_time_groups_descr_low, aes(x = timepoint, y = mean, group = sex_stress, color = sex_stress)) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, alpha = 0.7)

extinction_curve_figure_low <- extinction_curve_figure_low + scale_x_discrete(labels = c("tencurve1" = "2 minutes", "tencurve2"= "4 minutes", "tencurve3" = "6 minutes", "tencurve4" = "8 minutes", "tencurve5" = "10 minutes"))

extinction_curve_figure_low <- extinction_curve_figure_low + scale_color_manual(labels = c("F_ELS" = "Female ELS", "F_NS" = "Female non-stressed", "M_ELS" = "Male ELS", "M_NS" = "Male Non-stressed"),values=c("#ff870f", "#ffc182", "#1c20fc", "#b3b4ff"))

extinction_curve_figure_low <- extinction_curve_figure_low + labs(title="10 minute extinction curve", x = "Timepoint", y = "Freezing percentage", group = "Group", color = "Group")

#removes gridlines, makes axis lines black
extinction_curve_figure_low <- extinction_curve_figure_low + blank_figure_theme

extinction_curve_figure_low

ggsave("Extinction_lines_low.png", plot = extinction_curve_figure_low, path = "./Low/Figures/")


#retrieval----

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

# retrieval figure ----

bar_chart_retval1_low <- ggplot(recall_1_descr_low, aes(x = sex_stress, y = mean_extRecall, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 55), expand = FALSE, clip = "off") +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_extRecall-sem_extRecall, ymax=mean_extRecall + sem_extRecall, width=.2), colour = "black", position = position_dodge(width = 0.9))
# 

# bar_chart_retval1 <- bar_chart_retval1 + geom_text(data = data.frame(x = 1, y = 35, figures_sex = "Male", label = "Test"), 
#           aes(x = x, y = y, label = label), size = 4)
# #within annotate you can set x as 1 for the first set of results and 2 for the second. Y is equal to the scale.
# #annotate(geom = "text", x=1, y =20, label = "**")
# #geom_signif(map_signif_level , comparisons = list(c("Male_ELS_2", "Male_ELS_10")), annotations = "******", y_position = 185)
# 



bar_chart_retval1_low <- bar_chart_retval1_low + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stress", "Female_ELS" = "ELS", "Female_NS" = "Non-stressed"))

bar_chart_retval1_low <- bar_chart_retval1_low + labs(x = "Group", y = "Freezing percentage", fill = "Condition")

bar_chart_retval1_low <- bar_chart_retval1_low + theme(panel.spacing = unit(5,"line"))

bar_chart_retval1_low <- bar_chart_retval1_low + theme(panel.spacing = unit(2,"line"))
bar_chart_retval1_low + blank_figure_theme

ggsave("Retrieval_bars_low.png", plot = bar_chart_retval1_low, path = "./Low/Figures/")


#reminder with shock -----

reminder_shock_descr_low <- reminder_shock_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_shock_indivpoints_low <- reminder_shock_low %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

reminder_shock_descr_low$sex_stress <- as.factor(reminder_shock_descr_low$sex_stress)
reminder_shock_descr_low$reminder_day1_shock <- as.numeric(reminder_shock_descr_low$reminder_day1_shock)
reminder_shock_indivpoints$reminder_day1_shock <- as.numeric(reminder_shock_indivpoints$reminder_day1_shock)
reminder_shock_descr_low$sex_stress <- factor(reminder_shock_descr_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

reminder_shock_descr_low <- reminder_shock_descr_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_shock = mean(reminder_day1_shock, na.rm = TRUE), sem_reminder_shock = sd(reminder_day1_shock, na.rm = TRUE)/sqrt(length(reminder_day1_shock)))
write.csv(reminder_shock_descr_low, file = "./Low/Descriptives/reminder_shock_descr_low.csv")

reminder_shock_low_nona <- reminder_shock_low %>%
  drop_na()
  
reminder_shock_low_nona_count <- reminder_shock_low_nona %>% 
  group_by(Sex,Stress,Condition) %>% 
  count()

reminder_shock_low_nona_count

# reminder shock figure -----

bar_chart_reminder_shock_low <- ggplot(reminder_shock_descr_low, aes(x = sex_stress, y = mean_reminder_shock, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 55)) +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_reminder_shock-sem_reminder_shock, ymax=mean_reminder_shock + sem_reminder_shock, width=.2), colour = "black", position = position_dodge(width = 0.9))

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stress", "Female_ELS" = "ELS", "Female_NS" = "Non-stress"))

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + scale_fill_manual(values = c("#ff870f","#1c20fc" ))

bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + labs(x = "Group", y = "Freezing percentage", fill = "Condition")
bar_chart_reminder_shock_low <- bar_chart_reminder_shock_low + theme(panel.spacing = unit(2,"line"))

ggsave("Reminder_shock_low.png", plot = bar_chart_reminder_shock_low, path = "./Low/Figures/")



#reminder recall ------
reminder_recall_descr_low <- reminder_recall_low %>% 
  mutate(figures_sex = fct_reorder(Sex, desc(Sex)))%>%
  unite(sex_stress, c(Sex, Stress), remove=FALSE) %>% 
  unite(sex_stress_condition, c(Sex, Stress, Condition), remove = FALSE)

reminder_recall_indivpoints_low <- reminder_recall_low %>% 
  drop_na() %>% 
  unite(sex_stress_condition, c(Sex,Stress,Condition))

reminder_recall_descr_low$sex_stress <- as.factor(reminder_recall_descr_low$sex_stress)
reminder_recall_descr_low$sex_stress <- factor(reminder_recall_descr_low$sex_stress,levels = c("Male_ELS", "Male_NS", "Female_ELS", "Female_NS"))

reminder_recall_descr_low <- reminder_recall_descr_low %>%  
  group_by(sex_stress, Condition, Sex, Stress, figures_sex, sex_stress_condition) %>% 
  summarize(mean_reminder_recall = mean(reminder_day2, na.rm = TRUE), sem_reminder_recall = sd(reminder_day2, na.rm = TRUE)/sqrt(length(reminder_day2)))
write.csv(reminder_recall_descr_low, file = "./Low/Descriptives/reminder_recall_descr_low.csv")

# reminder recall figure ------------

bar_chart_reminder_recall_low <- ggplot(reminder_recall_descr_low, aes(x = sex_stress, y = mean_reminder_recall, fill = Condition)) +
  #geom_point(data = recall_1_indivpoints, aes(x = sex_stress_condition, y = extinction_recall), position = position_dodge(.1))
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~figures_sex, strip.position = "bottom", scales = "free_x") + 
  coord_cartesian(ylim = c(0, 55)) +
  scale_y_continuous(breaks=seq(0,100,5), expand = c(0,0))+
  geom_errorbar(aes(ymin=mean_reminder_recall-sem_reminder_recall, ymax=mean_reminder_recall + sem_reminder_recall, width=.2), colour = "black", position = position_dodge(width = 0.9))

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_x_discrete(labels = c("Male_ELS" = "ELS", "Male_NS"= "Non-stress", "Female_ELS" = "ELS", "Female_NS" = "Non-stress"))

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + scale_fill_manual(values = c("#ff870f","#1c20fc" ))

bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + labs(x = "Group", y = "Freezing percentage", fill = "Condition")
bar_chart_reminder_recall_low <- bar_chart_reminder_recall_low + theme(panel.spacing = unit(2,"line"))

ggsave("bar_chart_reminder_recall_low.png", plot =bar_chart_reminder_recall_low, path = "./Low/Figures/")
# combined figure 


reminder_combined_figure_low <- ggarrange(bar_chart_retval1_low + rremove("legend") , bar_chart_reminder_shock_low + rremove("ylab") + rremove("legend"), bar_chart_reminder_recall_low + rremove("ylab"), 
          labels = c("Extinction recall", "Reminder Shock", "Reminder recall"),
          ncol = 3, nrow = 1)

ggsave("Combined_reminder_figure_low.png", plot =reminder_combined_figure_low, path = "./Low/Figures/", width = 5050, height = 1000, unit = "px")





# inferential stats ######

#pre-post shock ----

par(mfrow=c(1,1))
hist(freezing_acquisition_low$Pre)
hist(freezing_acquisition_low$Post)

shapiro.test(freezing_acquisition_low$Pre)

qqplot(freezing_acquisition_low$Pre, freezing_acquisition_low$Post)


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
#b <- boxcox(pre_lm1_low, lambda = seq(-3,3))
#best_lam <- b$x[which(b$y == max(b$y))]

#combined test 
prepost_lm_low <- lm(data = freezing_acquisition_low, Post ~ Sex + Stress + Sex:Stress + Pre)
summary(prepost_lm_low)

freezing_acquisition_long_low <- freezing_acquisition_low %>% 
  pivot_longer(col = c("Pre", "Post"), names_to = "Timepoint", values_to = "Percentage") %>% 

prepost_lm_2_low <- lm(data = freezing_acquisition_long_low, Percentage ~ Timepoint + Sex + Stress + Sex:Stress)
summary(prepost_lm_2_low)

anova(prepost_lm_2_low)

# recall (two_minute_extinction)----
recall_lm_low <- lm(data = two_minute_extinction_low, recall_1 ~ Sex + Stress + Sex:Stress)
summary(recall_lm_low)

recall_lm_no_int_low <- lm(data = two_minute_extinction_low, recall_1 ~ Sex + Stress)
summary(recall_lm_no_int_low)

hist(two_minute_extinction_low$recall_1)
#10 minute (ten_minute_extinction) ----



# breaking this down. Need to check what to test. 
lm_extinction_low <- lm(data = ten_minute_extinction_long_low, percentage_log ~ Sex:Stress:timepoint)
#k_test_extinction <- kruskal.test(data = ten_minute_extinction_long, percentage_log ~Sex:Sex:Condition)
summary(lm_extinction_low)
anova(lm_extinction_low)

#extinction recall (recall_1) ----
hist(recall_1$extinction_recall)

#log transform
recall_1$extinction_recallPLusone <- recall_1$extinction_recall + 1
recall_1$extinction_recall_log <- log(recall_1$extinction_recallPLusone)

#sqrt transform
recall_1$extinction_recall_sqrt <- sqrt(recall_1$extinction_recall)

#box cox - doesnt work 
recall_1$extinction_recall_box <- boxCox(recall_1$extinction_recall)

hist(recall_1$extinction_recall)
hist(recall_1$extinction_recall_sqrt)
shapiro.test(recall_1$extinction_recall)
shapiro.test(recall_1$extinction_recall_sqrt)


ext_recall_lm <- lm(data = recall_1, extinction_recall_log ~ Sex + Stress + Condition + Sex:Stress + Sex:Condition + Stress:Condition + Sex:Sex:Condition)
summary(ext_recall_lm)


#individual t tests conducted assess each group 
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


# reminder (reminder_shock) ----

#reminder recall (reminder_recall) ----



#### 2 and 10 minute split #### ------------------------------------------------
# try spliting the high shock dataset into 2 across the 2 minute and 10 minute group
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



#### BUilding the figures for the report ---------------------------------------
####

#Pre post
plot_grid(low_linechart_prepost, linechart_prepost, labels = "AUTO", rel_widths = c(1,1))
