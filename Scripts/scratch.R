sex_column <- c("Male", "Female", "Male")
stress_column <- c("Stress","no_Stress","Stress")
Condition_column <- c(2,10,2)
ID_column <- c(5,7,2)
LitterID_column <- c(2,3,6)

df_1 <- data.frame(sex_column, stress_column, Condition_column,ID_column)
df_2 <- data.frame(sex_column, stress_column, Condition_column, LitterID_column)

df_1$LitterID_column <- NA
df_2$ID_column <- NA

rbind(df_1, df_2)

group<-c(1,1,1,0,0,0)
moderator<-c(1,2,3,4,5,6)
score<-c(6,3,8,5,7,4)

mod1 <- lm(score~group*moderator)
mod2 <- aov(score~group*moderator)

anova(mod1)
anova(mod2)
