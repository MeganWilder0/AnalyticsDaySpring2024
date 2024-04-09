## Research question: What psychological, physiological, environmental, academic, or social
## factors most greatly affect students' level of stress?

## The following are the packages utilized in this project:
# install.packages("tidyverse")
# install.packages("statar")
# install.packages("pROC")
# install.packages("ROCR")
# install.packages("ggpubr")
# install.packages("patchwork")

## Set working directory
setwd("/Users/meganwilder/Documents/Stuff/School/Data Visualization/")

## Import dataset
mhd <- read.csv("StressLevelDataset.csv")

## Let's first create a new binary variable for stress where 0 = 0 and 1, 2 = 1
library(tidyverse)
mhd <- mutate(mhd, stress_binary = ifelse(stress_level == 0, 0, 1))

## We also want to create binary variables from our ranked variables, since they are
## technically considered categorical already. For most of these variables, ranks 0-2
## will be counted as 0s and ranks 3-5 will be counted as 1s unless otherwise specified.
mhd <- mutate(mhd, headache_binary = ifelse(headache < 3, 0, 1))
mhd <- mutate(mhd, sleep_quality_binary = ifelse(sleep_quality < 3, 0, 1))
mhd <- mutate(mhd, breathing_problem_binary = ifelse(breathing_problem < 3, 0, 1))
mhd <- mutate(mhd, noise_level_binary = ifelse(noise_level < 3, 0, 1))
mhd <- mutate(mhd, living_conditions_binary = ifelse(living_conditions < 3, 0, 1))
mhd <- mutate(mhd, safety_binary = ifelse(safety < 3, 0, 1))
mhd <- mutate(mhd, basic_needs_binary = ifelse(basic_needs < 3, 0, 1))
mhd <- mutate(mhd, academic_performance_binary = ifelse(academic_performance < 3, 0, 1))
mhd <- mutate(mhd, study_load_binary = ifelse(study_load < 3, 0, 1))
mhd <- mutate(mhd, teacher_student_relationship_binary = ifelse(teacher_student_relationship < 3, 0, 1))
mhd <- mutate(mhd, future_career_concerns_binary = ifelse(future_career_concerns < 3, 0, 1))
mhd <- mutate(mhd, social_support_binary = ifelse(social_support < 3, 0, 1)) # this variable only goes from 0-4, but I decided to keep the same cutoff for creating the binary variable
mhd <- mutate(mhd, peer_pressure_binary = ifelse(peer_pressure < 3, 0, 1))
mhd <- mutate(mhd, extracurricular_activities_binary = ifelse(extracurricular_activities < 3, 0, 1))
mhd <- mutate(mhd, bullying_binary = ifelse(bullying < 3, 0, 1))

## For ease in creating the model, let's delete the original variables for the newly created
## binary variables
mhd <- mhd[-c(5,7:21)]


## blood_pressure is on a different scale (1-3), and will instead have a dummy coded variable 
## for each of its ranks, which will be set automatically later


## View descriptive statistics and check for missing data
library(statar)
sum_up(mhd)

## Looks good, now let's check all our ranked variables to make sure they 
## contain a sufficient amount of data within each category.
tab(mhd, stress_binary, academic_performance_binary) 
tab(mhd, stress_binary, basic_needs_binary)
tab(mhd, stress_binary, blood_pressure)
tab(mhd, stress_binary, breathing_problem_binary)
tab(mhd, stress_binary, bullying_binary)
tab(mhd, stress_binary, extracurricular_activities_binary)
tab(mhd, stress_binary, future_career_concerns_binary) 
tab(mhd, stress_binary, headache_binary) 
tab(mhd, stress_binary, living_conditions_binary) 
tab(mhd, stress_binary, noise_level_binary) 
tab(mhd, stress_binary, peer_pressure_binary) 
tab(mhd, stress_binary, safety_binary) 
tab(mhd, stress_binary, sleep_quality_binary) 
tab(mhd, stress_binary, social_support_binary)
tab(mhd, stress_binary, study_load_binary)
tab(mhd, stress_binary, teacher_student_relationship_binary) 

## Each of the variable categories seem to have an acceptable amount of observations

## Now, let's set a baseline for the blood_pressure variable
mhd$blood_pressure <- relevel(factor(mhd$blood_pressure), ref="2") # Setting 2 as the baseline


## Now we can create the logistic regression model, using a step-wise variable selection.
library(MASS)

log_mod <- glm(stress_binary ~ ., data = mhd, family = "binomial") %>%
  stepAIC(direction = c("both"),
          trace = FALSE)
summary(log_mod)

## Let's also check for multicollinearity.
library(car)
vif(log_mod)

## There's no problems with multicollinearity. Let's list out the predictors with significance.
# self esteem (p=0.003)
# noise level (p=.012)
# study load (p=0.026)
# safety (p=0.029)
# bullying (p=0.038)
# academic performance (p=0.038)


## Let's find risk ratios and their differences for our predictors.
log_mod_coefficients <- (exp(log_mod$coefficients))
log_mod_coefficients2 <- (exp(log_mod$coefficients) - 1) * 100 # difference
print(log_mod_coefficients)
print(log_mod_coefficients2) # difference


### Interpretations for significant variables below ###

## self_esteem: 0.9491387 (pct difference: -5.08613%)
# An increase in self esteem by one point is associated with a decrease in odds of 
# medium-to-high stress by approximately 5.1% (risk-ratio: 0.9491387), given all other variables 
# are held constant. This association is statistically significant (p=.003).

## noise_level_binary: 2.203546 (pct difference: 120.3546%)
# As compared to low noise-level environments, students in high noise-level environments
# are associated with an increase in likelihood of having medium-to-high stress by approximately
# 120.4% (risk ratio = 2.203546), given that all other variables are held constant. 
# This association is statistically significant. (p=.012).

## safety_binary: 0.5067640 (pct difference: -49.32360%)
# As compared to students with low safety environments, students in high safety environments
# are associated with a decrease in likelihood of having medium-to-high stress by approximately
# 49.32% (risk ratio = 0.5067640), given that all other variables are held constant. 
# This association is statistically significant. (p=.029).

## academic_performance_binary: 0.5210764 (pct difference: -47.89236%)
# As compared to students with low academic performance, students with high academic performance
# are associated with a decrease in likelihood of having medium-to-high stress by approximately
# 47.89% (risk ratio = 0.5210764), given that all other variables are held constant. 
# This association is statistically significant. (p=.038).

## study_load_binary: 2.004897 (pct difference: 100.4897%)
# As compared to students with a low study load, students with a high study load are associated 
# with an increase in likelihood of having medium-to-high stress by approximately 100.49% (risk 
# ratio = 2.004897), given that all other variables are held constant. This association is 
# statistically significant. (p=.026).

## bullying_binary: 1.938610 (pct difference: 93.86102%)
# As compared to students who report low levels of being bullied, students who report high levels
# of being bullied are associated with an increase in likelihood of having medium-to-high stress 
# by approximately 93.86% (risk ratio = 1.938610), given that all other variables are held 
# constant. This association is statistically significant. (p=.038).




##### Graphs and other visuals #####

# AUC-ROC plot
mhd$predicted <- predict(log_mod, newdata=mhd, 
                         type = "response")

# Calculating AUC-ROC value using pROC module
library(pROC)
auc(mhd$stress_binary, mhd$predicted)

# Visualizing AUC-ROC using ROCR module
library(ROCR)
pred_mod <- prediction(mhd$predicted, mhd$stress_binary)
perf_mod <- performance(pred_mod,"tpr","fpr")
plot(perf_mod, lwd = 1.5, col = "darkblue")


## Assigning a color palette for the plots (may be changed later)
# self esteem (psychological): green (dark: #237f5d, light: #6fb293)
# noise level (environmental): yellow (dark: #c0982b, light: #eec76b)
# safety (environmental): red (dark: #ac1917, light: #e54a50)
# academic performance (academic): blue (dark: #156b8a, light: #5d9db9)
# study load (academic): purple (dark: #704776, light: #926ca0)
# bullying (social): orange (dark: #b75420, light: #ea8553)

## Creating a Box plot for self-esteem:
# Note: scores between 15-25 are considered normal, scores below 15 are considered low self-esteem
library(ggpubr)

mhd |>
  ggplot(aes(fill=factor(stress_binary),y=self_esteem)) +
  geom_boxplot() +
  geom_hline(yintercept = 15,color='#d34467',linetype='longdash') +
  labs(x = "Stress Level",
       y = "Self-Esteem Rating (0-30)",
       fill = "Stress Level",
       title = "Self-Esteem Rating Using the Rosenberg Self-Esteem Scale",
       subtitle = "By Stress Level")+
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values=c("#6fb293", "#237f5d")) 
  



## Creating a 100% Stacked Bar Chart for Noise Level:
# Assigning colors for stress level
stress0 <- "#eec76b"
stress1 <- "#c0982b"
cols <- c("0" = stress0, "1" = stress1)

# Viewing contingency table
table(mhd$stress_binary,
      mhd$noise_level_binary)

# Reassigning data types to factor
mhd$noise_level_binary = as.factor(mhd$noise_level_binary)
mhd$stress_binary = as.factor(mhd$stress_binary)

# Sorting the data
mhd2 <- mhd |>
  group_by(stress_binary,noise_level_binary) |>
  count()

# Creating the visualization
mhd2 |>
  group_by(noise_level_binary) |>
  mutate(pct = n/sum(n),
         stress_binary = factor(stress_binary,levels = c("1","0")))|>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = noise_level_binary, y = pct, fill = stress_binary)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Noise Level",
       y = "Proportion",
       fill = "Stress",
       title = "Low vs High Noise Level") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c(0,1),labels = c("Low","High")) +
  scale_fill_manual(values = cols,
                    breaks = c("1","0"),
                    labels = c("Med-High Stress","Low Stress")) -> noise_plot



## Creating a 100% Stacked Bar Chart for Safety:
# Assigning colors for stress level
stress0 <- "#e54a50"
stress1 <- "#ac1917"
cols <- c("0" = stress0, "1" = stress1)

# Viewing contingency table
table(mhd$stress_binary,
      mhd$safety_binary)

# Reassigning data types to factor
mhd$safety_binary = as.factor(mhd$safety_binary)
mhd$stress_binary = as.factor(mhd$stress_binary)

# Sorting the data
mhd2 <- mhd |>
  group_by(stress_binary,safety_binary) |>
  count()

# Creating the visualization
mhd2 |>
  group_by(safety_binary) |>
  mutate(pct = n/sum(n),
         stress_binary = factor(stress_binary,levels = c("1","0")))|>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = safety_binary, y = pct, fill = stress_binary)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Safety",
       y = "Proportion",
       fill = "Stress",
       title = "Low vs High Feelings of Safety") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c(0,1),labels = c("Low","High")) +
  scale_fill_manual(values = cols,
                    breaks = c("1","0"),
                    labels = c("Med-High Stress","Low Stress")) -> safety_plot



## Creating a 100% Stacked Bar Chart for Academic Performance:
# Assigning colors for stress level
stress0 <- "#5d9db9"
stress1 <- "#156b8a"
cols <- c("0" = stress0, "1" = stress1)

# Viewing contingency table
table(mhd$stress_binary,
      mhd$academic_performance_binary)

# Reassigning data types to factor
mhd$academic_performance_binary = as.factor(mhd$academic_performance_binary)
mhd$stress_binary = as.factor(mhd$stress_binary)

# Sorting the data
mhd2 <- mhd |>
  group_by(stress_binary,academic_performance_binary) |>
  count()

# Creating the visualization
mhd2 |>
  group_by(academic_performance_binary) |>
  mutate(pct = n/sum(n),
         stress_binary = factor(stress_binary,levels = c("1","0")))|>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = academic_performance_binary, y = pct, fill = stress_binary)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Academic Performance",
       y = "Proportion",
       fill = "Stress",
       title = "Low vs High Academic Performance") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c(0,1),labels = c("Low","High")) +
  scale_fill_manual(values = cols,
                    breaks = c("1","0"),
                    labels = c("Med-High Stress","Low Stress")) -> academic_plot



## Creating a 100% Stacked Bar Chart for Study Load:
# Assigning colors for stress level
stress0 <- "#926ca0"
stress1 <- "#704776"
cols <- c("0" = stress0, "1" = stress1)

# Viewing contingency table
table(mhd$stress_binary,
      mhd$study_load_binary)

# Reassigning data types to factor
mhd$study_load_binary = as.factor(mhd$study_load_binary)
mhd$stress_binary = as.factor(mhd$stress_binary)

# Sorting the data
mhd2 <- mhd |>
  group_by(stress_binary,study_load_binary) |>
  count()

# Creating the visualization
mhd2 |>
  group_by(study_load_binary) |>
  mutate(pct = n/sum(n),
         stress_binary = factor(stress_binary,levels = c("1","0")))|>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = study_load_binary, y = pct, fill = stress_binary)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Study Load",
       y = "Proportion",
       fill = "Stress",
       title = "Small vs Large Study Load") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c(0,1),labels = c("Small","Large")) +
  scale_fill_manual(values = cols,
                    breaks = c("1","0"),
                    labels = c("Med-High Stress","Low Stress")) -> studyload_plot



## Creating a 100% Stacked Bar Chart for Bullying:
# Assigning colors for stress level
stress0 <- "#ea8553"
stress1 <- "#b75420"
cols <- c("0" = stress0, "1" = stress1)

# Viewing contingency table
table(mhd$stress_binary,
      mhd$bullying_binary)

# Reassigning data types to factor
mhd$bullying_binary = as.factor(mhd$bullying_binary)
mhd$stress_binary = as.factor(mhd$stress_binary)

# Sorting the data
mhd2 <- mhd |>
  group_by(stress_binary,bullying_binary) |>
  count()

# Creating the visualization
mhd2 |>
  group_by(bullying_binary) |>
  mutate(pct = n/sum(n),
         stress_binary = factor(stress_binary,levels = c("1","0")))|>
  mutate(pct1 = paste(round(pct*100,2),"%",sep="")) |>
  ggplot(aes(x = bullying_binary, y = pct, fill = stress_binary)) +
  geom_bar(stat = 'identity', position = position_stack()) +
  geom_text(aes(label = pct1),
            position=position_stack(vjust=0.5),
            color='white') +
  labs(x = "Bullying Intensity",
       y = "Proportion",
       fill = "Stress",
       title = "Low vs High Intensity of Bullying") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(breaks = c(0,1),labels = c("Low","High")) +
  scale_fill_manual(values = cols,
                    breaks = c("1","0"),
                    labels = c("Med-High Stress","Low Stress")) -> bullying_plot



## Let's use the patchwork package to combine our bar plots
library(patchwork)

op <- (studyload_plot + academic_plot) / (bullying_plot + safety_plot) / noise_plot + plot_annotation(title = "Proportion of Low Vs Medium-to-High Levels of Stress For Students By Significant Binary Independent Variables")
op

