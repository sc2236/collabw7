# How do concerns about transition to secondary school change across 3 timepoints? 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)
library(corrplot) 

dat <- read_sav("SchoolTransitionData.sav") %>%
  as.data.frame() %>%
  select(ID, attain_average, starts_with("C1_SConcern"), starts_with("C2_SConcern"), starts_with("C3_SConcern")) %>%
  select(-matches("21|22|23")) %>%
  filter(!is.na(attain_average) & !is.na(C1_SConcern1)) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "Not at all worried", 1, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "I get extremely worried about it", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "11", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "1", 0, as.character(.))))) 

data_long <- dat %>%
  pivot_longer(
    cols = starts_with("C"),  
    names_to = c("Wave", "Concern"), 
    names_pattern = "(C\\d+)_(SConcern\\d+)",  
    values_to = "Concern_Score"
  ) %>%
  mutate(Wave = as.numeric(gsub("C", "", Wave)))  
head(data_long)  

# By participant: 
ggplot(data_long, aes(x = as.factor(Wave), y = Concern_Score, group = ID, color = as.factor(ID))) +
  geom_line(alpha = 0.1) +  # Individual trajectories
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red", size = 1) +
  labs(title = "Change in School Concerns Over Time",
       x = "Wave", y = "Concern Score") +
  theme(legend.position = "none")  

# By concern: 
ggplot(data_long, aes(x = as.factor(Wave), y = Concern_Score, group = Concern, color = Concern)) +
  stat_summary(fun = mean, geom = "line", size = 1) + 
  stat_summary(fun.data = mean_se, width = 0.5, alpha = 0.5) + 
  theme_bw() +
  labs(title = "Change in School Concerns Over Time by Concern Type",
       x = "Wave", y = "Concern Score",
       color = "Concern Type") +   theme(legend.position = "right", legend.title = element_text(size = 5)) 

# Subcategory concern scores: 
concern_data <- data_long %>%
  mutate(Concern_Category = case_when(
    Concern_Score >= 0 & Concern_Score <= 2 ~ "Low (0-2)",
    Concern_Score >= 3 & Concern_Score <= 5 ~ "Moderate (3-5)",
    Concern_Score >= 6 & Concern_Score <= 8 ~ "High (6-8)",
    Concern_Score >= 9 & Concern_Score <= 10 ~ "Very High (9-10)"
  ))

# Proportion of concern levels over time: 
ggplot(concern_data, aes(x = as.factor(Wave), fill = Concern_Category)) +
    geom_bar(position = "fill") +  # Proportional stacked bars
    scale_y_continuous(labels = scales::percent) +  # Convert to percentage
    theme_minimal() +
    labs(title = "Proportion of Concern Levels Over Time",
         x = "Wave", y = "Proportion",
         fill = "Concern Level") + theme(legend.position= "right") 

# Curious to find out about how concern levels maintain over time within categories (so high concern people), so grouping by individual participants' score at Wave 1. 
scorewave1 <- data_long %>%
  filter(Wave == 1) %>%
  group_by(ID) %>%
  summarise(Initial_Concern = mean(Concern_Score, na.rm = TRUE)) %>%
  mutate(Initial_Concern_Category = factor(case_when(
    Initial_Concern >= 0 & Initial_Concern <= 2 ~ "Low (0-2)",
    Initial_Concern >= 3 & Initial_Concern <= 5 ~ "Moderate (3-5)",
    Initial_Concern >= 6 & Initial_Concern <= 8 ~ "High (6-8)",
    Initial_Concern >= 9 & Initial_Concern <= 10 ~ "Very High (9-10)"
  ), levels = c("Low (0-2)", "Moderate (3-5)", "High (6-8)", "Very High (9-10)"))) 

data_long <- data_long %>%
  left_join(scorewave1, by = "ID") 

ggplot(data_long, aes(x = as.factor(Wave), y = Concern_Score, group = ID, color = Initial_Concern_Category)) +
  geom_line(alpha = 0.1) +  # Individual trajectories
  stat_summary(aes(group = Initial_Concern_Category), fun = mean, geom = "line", size = 1.2) +
  theme_bw() +
  labs(title = "Change in Concern Over Time by Initial Concern Category", 
       x = "Wave", y = "Concern Score",
       color = "Wave 1 Concern Level") +
  theme(legend.position = "bottom") 

library(lme4)
model_lme4 <- lmer(Concern_Score ~ Wave + (Wave | ID), data = data_long)
summary(model_lme4) 