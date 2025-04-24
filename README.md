# Misc Analyses Codes
A general practise of visualising and cleaning data using R - for personal use

DATA VISUALISATION #1


set.seed(2)
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(readxl)
library(magrittr) #kind of an extension of pipe - allows you to read the sentence as an "and-then" statement. (ex: read_csv("data.csv") %>% mean(col))
library(dplyr) # select() - column from any data frame, 
library(ggplot2)
library(lme4)

setwd("Documents/R studio data")
data_pcos <- read_csv("pcos_rotterdam_balanceado.csv")
head(data_pcos)
view(data_pcos)

# This is a dataset from Kaggle about PCOS from 3000 women. I am intersted to find out if women with high BMI often report irregular periods? (Correlation?) - 
# Menstrual Irregularity is a Binary variable - convert. 
table(data_pcos$Menstrual_Irregularity) # 868 womnen report out of 3000 report MI - Okay, how does MI affect BMI?

ggplot(data_pcos, aes(x= as.factor(Menstrual_Irregularity), y= BMI)) +
         geom_boxplot(fill = "skyblue") +
         labs(
           x = "Menstrual Irregularity (0 = No, 1 = Yes)",
           y = "BMI",
           title = "how does BMI affect MI?"
         )+
         theme_bw()

# So, Menstrual Irregularity is higher for people with higher BMI. Okay, how much higher is it? Need numerical evidence and statistical significance.
data_pcos %>% 
  group_by(Menstrual_Irregularity) %>% 
  summarise(mean_BMI = mean(BMI, na.rm = TRUE),
            count = n()) %>% # would give mean BMI for peaople who reported menstrual irregularity -> could visualise this at the same time. 
  ggplot(aes(x = as.factor(Menstrual_Irregularity), y = mean_BMI, fill = as.factor(Menstrual_Irregularity))) +
  geom_col() +
  labs(
    x = "Menstrual Irregularity",
    y = "Mean BMI",
    title = "Mean BMI by Menstrual Irregularity"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("red", "pink3")) +
  theme(legend.position = "none")

# T - test?
t.test(BMI ~ Menstrual_Irregularity, data = data_pcos) # Significant. 

# Visualising numerical significance with individual variability-
data_pcos %>%
  mutate(irregular = ifelse(Menstrual_Irregularity == 1, "Irregular", "Regular")) %>%
  ggplot(aes(x = irregular, y = BMI, fill = irregular)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkblue") +
  labs(title = "BMI Distribution by Menstrual Regularity", x = "Cycle Regularity", y = "BMI") +
  theme_minimal()

#We can fit a model to this and visualise the correlation:

glm(Menstrual_Irregularity ~ BMI, , data = data_pcos, family = "binomial")