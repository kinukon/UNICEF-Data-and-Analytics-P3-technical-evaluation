
library(readxl)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(psych)
library(GPArotation)
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(writexl)

setwd("~/GitHub/UNICEF-P3-assessment-public/01_rawdata")
column_names <- c("interview_date", "child_age_years", "child_birthday", "EC6", "EC7", 
                  "EC8", "EC9", "EC10", "EC11", "EC12", "EC13","EC14","EC15")
survey_responses <- read.csv("Zimbabwe_children_under5_interview.csv", col.names = column_names)
survey_data_clean <- survey_responses %>%
  drop_na()

survey_data_recode <- survey_data_clean %>%
  mutate(across(EC6:EC15, ~case_when(
    . == 1 ~ 1,  # Yes stays as 1
    . == 2 ~ 0,  # No becomes 0
    . == 8 ~ 0   # DK becomes 0
  )))

head(survey_data_recode)

percent_correct <- function(x) {
  mean(x == 1, na.rm = TRUE) * 100
}
summary_stats <- survey_data_recode %>%
  group_by(child_age_years) %>%
  summarise(across(EC6:EC15, percent_correct))

summary_stats

survey_data_recode <- survey_data_recode %>%
  rowwise() %>% 
  mutate(index = mean(c_across(EC6:EC15), na.rm = TRUE))

head(survey_data_recode)

summary_stats <- summary_stats %>%
  rowwise() %>% 
  mutate(index = mean(c_across(EC6:EC15), na.rm = TRUE))

summary_stats

survey_data_recode <- survey_data_recode %>%
  drop_na(child_birthday)

survey_data_recode <- survey_data_recode %>%
  mutate(interview_date = mdy(interview_date),
         child_birthday = mdy(child_birthday))


head(survey_data_recode$interview_date)
head(survey_data_recode$child_birthday)

survey_data_recode <- survey_data_recode %>%
  mutate(child_age_days = as.numeric(interview_date - child_birthday))

head(survey_data_recode$child_age_days)

survey_data_recode <- survey_data_recode %>%
  mutate(child_age_months = child_age_days / 30.5)

head(survey_data_recode$child_age_months)

summary(survey_data_recode$child_age_months)

age_index <- survey_data_recode %>%
  group_by(child_age_months) %>%
  summarise(mean_index = mean(index, na.rm = TRUE))

head(survey_data_recode)

age_index <- survey_data_recode %>%
  group_by(child_age_months) %>%
  summarise(mean_index = mean(index, na.rm = TRUE))

sum(is.na(survey_data_recode$interview_date))
sum(is.na(survey_data_recode$child_birthday))
sum(is.na(survey_data_recode$child_age_months))


ggplot(age_index, aes(x = child_age_months, y = mean_index)) +
  geom_line(color = "blue") +
  labs(title = "conditional mean of index by child's age in months",
       x = "child's age in months",
       y = "mean index") +
  theme_minimal()



survey_data_recode <- survey_data_recode %>%
  mutate(
    literacy_math = rowMeans(select(., EC6, EC7, EC8), na.rm = TRUE),
    
    physical = rowMeans(select(., EC9, EC10), na.rm = TRUE),
    
    learning = rowMeans(select(., EC11, EC12), na.rm = TRUE),
    
    socio_emotional = rowMeans(select(., EC13, EC14, EC15), na.rm = TRUE)
  )
head(survey_data_recode)

summary(survey_data_recode$literacy_math)
summary(survey_data_recode$physical)
summary(survey_data_recode$learning)
summary(survey_data_recode$socio_emotional)

age_education_summary <- survey_data_recode %>%
  group_by(child_age_months) %>%
  summarise(
    avg_literacy_math = mean(literacy_math, na.rm = TRUE),
    avg_physical = mean(physical, na.rm = TRUE),
    avg_learning = mean(learning, na.rm = TRUE),
    avg_socio_emotional = mean(socio_emotional, na.rm = TRUE)
  )

head(age_education_summary)

Q1_literacy_math <- quantile(survey_data_recode$literacy_math, 0.25, na.rm = TRUE)
Q3_literacy_math <- quantile(survey_data_recode$literacy_math, 0.75, na.rm = TRUE)
IQR_literacy_math <- Q3_literacy_math - Q1_literacy_math

lower_bound_lm <- Q1_literacy_math - 1.5 * IQR_literacy_math
upper_bound_lm <- Q3_literacy_math + 1.5 * IQR_literacy_math

Q1_physical <- quantile(survey_data_recode$physical, 0.25, na.rm = TRUE)
Q3_physical <- quantile(survey_data_recode$physical, 0.75, na.rm = TRUE)
IQR_physical <- Q3_physical - Q1_physical
lower_bound_physical <- Q1_physical - 1.5 * IQR_physical
upper_bound_physical <- Q3_physical + 1.5 * IQR_physical

Q1_learning <- quantile(survey_data_recode$learning, 0.25, na.rm = TRUE)
Q3_learning <- quantile(survey_data_recode$learning, 0.75, na.rm = TRUE)
IQR_learning <- Q3_learning - Q1_learning
lower_bound_learning <- Q1_learning - 1.5 * IQR_learning
upper_bound_learning <- Q3_learning + 1.5 * IQR_learning

Q1_socio_emotional <- quantile(survey_data_recode$socio_emotional, 0.25, na.rm = TRUE)
Q3_socio_emotional <- quantile(survey_data_recode$socio_emotional, 0.75, na.rm = TRUE)
IQR_socio_emotional <- Q3_socio_emotional - Q1_socio_emotional
lower_bound_socio_emotional <- Q1_socio_emotional - 1.5 * IQR_socio_emotional
upper_bound_socio_emotional <- Q3_socio_emotional + 1.5 * IQR_socio_emotional

survey_data_no_outliers <- survey_data_recode %>%
  filter(literacy_math >= lower_bound_lm & literacy_math <= upper_bound_lm) %>%
  filter(physical >= lower_bound_physical & physical <= upper_bound_physical) %>%
  filter(learning >= lower_bound_learning & learning <= upper_bound_learning) %>%
  filter(socio_emotional >= lower_bound_socio_emotional & socio_emotional <= upper_bound_socio_emotional)

summary(survey_data_no_outliers)

monthly_summary <- survey_data_recode %>%
  group_by(child_age_months) %>%
  summarise(
    avg_literacy_math = mean(literacy_math, na.rm = TRUE),
    avg_physical = mean(physical, na.rm = TRUE),
    avg_learning = mean(learning, na.rm = TRUE),
    avg_socio_emotional = mean(socio_emotional, na.rm = TRUE)
  )


survey_data_recode <- survey_data_recode %>%
  mutate(age_group_3months = cut(child_age_months, 
                                 breaks = seq(0, 60, by = 3), 
                                 include.lowest = TRUE, 
                                 right = FALSE, 
                                 labels = paste0(seq(0, 57, by = 3), "-", seq(3, 60, by = 3), " months")))

head(monthly_summary_3months)
