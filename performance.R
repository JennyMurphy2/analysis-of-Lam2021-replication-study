# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

# Replication data load

rep_data <- read_csv("replication_dataPerf.csv") %>%
  clean_names() %>%
  mutate(trials)
head(rep_data)

# Jump Height --------------

jump_height_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = jump_height # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
jump_height_data_wide <-  jump_height_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

jump_height_data_wide <- jump_height_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

jump_height_data_long <- jump_height_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "jump_height")

## Descriptives  ---------------

rep_desc <- jump_height_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(jump_height),
            sd = sd(jump_height)) %>%
  mutate(mean_diff = mean(jump_height_data_long$differences), 
         sd_diff = sd(jump_height_data_long$differences))
rep_desc

## Normality check  -----------

jump_height_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

jump_height_data_long$conditions <- as.factor(jump_height_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

jump_height_data_long$conditions <- forcats::fct_relevel(jump_height_data_long$conditions, "single", "consecutive")

jump_height_ttest <- t.test(jump_height ~ conditions, jump_height_data_long, 
                    alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
jump_height_ttest


# Takeoff velocity --------------

vel_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = takeoff_vel # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
vel_data_wide <-  vel_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

vel_data_wide <- vel_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

vel_data_long <- vel_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "takeoff_vel")

## Descriptives  ---------------

vel_desc <- vel_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(takeoff_vel),
            sd = sd(takeoff_vel)) %>%
  mutate(mean_diff = mean(vel_data_long$differences), 
         sd_diff = sd(vel_data_long$differences))
vel_desc

## Normality check  -----------

vel_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

vel_data_long$conditions <- as.factor(vel_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

vel_data_long$conditions <- forcats::fct_relevel(vel_data_long$conditions, "single", "consecutive")

vel_ttest <- t.test(takeoff_vel ~ conditions, vel_data_long, 
                        alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
vel_ttest

# Pkam --------------

p_kam_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = p_kam # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
p_kam_data_wide <-  p_kam_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

p_kam_data_wide <- p_kam_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

p_kam_data_long <- p_kam_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "p_kam")

## Descriptives  ---------------

p_kam_desc <- p_kam_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(p_kam),
            sd = sd(p_kam)) %>%
  mutate(mean_diff = mean(p_kam_data_long$differences), 
         sd_diff = sd(p_kam_data_long$differences))
p_kam_desc

## Normality check NOT NORMAL -----------

p_kam_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

p_kam_data_long$conditions <- as.factor(p_kam_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

p_kam_data_long$conditions <- forcats::fct_relevel(p_kam_data_long$conditions, "single", "consecutive")

p_kam_ttest <- t.test(p_kam ~ conditions, p_kam_data_long, 
                        alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
p_kam_ttest

