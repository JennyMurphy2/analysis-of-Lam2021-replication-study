# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

# Replication data load

rep_data <- read_csv("replication_dataLanding.csv") %>%
  clean_names() %>%
  mutate(trials)
head(rep_data)

# Peak GRF --------------

grf_data_wide <-  pivot_wider(
    data = rep_data,
    id_cols = id, # identifying column(s)
    names_from = c("trials", "conditions"), # the new column names
    values_from = vertical_grf # the new column values
  ) 

# create a new variable for the mean of trials 2 - 5 as per the original study
 grf_data_wide <-  grf_data_wide %>%
   rowwise() %>%
   mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                          `4_single`, `5_single`, `6_single`)),
          consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                               `4_consecutive`, `5_consecutive`, `6_consecutive`))) 
 
# add differences column to wide dataset

grf_data_wide <- grf_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

grf_data_long <- grf_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "vertical_grf")

### Outliers check --------

grf_data_long %>%
  identify_outliers(differences)

### Normality check  --------

grf_data_long %>% shapiro_test(differences) 

## Descriptives  ---------------

rep_desc <- grf_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(vertical_grf),
            sd = sd(vertical_grf)) %>%
  mutate(mean_diff = mean(grf_data_long$differences), 
         sd_diff = sd(grf_data_long$differences))
rep_desc

## Paired t-test  -----------------------------

grf_data_long$conditions <- as.factor(grf_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

grf_data_long$conditions <- forcats::fct_relevel(grf_data_long$conditions, "single", "consecutive")

grf_ttest <- t.test(vertical_grf ~ conditions, grf_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
grf_ttest


# MTP angle --------------

mtp_agl_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = mtp_agl # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
mtp_agl_data_wide <-  mtp_agl_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

mtp_agl_data_wide <- mtp_agl_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

mtp_agl_data_long <- mtp_agl_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "mtp_agl")

### Outliers check ---------

mtp_agl_data_long %>%
  identify_outliers(differences)

### Normality check  ----

mtp_agl_data_long %>% shapiro_test(differences) 

## Descriptives  ---------------

mtp_agl_desc <- mtp_agl_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(mtp_agl),
            sd = sd(mtp_agl)) %>%
  mutate(mean_diff = mean(mtp_agl_data_long$differences), 
         sd_diff = sd(mtp_agl_data_long$differences))
mtp_agl_desc


## Paired t-test  -----------------------------

mtp_agl_data_long$conditions <- as.factor(mtp_agl_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

mtp_agl_data_long$conditions <- forcats::fct_relevel(mtp_agl_data_long$conditions, "single", "consecutive")

mtp_agl_ttest <- t.test(mtp_agl ~ conditions, mtp_agl_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
mtp_agl_ttest

# MTP velocity --------------

mtp_vel_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = mtp_vel # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
mtp_vel_data_wide <-  mtp_vel_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

mtp_vel_data_wide <- mtp_vel_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

mtp_vel_data_long <- mtp_vel_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "mtp_vel")

## Descriptives  ---------------

mtp_vel_desc <- mtp_vel_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(mtp_vel),
            sd = sd(mtp_vel)) %>%
  mutate(mean_diff = mean(mtp_vel_data_long$differences), 
         sd_diff = sd(mtp_vel_data_long$differences))
mtp_vel_desc

### Outliers check --------

mtp_vel_data_long %>%
  identify_outliers(differences)

### Normality check  --------

mtp_vel_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

mtp_vel_data_long$conditions <- as.factor(mtp_vel_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

mtp_vel_data_long$conditions <- forcats::fct_relevel(mtp_vel_data_long$conditions, "single", "consecutive")

mtp_vel_ttest <- t.test(mtp_vel ~ conditions, mtp_vel_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
mtp_vel_ttest

# Ankle angle --------------

ankle_agl_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = ankle_agl # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
ankle_agl_data_wide <-  ankle_agl_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

ankle_agl_data_wide <- ankle_agl_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

ankle_agl_data_long <- ankle_agl_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "ankle_agl")

## Descriptives  ---------------

ankle_agl_desc <- ankle_agl_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(ankle_agl),
            sd = sd(ankle_agl)) %>%
  mutate(mean_diff = mean(ankle_agl_data_long$differences), 
         sd_diff = sd(ankle_agl_data_long$differences))
ankle_agl_desc

### Outliers check --------

ankle_agl_data_long %>%
  identify_outliers(differences)

### Normality check  --------

ankle_agl_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

ankle_agl_data_long$conditions <- as.factor(ankle_agl_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

ankle_agl_data_long$conditions <- forcats::fct_relevel(ankle_agl_data_long$conditions, "single", "consecutive")

ankle_agl_ttest <- t.test(ankle_agl ~ conditions, ankle_agl_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
ankle_agl_ttest

# Ankle velocity --------------

ankle_vel_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = ankle_vel # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
ankle_vel_data_wide <-  ankle_vel_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

ankle_vel_data_wide <- ankle_vel_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

ankle_vel_data_long <- ankle_vel_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "ankle_vel")

## Descriptives  ---------------

ankle_vel_desc <- ankle_vel_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(ankle_vel),
            sd = sd(ankle_vel)) %>%
  mutate(mean_diff = mean(ankle_vel_data_long$differences), 
         sd_diff = sd(ankle_vel_data_long$differences))
ankle_vel_desc

### Outliers check --------

ankle_vel_data_long %>%
  identify_outliers(differences)

### Normality check  --------

ankle_vel_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

ankle_vel_data_long$conditions <- as.factor(ankle_vel_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

ankle_vel_data_long$conditions <- forcats::fct_relevel(ankle_vel_data_long$conditions, "single", "consecutive")

ankle_vel_ttest <- t.test(ankle_vel ~ conditions, ankle_vel_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
ankle_vel_ttest

# Ankle momentum --------------

ankle_mom_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = ankle_mom # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
ankle_mom_data_wide <-  ankle_mom_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

ankle_mom_data_wide <- ankle_mom_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

ankle_mom_data_long <- ankle_mom_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "ankle_mom")

## Descriptives  ---------------

ankle_mom_desc <- ankle_mom_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(ankle_mom),
            sd = sd(ankle_mom)) %>%
  mutate(mean_diff = mean(ankle_mom_data_long$differences), 
         sd_diff = sd(ankle_mom_data_long$differences))
ankle_mom_desc

### Outliers check --------

ankle_mom_data_long %>%
  identify_outliers(differences)

## Normality check NOT NORMAL -----------

ankle_mom_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

ankle_mom_data_long$conditions <- as.factor(ankle_mom_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

ankle_mom_data_long$conditions <- forcats::fct_relevel(ankle_mom_data_long$conditions, "single", "consecutive")

ankle_mom_ttest <- t.test(ankle_mom ~ conditions, ankle_mom_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
ankle_mom_ttest

# Ankle Pow --------------

ankle_pow_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = ankle_pow # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
ankle_pow_data_wide <-  ankle_pow_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

ankle_pow_data_wide <- ankle_pow_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

ankle_pow_data_long <- ankle_pow_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "ankle_pow")

## Descriptives  ---------------

ankle_pow_desc <- ankle_pow_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(ankle_pow),
            sd = sd(ankle_pow)) %>%
  mutate(mean_diff = mean(ankle_pow_data_long$differences), 
         sd_diff = sd(ankle_pow_data_long$differences))
ankle_pow_desc

### Outliers check --------

ankle_pow_data_long %>%
  identify_outliers(differences)

## Normality check NOT NORMAL -----------

ankle_pow_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

ankle_pow_data_long$conditions <- as.factor(ankle_pow_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

ankle_pow_data_long$conditions <- forcats::fct_relevel(ankle_pow_data_long$conditions, "single", "consecutive")

ankle_pow_ttest <- t.test(ankle_pow ~ conditions, ankle_pow_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
ankle_pow_ttest

# knee angle --------------

knee_agl_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = knee_agl # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
knee_agl_data_wide <-  knee_agl_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

knee_agl_data_wide <- knee_agl_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

knee_agl_data_long <- knee_agl_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "knee_agl")

## Descriptives  ---------------

knee_agl_desc <- knee_agl_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(knee_agl),
            sd = sd(knee_agl)) %>%
  mutate(mean_diff = mean(knee_agl_data_long$differences), 
         sd_diff = sd(knee_agl_data_long$differences))
knee_agl_desc

### Outliers check --------

knee_agl_data_long %>%
  identify_outliers(differences)

## Normality check  NOT NORMAL -----------

knee_agl_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

knee_agl_data_long$conditions <- as.factor(knee_agl_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

knee_agl_data_long$conditions <- forcats::fct_relevel(knee_agl_data_long$conditions, "single", "consecutive")

knee_agl_ttest <- t.test(knee_agl ~ conditions, knee_agl_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
knee_agl_ttest

# knee velocity --------------

knee_vel_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = knee_vel # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
knee_vel_data_wide <-  knee_vel_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

knee_vel_data_wide <- knee_vel_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

knee_vel_data_long <- knee_vel_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "knee_vel")

## Descriptives  ---------------

knee_vel_desc <- knee_vel_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(knee_vel),
            sd = sd(knee_vel)) %>%
  mutate(mean_diff = mean(knee_vel_data_long$differences), 
         sd_diff = sd(knee_vel_data_long$differences))
knee_vel_desc

### Outliers check --------

knee_vel_data_long %>%
  identify_outliers(differences)

### Normality check  --------

knee_vel_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

knee_vel_data_long$conditions <- as.factor(knee_vel_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

knee_vel_data_long$conditions <- forcats::fct_relevel(knee_vel_data_long$conditions, "single", "consecutive")

knee_vel_ttest <- t.test(knee_vel ~ conditions, knee_vel_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
knee_vel_ttest

# knee momentum --------------

knee_mom_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = knee_mom # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
knee_mom_data_wide <-  knee_mom_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

knee_mom_data_wide <- knee_mom_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

knee_mom_data_long <- knee_mom_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "knee_mom")



## Descriptives  ---------------

knee_mom_desc <- knee_mom_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(knee_mom),
            sd = sd(knee_mom)) %>%
  mutate(mean_diff = mean(knee_mom_data_long$differences), 
         sd_diff = sd(knee_mom_data_long$differences))
knee_mom_desc

### Outliers check --------

knee_mom_data_long %>%
  identify_outliers(differences)

### Normality check  --------

knee_mom_data_long %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

knee_mom_data_long$conditions <- as.factor(knee_mom_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

knee_mom_data_long$conditions <- forcats::fct_relevel(knee_mom_data_long$conditions, "single", "consecutive")

knee_mom_ttest <- t.test(knee_mom ~ conditions, knee_mom_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
knee_mom_ttest

# knee Pow --------------

knee_pow_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = knee_pow # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
knee_pow_data_wide <-  knee_pow_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

knee_pow_data_wide <- knee_pow_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

knee_pow_data_long <- knee_pow_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "knee_pow")


### Outliers check --------

knee_pow_data_long %>%
  identify_outliers(differences)

### Normality check  --------

knee_pow_data_long %>% shapiro_test(differences) 

## Descriptives  ---------------

knee_pow_desc <- knee_pow_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(knee_pow),
            sd = sd(knee_pow)) %>%
  mutate(mean_diff = mean(knee_pow_data_long$differences), 
         sd_diff = sd(knee_pow_data_long$differences))
knee_pow_desc

### Outliers check --------

knee_pow_data_long %>%
  identify_outliers(differences)

## Normality check NOT NORMAL -----------

knee_pow_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

knee_pow_data_long$conditions <- as.factor(knee_pow_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

knee_pow_data_long$conditions <- forcats::fct_relevel(knee_pow_data_long$conditions, "single", "consecutive")

knee_pow_ttest <- t.test(knee_pow ~ conditions, knee_pow_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
knee_pow_ttest

# hip angle --------------

hip_agl_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = hip_agl # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
hip_agl_data_wide <-  hip_agl_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

hip_agl_data_wide <- hip_agl_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

hip_agl_data_long <- hip_agl_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "hip_agl")

## Descriptives  ---------------

hip_agl_desc <- hip_agl_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(hip_agl),
            sd = sd(hip_agl)) %>%
  mutate(mean_diff = mean(hip_agl_data_long$differences), 
         sd_diff = sd(hip_agl_data_long$differences))
hip_agl_desc

### Outliers check --------

hip_agl_data_long %>%
  identify_outliers(differences)

## Normality check  -----------

hip_agl_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

hip_agl_data_long$conditions <- as.factor(hip_agl_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

hip_agl_data_long$conditions <- forcats::fct_relevel(hip_agl_data_long$conditions, "single", "consecutive")

hip_agl_ttest <- t.test(hip_agl ~ conditions, hip_agl_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
hip_agl_ttest

# hip velocity --------------

hip_vel_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = hip_vel # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
hip_vel_data_wide <-  hip_vel_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

hip_vel_data_wide <- hip_vel_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

hip_vel_data_long <- hip_vel_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "hip_vel")

## Descriptives  ---------------

hip_vel_desc <- hip_vel_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(hip_vel),
            sd = sd(hip_vel)) %>%
  mutate(mean_diff = mean(hip_vel_data_long$differences), 
         sd_diff = sd(hip_vel_data_long$differences))
hip_vel_desc

### Outliers check --------

hip_vel_data_long %>%
  identify_outliers(differences)

## Normality check  -----------

hip_vel_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

hip_vel_data_long$conditions <- as.factor(hip_vel_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

hip_vel_data_long$conditions <- forcats::fct_relevel(hip_vel_data_long$conditions, "single", "consecutive")

hip_vel_ttest <- t.test(hip_vel ~ conditions, hip_vel_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
hip_vel_ttest

# hip momentum --------------

hip_mom_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = hip_mom # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
hip_mom_data_wide <-  hip_mom_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

hip_mom_data_wide <- hip_mom_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

hip_mom_data_long <- hip_mom_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "hip_mom")

## Descriptives  ---------------

hip_mom_desc <- hip_mom_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(hip_mom),
            sd = sd(hip_mom)) %>%
  mutate(mean_diff = mean(hip_mom_data_long$differences), 
         sd_diff = sd(hip_mom_data_long$differences))
hip_mom_desc

### Outliers check --------

hip_mom_data_long %>%
  identify_outliers(differences)

## Normality check NOT NORMAL -----------

hip_mom_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

hip_mom_data_long$conditions <- as.factor(hip_mom_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

hip_mom_data_long$conditions <- forcats::fct_relevel(hip_mom_data_long$conditions, "single", "consecutive")

hip_mom_ttest <- t.test(hip_mom ~ conditions, hip_mom_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
hip_mom_ttest

# hip Pow --------------

hip_pow_data_wide <-  pivot_wider(
  data = rep_data,
  id_cols = id, # identifying column(s)
  names_from = c("trials", "conditions"), # the new column names
  values_from = hip_pow # the new column values
) 

# create a new variable for the mean of trials 2 - 5 as per the original study
hip_pow_data_wide <-  hip_pow_data_wide %>%
  rowwise() %>%
  mutate(single = mean(c(`1_single`,`2_single`, `3_single`, 
                         `4_single`, `5_single`, `6_single`)),
         consecutive = mean(c(`1_consecutive`,`2_consecutive`, `3_consecutive`, 
                              `4_consecutive`, `5_consecutive`, `6_consecutive`))) 

# add differences column to wide dataset

hip_pow_data_wide <- hip_pow_data_wide %>% 
  select(id, single, consecutive) %>%
  mutate(differences =  single - consecutive) 

# long dataset

hip_pow_data_long <- hip_pow_data_wide %>% 
  pivot_longer(cols = c("single", "consecutive"),
               names_to = "conditions",
               values_to = "hip_pow")

## Descriptives  ---------------

hip_pow_desc <- hip_pow_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(hip_pow),
            sd = sd(hip_pow)) %>%
  mutate(mean_diff = mean(hip_pow_data_long$differences), 
         sd_diff = sd(hip_pow_data_long$differences))
hip_pow_desc

### Outliers check --------

hip_pow_data_long %>%
  identify_outliers(differences)

## Normality check NOT NORMAL -----------

hip_pow_data_long %>% shapiro_test(differences)

## Paired t-test  -----------------------------

hip_pow_data_long$conditions <- as.factor(hip_pow_data_long$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

hip_pow_data_long$conditions <- forcats::fct_relevel(hip_pow_data_long$conditions, "single", "consecutive")

hip_pow_ttest <- t.test(hip_pow ~ conditions, hip_pow_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
hip_pow_ttest


