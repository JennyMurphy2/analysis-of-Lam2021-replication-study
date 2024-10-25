# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

set.seed(21)

# Replication data load

rep_data <- read_csv("replication_dataLanding.csv") %>%
  clean_names() %>%
  mutate(trials)
head(rep_data)

# Peak landing GRF --------------

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

## Descriptives  ---------------

desc <- grf_data_long %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(vertical_grf),
            sd = sd(vertical_grf)) %>%
  mutate(mean_diff = mean(grf_data_long$differences), 
         sd_diff = sd(grf_data_long$differences))
desc

## Resolving assumptions  ---------------------------------------

## Distribution check 

ggplot(grf_data_long, aes(vertical_grf)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(grf_data_long, aes(conditions, vertical_grf, color = conditions)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

grf_data_long %>%
  identify_outliers(differences)

### Normality check  

grf_data_long %>% shapiro_test(differences) 

### Outlier removal ---------

# remove outlier for normal data

normal_data <- grf_data_long %>%
  filter(id != 40)

### Outliers check 

normal_data %>%
  identify_outliers(differences)

### Normality check  

normal_data %>% shapiro_test(differences) 

desc_normal <- normal_data %>%
  group_by(conditions) %>%
  summarise(count = n(),
            mean = mean(vertical_grf),
            sd = sd(vertical_grf)) %>%
  mutate(mean_diff = mean(normal_data$differences), 
         sd_diff = sd(normal_data$differences))
desc_normal

### Distribution check

ggplot(normal_data, aes(vertical_grf)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(normal_data, aes(conditions, vertical_grf, color = conditions)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()


# Paired t-test  -----------------------------

normal_data$conditions <- as.factor(normal_data$conditions)

# R compares conditions alphabetically, I am reordering here to match the original study

normal_data$conditions <- forcats::fct_relevel(normal_data$conditions, "single", "consecutive")

#normal_replication_ttest <- t.test(vertical_grf ~ conditions, normal_data, 
#                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
#  tidy()
#normal_replication_ttest

normal_data_wide <- grf_data_wide %>%
  filter(id != 40)

normal_replication_ttest <- t.test(normal_data_wide$single, normal_data_wide$consecutive, paired = TRUE)
normal_replication_ttest

### Replication effect size calculation ------

rep_dz <- d.dep.t.diff.t(t = normal_replication_ttest$statistic, n = desc_normal$count[1], a = 0.05)
rep_dz

rep_dav <- d.dep.t.avg(m1 = desc_normal$mean[2], m2 = desc_normal$mean[1], 
                       sd1 = desc_normal$sd[2], sd2 = desc_normal$sd[1], 
                       n = desc_normal$count[1], a = 0.05)
rep_dav

## Calculate Original ES --------

#Original descriptives

ori_values <- data.frame(
  ori_pval = 0.00099,
  ori_N = 18,
  reported_es = 1.35,
  t_val = 5.14,
  ori_m1 = 2.82,
  ori_sd1 = 0.69,
  ori_m2 = 2.09,
  ori_sd2 = 0.41)

# Estimating the original effect size

ori_dz <- d.dep.t.diff.t(t = ori_values$t_val, n = ori_values$ori_N, a = 0.05)
ori_dz

ori_dav <- d.dep.t.avg(m1 = ori_values$ori_m1, m2 = ori_values$ori_m2, 
                       sd1 = ori_values$ori_sd1, sd2 = ori_values$ori_sd2, 
                       n = ori_values$ori_N, a = 0.05)
ori_dav

# Z-test (dz) --------

rep_test <- compare_smd(
  smd1 = ori_dz$d,
  n1 = ori_values$ori_N,
  smd2 = rep_dz$d,
  n2 = desc_normal$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

# Z-test (reported es) --------

rep_test <- compare_smd(
  smd1 = ori_values$reported_es,
  n1 = ori_values$ori_N,
  smd2 = rep_dav$d,
  n2 = desc_normal$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test
