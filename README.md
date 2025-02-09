# SDG-Final-Project
Analyzing development towards SDGs using data by UN
---
title: "UNICEF Project Exploration - STA130 Winter 2024"
author: "Nikita Jain, Anish Pai, Anudari Jamsran
"
output:
  pdf_document: default
---


# Final Project Overview: Identifying Opportunities to Accelerate Progress on Sustainable Development Goals (SDG)

## Guiding Research Question

How do landlocked countries compare with small island nations in their advancement toward meeting the UN’s Sustainable Development Goals (SDGs)?

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(rpart)
library(partykit)
library(grid)
library(readr)
library(dplyr)
```

Research Question 1:

## Specific Research Question

Can we classify countries as having high or low economic growth based on different economic empowerment metrics (like education and time use), and which country type (landlocked vs. small island) has a lower error rate in this classification (higher accuracy)?

## Data Wrangling and Cleaning

```{r}
# Read in country codes data, select relevant columns for further analysis, drop any rows with missing values, and filter for developing countries only. Rename the ISO-alpha3 code column for consistency.
names <- read_csv("country_codes.csv")
names_ll <- names %>%
  select("ISO-alpha3 Code (M49)", "Country or Area_en (M49)", "Developed / Developing Countries (M49)", "Land Locked Developing Countries (LLDC) (M49)") %>% drop_na() %>% filter(`Developed / Developing Countries (M49)`=="Developing") %>% rename(con_codes = "ISO-alpha3 Code (M49)")

# Similar to above, but for Small Island Developing States (SIDS) instead of landlocked countries.
names_si <- names %>%
  select("ISO-alpha3 Code (M49)", "Country or Area_en (M49)", "Developed / Developing Countries (M49)", "Small Island Developing States (SIDS) (M49)") %>% drop_na() %>% filter(`Developed / Developing Countries (M49)`=="Developing") %>% rename(con_codes = "ISO-alpha3 Code (M49)")

```

```{r}
# Load in SDG goal scores, select relevant columns, drop rows with missing values, and rename the country code column for consistency. Calculate the average score of Goal 8 (Decent Work and Economic Growth) across all countries.
sdg <- read_csv("sdr_fd5e4b5a.csv") %>% select("Goal 8 Score", "country_label", "Country Code ISO3") %>% drop_na() %>% rename(con_codes = "Country Code ISO3")

# Perform inner joins to match country codes from the names_ll and names_si datasets with their corresponding SDG scores.
namell_sdg <- inner_join(x=names_ll, y=sdg, by="con_codes")
namesi_sdg <- inner_join(x=names_si, y=sdg, by="con_codes")
```
```{r}
# Read in country indicators, select relevant columns for gender-related economic empowerment, drop rows with missing values, and rename the country code column for consistency.
indicators <- read_csv("country_indicators.csv") %>% select("sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_male", "sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_female", "sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_male_total", "sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_female_total", "sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_male_total", "sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_female_total", "iso3") %>% rename(con_codes = "iso3") %>% drop_na()

# Merge the country indicators with the SDG scores for both landlocked and small island countries, excluding irrelevant columns.
data_land_locked <- inner_join(x=namell_sdg, y=indicators, by="con_codes") %>% select(-`Developed / Developing Countries (M49)`, -`Land Locked Developing Countries (LLDC) (M49)`, -`country_label`)
data_small_island <- inner_join(x=namesi_sdg, y=indicators, by="con_codes") %>% select(-`Developed / Developing Countries (M49)`, -`Small Island Developing States (SIDS) (M49)`, -`country_label`)
```
```{r}
# Calculate the mean Goal 8 Score for both small island and landlocked country groups. This mean will be used to classify countries' economic growth as 'good' or 'bad' based on their Goal 8 Score relative to the group averages.
smallmean = mean(data_small_island$`Goal 8 Score`, na.rm = TRUE)
landmean = mean(data_land_locked$`Goal 8 Score`, na.rm = TRUE)

# For landlocked countries, classify economic growth, calculate average educational attainment, labor force participation, and unemployment rates using specific indicators. Repeat the same process for small island countries.
data_land_locked <- data_land_locked %>%
  mutate(EconomicGrowthLabel = ifelse(`Goal 8 Score` >= ((smallmean + landmean) / 2), 'good', 'bad')) %>% 
  mutate(AvgEducationalAttainment = (`sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_male` + `sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_female`) / 2) %>%
  mutate(AvgLaborForceParticipation = (`sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_male_total` +
                                      `sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_female_total`) / 2)%>%
  mutate(AvgUnemploymentRate = (`sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_female_total` +
                                      `sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_female_total`) / 2)
  
data_small_island <- data_small_island %>%
  mutate(EconomicGrowthLabel = ifelse(`Goal 8 Score` >= ((smallmean + landmean) / 2), 'good', 'bad')) %>% mutate(AvgEducationalAttainment = (`sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_male` + 
                                     `sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_female`) / 2) %>%
  mutate(AvgLaborForceParticipation = (`sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_male_total` +
                                      `sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_female_total`) / 2) %>%
  mutate(AvgUnemploymentRate = (`sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_female_total` +
                                      `sowc_women-s-economic-empowerment__unemployment-rate-2010-2020-r_female_total`) / 2)


```


## Tree Creation
```{r}
# Build a classification tree model (tree_model1) for landlocked countries. The model predicts the EconomicGrowthLabel (good or bad) based on average educational attainment, average unemployment rate, and average labor force participation. The control parameters are set to fine-tune the tree's complexity and prevent overfitting.
tree_model1 <- rpart(EconomicGrowthLabel ~ 
                       AvgEducationalAttainment + AvgUnemploymentRate 
                     + AvgLaborForceParticipation,
                    data = data_land_locked,
                    method = "class",
                    control = rpart.control(cp = 0.1, 
                                            minsplit = 5, 
                                            minbucket = 1, 
                                            maxdepth = 30))

# Similarly, build a classification tree model (tree_model2) for small island countries using the same predictors and control settings. This model helps in understanding how economic growth classification correlates with the selected predictors in small island contexts.

tree_model2 <- rpart(EconomicGrowthLabel ~ 
                       AvgEducationalAttainment + 
                       AvgUnemploymentRate + AvgLaborForceParticipation,
                    data = data_small_island,
                    method = "class",
                    control = rpart.control(cp = 0.1, 
                                            minsplit = 5, 
                                            minbucket = 1, 
                                            maxdepth = 30))
```


## Visualizations
```{r}
# Visualize the classification tree (tree_model1) for landlocked countries. Two types of plots are generated: a simple plot for a straightforward view of the tree and an extended plot for a detailed view including split criteria and node information.
plot(as.party(tree_model1), type = "simple", gp=gpar(cex=0.8), main = "Land Locked Economic Classification")
plot(as.party(tree_model1), type = "extended", gp=gpar(cex=0.8), main = "Land Locked Economic Classification, Extended")

# Visualize the classification tree (tree_model2) for small island countries in the same manner as for landlocked countries. These visualizations aid in comparing the decision-making process of the models between the two types of geographical entities.
plot(as.party(tree_model2), type = "simple", gp=gpar(cex=0.8), main = "Small Island Economic Classification")
plot(as.party(tree_model2), type = "extended", gp=gpar(cex=0.8), main = "Small Island Economic Classification, Extended")
```

Research Question 2

```{r}
# load in country indicators
country_indicators <- 
  read_csv("country_indicators.csv") %>%
  select(-...1) %>%  # remove first column
  select(iso3, everything()) %>%  # reorder the columns to put iso3 as column 1
  rename(country_code_iso3 = iso3)  # rename first column to country_code_iso3

# preview data
country_indicators
```

We see in addition to the country codes, which we have called `country_code_iso3`, there is also a whole host of additional information on each country. A list of codes from above is printed out below.

```{r}
country_indicators$country_code_iso3
```

Next let's take a look at the Sustainable Development Report's SDG Index data.

```{r}
# load SDG data
sdg <- 
  read_csv("sdr_fd5e4b5a.csv") %>%
  select(-...1)  # remove first column

# rename columns
names(sdg)[1:(2*17)] <- 
  paste(c(rep(paste("goal_", 1:17, sep=""), each=2)), 
        rep(c("_status", "_trend"), times=17), sep="")
names(sdg)[(2*17 + 1):(3*17)] <- 
  paste("goal_", 1:17, "_score", sep="")
names(sdg)[names(sdg)=="2023 SDG Index Score"] <- 
  "SDG_index_score_2023"
names(sdg)[names(sdg)=="2023 SDG Index Rank"] <- 
  "SDG_index_rank_2023"
names(sdg)[names(sdg)=="Percentage missing values"] <- 
  "percentage_missing_values"
names(sdg)[names(sdg)=="International Spillovers Score (0-100)"] <- 
  "international_spillover_score"
names(sdg)[names(sdg)=="International Spillovers Rank"] <- 
  "international_spillover_rank"
names(sdg)[names(sdg)=="Country Code ISO3"] <- 
  "country_code_iso3"

# preview data
sdg
```

Joining the two datas together

```{r}
# join tables
data <- inner_join(x=country_indicators, y=sdg, by="country_code_iso3")

# preview data
data
```

\newpage

## Research Question 2: SDG 6

Goal 6: Clean water and sanitation Are people from landlocked countries using more clean waters compared to small island country people?

```{r}
# From country_codes file select relevant columns for SDG 6
country_codes <- read_csv("country_codes.csv") %>%
  select("ISO-alpha3 Code (M49)", 
         "Land Locked Developing Countries (LLDC) (M49)", 
         "Small Island Developing States (SIDS) (M49)")

filtered_countries <- country_codes %>%
  filter(`Land Locked Developing Countries (LLDC) (M49)` == "TRUE" | 
           `Small Island Developing States (SIDS) (M49)` == "TRUE")

print(filtered_countries)
```

```{r}
# For SDG 6 select needed data
data1 <- read_csv("country_indicators.csv") %>%
  select(-...1) %>% 
  select(iso3, "sowc_wash__households-2020_at-least-basic-drinking-water-services_total",
         "sowc_wash__households-2020_at-least-basic-drinking-water-services_urban",
         "sowc_wash__households-2020_at-least-basic-drinking-water-services_rural") %>%  
  rename(country_code_iso3 = iso3)  

# Inner join data1 with filtered_countries
data2 <- inner_join(data1, filtered_countries, 
                    by = c("country_code_iso3" = "ISO-alpha3 Code (M49)"))

print(data2)
```

```{r}
# Select SDG goal 6 score from sdg dataset
sdg_goal_6 <- sdg %>%
  select(country_code_iso3, goal_6_score)

data3 <- left_join(data2, sdg_goal_6, by = "country_code_iso3")
names(data3)[names(data3) == "sowc_wash__households-2020_at-least-basic-drinking-water-services_total"] <- 
  "clean_water_usage_total"
names(data3)[names(data3) == "sowc_wash__households-2020_at-least-basic-drinking-water-services_urban"] <- 
  "clean_water_usage_urban"
names(data3)[names(data3) == "sowc_wash__households-2020_at-least-basic-drinking-water-services_rural"] <- 
  "clean_water_usage_rural"
print(data3)
```

```{r}
seed <- 230  
set.seed(seed)  

n_trials <- 10000  
n_sample <- 100  

# Simulate clean water usage
clean_water_usage_simulations <- numeric(n_trials)  

for (i in 1:n_trials) {
  clean_water_usage <- runif(n_sample, min = 0, max = 100)  

  mean_clean_water_usage <- mean(clean_water_usage)
  
  clean_water_usage_simulations[i] <- mean_clean_water_usage
}
```

\
**Hypothesis testing**

```{r}
data3 <- data3 %>%
  mutate(Group = case_when(
    `Land Locked Developing Countries (LLDC) (M49)` == TRUE ~ "Landlocked",
    `Small Island Developing States (SIDS) (M49)` == TRUE ~ "Small Island",
    TRUE ~ "Other"
  ))

# Filter data to include only landlocked and small island countries
data3 <- data3 %>%
  filter(Group %in% c("Landlocked", "Small Island"))
data3 <- data3 %>%
  filter(!is.na(clean_water_usage_total), is.finite(clean_water_usage_total))

# Perform two-sample t-test
t_test_water <- t.test(clean_water_usage_total ~ Group, data = data3)

print(t_test_water)
print(data3)
```

**Visualizations**
```{r}
# Visualization of the mean clean water usage simulations using histograms
ggplot(data.frame(mean_clean_water_usage = clean_water_usage_simulations, 
                  Group = rep(c("Landlocked", "Small Island"), 
                              each = n_trials/2)), 
       aes(x = mean_clean_water_usage, fill = Group)) +  
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7, color = "black") +
  labs(title = "Distribution of Mean Clean Water Usage",
       x = "Mean Clean Water Usage",
       y = "Frequency") +
  facet_wrap(~ Group, scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c("Landlocked" = "yellow", "Small Island" = "blue"))  

# Visualize using boxplots
ggplot(data.frame(mean_clean_water_usage = clean_water_usage_simulations, 
                  Group = rep(c("Landlocked", "Small Island"), 
                              each = n_trials/2)), 
       aes(x = Group, y = mean_clean_water_usage, fill = Group)) + 
  geom_boxplot() +
  labs(title = "Comparison of Mean Clean Water Usage",
       x = "Group",
       y = "Mean Clean Water Usage") +
  theme_minimal() +
  scale_fill_manual(values = c("Landlocked" = "yellow", "Small Island" = "blue"))  
```

```{r}
mean_difference_simulations <- numeric(n_trials)

# Simulate clean water usage for Landlocked countries
clean_water_landlocked <- matrix(runif(n_trials * n_sample, min = 0, max = 100), ncol = n_sample)

# Simulate clean water usage for Small Island countries
clean_water_small_island <- matrix(runif(n_trials * n_sample, min = 0, max = 100), ncol = n_sample)

for (i in 1:n_trials) {
  # Calculate mean clean water usage for Landlocked and Small Island countries
  mean_landlocked <- mean(clean_water_landlocked[i, ])
  mean_small_island <- mean(clean_water_small_island[i, ])
  mean_difference_simulations[i] <- mean_landlocked - mean_small_island
}

# Plotting histogram
ggplot(data.frame(mean_difference = mean_difference_simulations), 
       aes(x = mean_difference)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray", alpha = 0.7) +
  labs(title = "Differences in Two-Sample Means for Clean Water Usage",
       x = "Difference in Means (Landlocked - Small Island)",
       y = "Frequency") +
  theme_minimal()

```

Research Question 3
## Specific Research Question

Do countries show higher/lower gender inequality due to their geographic categorization as landlocked or small islands based on the mean Women Empowerment Index scores?

## Data Wrangling and Cleaning

```{r}


# load in country indicators with required variables
country_indicators <- 
  read_csv("country_indicators.csv") %>%
  select(
    'iso3',
    'hdr_pr_f_2021',
    'sowc_maternal-and-newborn-health__demand-for-family-planning-satisfied-with-modern-methods-2016-2021-r_service-coverage-sub-index-on-reproductive-maternal-newborn-and-child-health',
    'sowc_adolescent-health__adolescent-birth-rate-2016-2021-r_aged-15-19_female',
    'sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_female',
    'sowc_adolescents__transition-to-work-2013-2021-r_not-in-education-employment-or-training_female',
    'sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_female_total',
    'sowc_women-s-economic-empowerment__financial-inclusion-2014-2020-r_female_female',
    'sowc_adolescents__protection_intimate-partner-violence-2013-2020-r_female') %>% 
  select(iso3, everything()) %>%  
  rename(con_code = iso3) 

# preview data
country_indicators
```

sdg file

```{r}
# load SDG data and select necessary variable
sdg <- 
  read_csv("sdr_fd5e4b5a.csv") %>%
  select('Country Code ISO3') %>% rename(con_code = 'Country Code ISO3')

# preview data
sdg
```


country_codes file

```{r}
# load country_codes data and select variables
country_codes <- 
  read_csv("country_codes.csv") %>%
  select('ISO-alpha3 Code (M49)','Small Island Developing States (SIDS) (M49)', 
         'Land Locked Developing Countries (LLDC) (M49)') %>% 
  rename(con_code = 'ISO-alpha3 Code (M49)')

country_codes
```

## Data Integration

2- sample, 2-sided hypothesis testing

Null Hypothesis- There is no difference in the mean Women Empowerment Index scores between Landlocked and Small Islands
$H_0$: $g_1 = g_2$ and $g_1 - g_2 = 0$  

Alternate Hypothesis- There is a difference in the mean Women Empowerment Index scores between Landlocked and Small Islands
$H_1$: $g_1 != g_2$ and $g_1 - g_2 != 0$

The set significance level (alpha level) would be set to $\alpha$ = 0.05. Null hypothesis would be rejected if p <= $\alpha$

```{r}

# Integrating data from all three data sets using con_code as a common key

wei_rough_data <- inner_join(x=country_codes, y=country_indicators, by="con_code")
wei_clean <- inner_join(x=wei_rough_data, y=sdg, by="con_code")

# Renaming columns and removing extra variables

wei_rename <- wei_clean %>% 
  mutate(country_type = case_when(
      `Small Island Developing States (SIDS) (M49)` == TRUE ~ "Small Island",
      `Land Locked Developing Countries (LLDC) (M49)` == TRUE ~ "Land Locked",)) %>% 
  rename(
    MMC ='sowc_maternal-and-newborn-health__demand-for-family-planning-satisfied-with-modern-methods-2016-2021-r_service-coverage-sub-index-on-reproductive-maternal-newborn-and-child-health',
    ABR = 'sowc_adolescent-health__adolescent-birth-rate-2016-2021-r_aged-15-19_female',
    CSE = 'sowc_women-s-economic-empowerment__educational-attainment-2008-2021-r_upper-secondary_female',
    NEET = 'sowc_adolescents__transition-to-work-2013-2021-r_not-in-education-employment-or-training_female',
    LFPR = 'sowc_women-s-economic-empowerment__labour-force-participation-rate-2010-2020-r_female_total',
    FI = 'sowc_women-s-economic-empowerment__financial-inclusion-2014-2020-r_female_female',
    PR = hdr_pr_f_2021,
    IVP = 'sowc_adolescents__protection_intimate-partner-violence-2013-2020-r_female') %>% 
  select(con_code, country_type, everything(), -"Land Locked Developing Countries (LLDC) (M49)", 
         -"Small Island Developing States (SIDS) (M49)") 

# replacing missing values of FI and IVP with their mean scores
wei_rename_fill <- wei_rename %>% mutate(
    FI = if_else(is.na(FI), mean(FI, na.rm = TRUE), FI),
    IVP = if_else(is.na(IVP), mean(IVP, na.rm = TRUE), IVP)) %>% drop_na()


wei_rename_fill
```

# Creating Women Empowerment index-
```{r}

# Setting maximum and minimum values for each indicator taken from 
# Technical Note: Twin Indices on Women’s Empowerment and Gender Equality

min_MMC = 0
min_ABR = 0
min_CSE = 0
min_NEET = 0
min_LFPR = 0
min_FI = 0
min_PR = 0
min_IVP = 0

max_MMC = 100
max_ABR = 200
max_CSE = 100
max_NEET = 85
max_LFPR = 100
max_FI = 100
max_PR = 75
max_IVP = 60

# Normalizing each variable to help with comparison
# All variables are positive indicators (higher values indicate better performance in 
# that field) except ABR, NEET, IPV

wei_data <- wei_rename_fill %>% 
  mutate(
    
    # Normalizing positive indicators
    norm_MMC  = (MMC - min_MMC)/(max_MMC - min_MMC),
    norm_FI   = (FI - min_FI)/(max_FI - min_FI),
    norm_PR   = (PR - min_PR)/(max_PR - min_PR),
    norm_CSE  = (CSE - min_CSE)/(max_CSE - min_CSE),
    norm_LFPR = (LFPR - min_LFPR)/(max_LFPR - min_LFPR),
    
    # Normalizing negative indicators
    norm_ABR  = (max_ABR - ABR)/(max_ABR - min_ABR),
    norm_NEET = (max_NEET - NEET)/(max_NEET - min_NEET),
    norm_IVP  = (max_IVP - IVP)/(max_IVP - min_IVP))

# Calculation of dimension indices
wei_data <- wei_data %>% 
  mutate(
    I_health    = (norm_MMC + norm_ABR) / 2,
    I_education = (norm_CSE + norm_NEET) / 2,
    I_inclusion = (norm_LFPR + norm_FI) / 2,
    I_decision  = norm_PR,
    I_violence  = norm_IVP)
  

# Computing Women Empowerment Index (WEI is a positive index)
wei <- wei_data %>% select(-MMC, -ABR, -CSE, -NEET, -LFPR, -FI, -PR,-IVP) %>%
  mutate(WEI = (I_health * I_education * I_inclusion * I_decision * I_violence) ^ (1/5))

wei

```

# Calculating the observed test statistic- 
$\Delta\hat{g}$ will be the difference in the mean scores of WEI of landlocked and small island countries

```{r}
ghat <- wei %>% 
  group_by(country_type) %>%
  summarise(means = mean(WEI)) 
ghat

delta_ghat <- 
  wei %>% 
  group_by(country_type) %>%
  summarise(means = mean(WEI)) %>%
  summarise(value = diff(means)) %>%
  as.numeric()

print(delta_ghat)
```

\newpage
Below is R code that simulates $N = 1000$ values of the test statistic $\Delta\hat{g}_{\rm sim}$ **under the null hypothesis** using a permutation test. In this test, we assume that our groups are identical under our null hypothesis. Mixing the two groups together, randomly generating new groups with the same sizes, and then recomputing our test statistic each time therefore should allow us to simulate values from the sampling distribution provided our sample size is large enough.

```{r}
seed_num <- 130
set.seed(seed_num)  # creating seed

# setup
n_trials <- 1000  # number of permutations

# simulating test statistic (difference between mean scores of WEI)
delta_ghat_simulations <- rep(NA, n_trials)

for(i in 1:n_trials){
  # perform a random permutation
  simdata <- 
    wei %>%
    mutate(country_type = sample(country_type, replace=FALSE))
  
  # compute the simulated test statistic
  delta_ghat_sim <- 
    simdata %>% 
    group_by(country_type) %>%
    summarise(means = mean(WEI), .groups="drop") %>%
    summarise(value = diff(means)) %>%
    as.numeric()
  
  # store the simulated value
  delta_ghat_simulations[i] <- delta_ghat_sim
}

```

# Visualizations-

```{r}
# Visualizing sampling distribution of simulated test statistics using histograms

ggplot() +
  geom_histogram(aes(x=delta_ghat_simulations), color = "purple", fill = "purple", 
                 position = "Identity", alpha = 0.7, bins= 20) +
  labs(x = "Difference in Women Empowerment Index scores (delta_ghat_simulations)", 
       y = "Frequency",
       title = "Differences in 2-Sample Means for Gender Disparity Measurements") +
  theme_minimal() +
  theme(legend.position = "top")

# Creating box plots to compare the median WEI scores

ggplot(data = wei, aes(x = country_type, y = WEI, fill = country_type)) +
  geom_boxplot() +
  labs(x = "Country Type", y = "Women Empowerment Index Scores", 
       title = "Comparison of Women Empowerment Index Scores by Country Type") +
  scale_fill_manual(values = c("orange", "purple")) +  
  theme_minimal() +
  theme(legend.position = "top")

```


# Computing the p-value- 
(the probability of observing a test statistic at least as extreme as the observed value if the null hypothesis is true)
```{r}
# null hypothesis value
delta_median_null <- 0

p_value <-sum(abs(delta_ghat_simulations - delta_median_null) >= 
                abs(delta_ghat - delta_median_null)) / n_trials
print(p_value)

```



# Citations (MLA 9th edition)

1. Jain-Chandra, Sonali. “Chapter 2. Gender Inequality around the World.” Www.elibrary.imf.org, International Monetary Fund, 
www.elibrary.imf.org/display/book/9781513516103/ch002.xml#:~:text=The%20gender
%20gap%20varies%20strongly.  

2. TOWARDS IMPROVED MEASURES of GENDER INEQUALITY: An Evaluation of the UNDP Gender Inequality Index and a Proposal. 
www.unwomen.org/sites/default/files/2022-11/Discussion-paper-Towards-improved-measures-of-gender-inequality-en.pdf.   

3. Technical Note: Twin Indices on Women’s Empowerment and Gender Equality.
hdr.undp.org/sites/default/files/
publications/additional-files/2023-07/paths_equal_2023_tn.pdf.  

4. Duflo, Esther. "Women Empowerment and Economic Development." *Journal of Economic Literature*, vol. 50, no. 4, 2012, pp. 1051-79.
