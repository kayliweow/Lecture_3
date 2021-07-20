## ============================================================================== ##
# FUNCTION : LECTURE 3 - BACK-TO-BASICS.R ------------------------------------------
# BY       : Ankit Patel -----------------------------------------------------------
# DATED    : 20 JULY 2020 ----------------------------------------------------------
# PURPOSE  : Correlation vs. Covariance --------------------------------------------
## ============================================================================== ##


# 1. Preparing Enviornment - Load Data & Packages -------------------------------------------
## Load relevant packages for this lecture
#install.packages(c("corrplot", "RColorBrewer", "rstatix"))
pacman::p_load("tidyverse", "outbreaks", "corrplot", "RColorBrewer", "rstatix")

## Setting working directory
setwd(paste0("C:\Users\64273\Documents\Ankit Patel\Google Drive\Victoria University\Victoria University",
             "\Victoria University 2020\HLWB303\course material\2021\Lecture 3", sep = ""))

## Load the diet data
diet_data <- read_csv("Diet.csv", col_names = TRUE) ##load fat data (252 observations and 19 variables)



# 2. SLIDE 6 and 7: Data preparation and manipulation ---------------------------------------
attach(diet_data) ## attach df to this session
## Data fun facts
class(diet_data)
names(diet_data) ## columns available
lapply(diet_data, class) ## Column types


## Recoding gender, calculate weight lost, and change diet and gender type
diet_data <- diet_data %>%
  mutate(gender_name = ifelse(gender == 1, "Male", "Female")) %>%
  mutate(diet_type = ifelse(Diet == 1, "Diet A", 
                            ifelse(Diet == 2, "Diet B", "Diet C"))) %>%
  mutate(weight_lost = pre.weight - weight6weeks) %>% ## calculate weight lost
  drop_na() ## remove na rows
  
  
lapply(diet_data, class) ## Column types
attach(diet_data) ## attach df to this session



# 3. SLIDE 8: Central tendency --------------------------------------------------------------

lapply(diet_data, class) ## Column types
## Mean of Age, Height and weight
diet_data %>% dplyr::summarise(mean_age = mean(Age, na.rm=TRUE))
#mean_age
#<dbl>
#  1     39.2

diet_data %>% dplyr::summarise(mean_height = mean(Height, na.rm=TRUE))
#mean_height
#<dbl>
#  1        171.

diet_data %>% dplyr::summarise(mean_pre_weight = mean(pre.weight, na.rm=TRUE))
#mean_pre_weight
#<dbl>
#  1            72.5



## Median of Age, Height and weight
diet_data %>% dplyr::summarise(median_age = median(Age, na.rm=TRUE))
#median_age
#<dbl>
#  1         39

diet_data %>% dplyr::summarise(median_height = median(Height, na.rm=TRUE))
#median_height
#<dbl>
#  1          170.

diet_data %>% dplyr::summarise(median_height = median(pre.weight, na.rm=TRUE))
#median_height
#<dbl>
#  1            72



## Calculating the mean and median by group
## Mean Age by gender
diet_data %>% group_by(gender) %>%
  dplyr::summarise(mean_age = mean(Age, na.rm=TRUE))
#gender mean_age
#<dbl>    <dbl>
#  1      0     39.1
#  2      1     39.4


## Median Age by gender
diet_data %>% group_by(gender) %>%
  dplyr::summarise(median_age = median(Age, na.rm=TRUE))
#gender median_age
#<dbl>      <dbl>
#  1      0       37  
#  2      1       39  


## Conducting a 'summary' of the data
summary(diet_data)  ## cheap and easy way of EDA



# 3. SLIDE 9: Standard deviation and variance -----------------------------------------------

## Standard deviation of Height
diet_data %>% dplyr::summarise(sd_height = sd(Height, na.rm=TRUE))
#sd_height
#<dbl>
#  1      11.4

## Standard deviation of Weight
diet_data %>% dplyr::summarise(sd_height = sd(pre.weight, na.rm=TRUE))
#sd_height
#<dbl>
#  1      7.97

## Variance of Height and weight
diet_data %>% dplyr::summarise(variance_height = var(Height, na.rm=TRUE))
#variance_height
#<dbl>
#  1            130.

## Variance of weight
diet_data %>% dplyr::summarise(variance_height = var(pre.weight, na.rm=TRUE))
#variance_height
#<dbl>
#  1            63.6


## Distribution of Height and Weight
ggplot(diet_data, aes(Height)) + 
  geom_bar(colour = "red", fill = "red") + 
  geom_density(alpha=20)

ggplot(diet_data, aes(pre.weight)) + 
  geom_bar(colour = "red", fill = "red") + 
  geom_density(alpha=20)


## Distribution of Height by gender
ggplot(diet_data, aes(x = Height, fill = gender_name)) + 
  geom_density(alpha = 0.5)


## Distribution of Weight by gender
ggplot(diet_data, aes(x = pre.weight, fill = gender_name)) + 
  geom_density(alpha = 0.5)




# 3. SLIDE 11 and 12: Corrrelation ----------------------------------------------------------
## Correlation between columns
attach(diet_data)
cor(Age, Height, method = "pearson") ## correlation of 0.0834 correlation

## Let's identify the most significant correlations
names(diet_data)
diet_data_filter <- diet_data %>% select(-c(Person, gender, Diet, gender_name,
                                            diet_type)) ## filtering out relevant columns
corrplot(cor(diet_data_filter,method="p"), method = "number")




# 4. SLIDE 7: Covariance --------------------------------------------------------------------
## Covariance between variables
cov(Height, pre.weight) 
cov(Height, weight6weeks) 
cov(pre.weight, weight6weeks) 