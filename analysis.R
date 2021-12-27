###################
##  12/27/21  ##
###################

####  Startup  ####

# Load packages

if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(here)){install.packages('here')}
if(!require(ggplot2)){install.packages('ggplot2')}

library(tidyverse)
library(here)
library(ggplot2)


#### Load data ####

# The here() package returns filepaths based on the arguments you give it,
# starting from the folder with your .Rproj file; in this case, it will return
# the filepath to the dataset inside the "data" folder
# This is especially useful when a script will be run on Windows and Mac 
# computers, as they use different folder separators ("/" vs "\")

df_1 <- readRDS(here("data", "T2T Data for Jans et al.rds"))


####  Prepare Data  ####

  # Adding a column for CDI mean

  df_2 <- df_1 %>% mutate(cdi_mean = rowMeans(select(.,contains("cdi")), na.rm = FALSE))
  
  # Selecting variables
  
  df_3 <- df_2 %>% select(yb_lsmh_id, yb_permanence, yb_cause_brain, yb_cause_env, yb_change_brain, yb_change_env, cdi_mean)
  
  # Removing rows containing NA
  
  df_4 <- df_3 %>% filter(complete.cases(.))
  

####  Analysis  ####

## Regression with CDI as covariate

my_model_1 <- lm(data = df_4, yb_permanence ~ yb_cause_brain + yb_change_brain + yb_cause_brain*yb_change_brain + cdi_mean)
summary(my_model_1)

my_model_2 <- lm(data = df_4, yb_permanence ~ yb_cause_env + yb_change_env + yb_cause_env*yb_change_env + cdi_mean)
summary(my_model_2)

## Regression without CDI

my_model_3 <- lm(data = df_4, yb_permanence ~ yb_cause_brain + yb_change_brain + yb_cause_brain*yb_change_brain)
summary(my_model_3)

my_model_4 <- lm(data = df_4, yb_permanence ~ yb_cause_env + yb_change_env + yb_cause_env*yb_change_env)
summary(my_model_4)

## Plots

interaction.plot(x.factor=df_4$yb_cause_brain, trace.factor=df_4$yb_change_brain, response=df_4$yb_permanence)

interaction.plot(x.factor=df_4$yb_cause_env, trace.factor=df_4$yb_change_env, response=df_4$yb_permanence)

## Plots grouping permanence variable into high, low, and median malleability 
#(to create an interaction plot more similar to the pre-registration)

median_brain_malleability <- median(df_4$yb_change_brain)

median_env_malleability <- median(df_4$yb_change_env)

df_5 <- df_4 %>% mutate(malleability_brain = case_when(yb_change_brain < median_brain_malleability ~ "low", 
                                                 yb_change_brain > median_brain_malleability ~ "high",
                                                 yb_change_brain == median_brain_malleability ~ "median"))

df_6 <- df_5 %>% mutate(malleability_env = case_when(yb_change_env < median_env_malleability ~ "low", 
                                                     yb_change_env > median_env_malleability ~ "high",
                                                     yb_change_env == median_env_malleability ~ "median"))

interaction.plot(x.factor=df_6$yb_cause_brain, trace.factor=df_6$malleability_brain, response=df_6$yb_permanence)

interaction.plot(x.factor=df_6$yb_cause_env, trace.factor=df_6$malleability_env, response=df_6$yb_permanence)

## Generating histograms of each variable

ggplot(df_6, aes(yb_permanence)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression permanence belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_6$yb_permanence), sd = sd(df_6$yb_permanence)), col="red")

ggplot(df_6, aes(yb_cause_brain)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression caused by brain belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_6$yb_cause_brain), sd = sd(df_6$yb_cause_brain)), col="red")

ggplot(df_6, aes(yb_cause_env)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression caused by environment belief")+ 
  stat_function(fun=dnorm,args = list(mean = mean(df_6$yb_cause_env), sd = sd(df_6$yb_cause_env)), col="red")

ggplot(df_6, aes(yb_change_brain)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Brain can change belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_6$yb_change_brain), sd = sd(df_6$yb_change_brain)), col="red")

ggplot(df_6, aes(yb_change_env)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Environment can change belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_6$yb_change_env), sd = sd(df_6$yb_change_env)), col="red")

## Checking for multicollinearity

ggplot(df_6, aes(yb_cause_brain, yb_change_brain)) + geom_jitter() + geom_smooth()
cor(df_6$yb_cause_brain, df_6$yb_change_brain)

ggplot(df_6, aes(yb_cause_env, yb_change_env)) + geom_jitter() + geom_smooth()
cor(df_6$yb_cause_env, df_6$yb_change_env)

ggplot(df_6, aes(yb_cause_brain, cdi_mean)) + geom_jitter() + geom_smooth()
cor(df_6$yb_cause_brain, df_6$cdi_mean)

ggplot(df_6, aes(yb_cause_env, cdi_mean)) + geom_jitter() + geom_smooth()
cor(df_6$yb_cause_env, df_6$cdi_mean)

ggplot(df_6, aes(yb_change_brain, cdi_mean)) + geom_jitter() + geom_smooth()
cor(df_6$yb_change_brain, df_6$cdi_mean)

ggplot(df_6, aes(yb_change_env, cdi_mean)) + geom_jitter() + geom_smooth()
cor(df_6$yb_change_env, df_6$cdi_mean)


#### Summary Statistics ####

  # changing character variables into factors for summary function

  df_2$pb_childgender <- as.factor(df_2$pb_childgender)
  df_2$pb_income <- as.factor(df_2$pb_income)
  df_2$pb_childethnicity <- as.factor(df_2$pb_childethnicity)

  # Creating new df and running summary function

  df_for_summary <- df_2 %>% select(pb_childgender, pb_income, pb_childethnicity, yb_permanence, pb_childage, 
                                  yb_cause_brain, yb_cause_env, yb_change_brain, yb_change_env, cdi_mean)

  print(summary(df_for_summary))