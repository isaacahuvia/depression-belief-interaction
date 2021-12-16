###################
##  Sample Code  ##
###################

####  Startup  ####
## Load packages
library(tidyverse)
library(assertr)
library(here)

#test3

####  Load Data  ####
# The here() package returns filepaths based on the arguments you give it,
# starting from the folder with your .Rproj file; in this case, it will return
# the filepath to the dataset inside the "data" folder
# This is especially useful when a script will be run on Windows and Mac 
# computers, as they use different folder separators ("/" vs "\")
df <- readRDS(here("data", "T2T Data for Jans et al.rds"))



####  Prepare Data  ####
df <- df %>%
  
  # Create new variables
  mutate(race_recoded = "...") %>%
  
  # Confirm no missing values
  verify(!is.na(yb_cdi_1)) %>% #see vignette("assertr")
  
  # Select only variables needed for analysis
  select(everything())



####  Analysis  ####
## Regression
my_model <- lm(data = df, yb_cdi_1 ~ yb_cdi_2 + yb_cdi_3)
summary(my_model)


## Plots
df %>%
  ggplot() +
  geom_histogram(aes(yb_cdi_1))
