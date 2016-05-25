#### Created By Daniel Hadley Wed May 25 13:50:52 EDT 2016 #####
## A classification contest: using x vars, predict the type of crime ##
# This script will load data created in the engineer_features and build the final model
setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2

library(dplyr)
library(tidyr)
library(stringr)
library(xgboost) # XGboost approach
set.seed(543)
