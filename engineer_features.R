#### Created By Daniel Hadley Wed May 25 13:49:20 EDT 2016 ####
## A classification contest: using x vars, predict the type of crime ##
setwd("~/Github/2015_SF_Crime_Classification/")
setwd("C:/Users/dhadley/Documents/GitHub/2015_SF_Crime_Classification/")
setwd("/home/rstudio/Dropbox/2015_Kaggle_SF/") #Amazon EC2


library(dplyr)
library(tidyr)
library(stringr)
set.seed(543)


train <- read.csv("./train.csv")
# sample <- read.csv("./sampleSubmission.csv")
test <- read.csv("./test.csv")
# Only load test when needed




#### Processing data ####

#I found the dates to be a bit messy in the file.
#Comes with the job though.

#Adjust the formatting of the text for Dates.
test$Dates <- as.character(test$Dates)
train$Dates <- as.character(train$Dates)

#Make Hour
test$Hour<- as.matrix(as.numeric(substring(test$Dates, 12, 13)))
train$Hour<- as.matrix(as.numeric(substring(train$Dates, 12, 13)))

#Make Year 
test$Year<- as.matrix(as.numeric(substring(test$Dates, 1, 4)))
train$Year<- as.matrix(as.numeric(substring(train$Dates, 1, 4)))

#Make Month
test$Month<- as.matrix(as.numeric(substring(test$Dates, 6, 7)))
train$Month<- as.matrix(as.numeric(substring(train$Dates, 6, 7)))

#Make Day
test$Day<- as.matrix(as.numeric(substring(test$Dates, 9, 10)))
train$Day<- as.matrix(as.numeric(substring(train$Dates, 9, 10)))




#### Ok, let's try to engineer some geo-spatial and temporal features ####
# After this we will put it in the cloud to train on all of the available data instead of the sample

# K means 30
# This is how we group crimes on a map.
# It may be more convenient to use reporting areas, but often those bisect a cluster
clust <- test %>% select(X, Y)
clust2 <- train %>% select(X, Y)

clust <- rbind(clust, clust2)
rm(clust2)

k <- kmeans(clust, 30)

# Add cluster variable back to the data frame with the last n clusters
# I went with 30 because I suspect that will be enough for prediction
test$Cluster <- as.factor(k$cluster[1:884262]) 
train$Cluster <- as.factor(k$cluster[884263:1762311])

rm(k, clust)




#### Model 28.0 ####
# Try to add the address features back in 
# And add a couple new features, like is_dup

# The code below is for making a steet scale variable
# This essentially measures how often the stret shows up in the data
# we will see if this is predictive
test_to_combine <- test %>% 
  select(-Id)

train_to_combine <- train %>% 
  select(-Descript, -Resolution, - Category)

test_and_train <- rbind(test_to_combine, train_to_combine)

rm(test_to_combine, train_to_combine)

test_and_train <- test_and_train %>% 
  mutate(test_and_train_ID = seq(1:nrow(test_and_train)))

test_and_train$street_1 <- str_split_fixed(as.character(test_and_train$Address), " of ", 2)[,2]
test_and_train$street_2 <- str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,2]
test_and_train$street_3 <- ifelse(test_and_train$street_1 == "", 
                                  str_split_fixed(as.character(test_and_train$Address), " / ", 2)[,1],
                                  "")

street_1 <- test_and_train %>% 
  group_by(street_1) %>% 
  summarise(n=n())%>% 
  rename(street = street_1)

street_2 <- test_and_train %>% 
  group_by(street_2) %>% 
  summarise(n=n()) %>% 
  rename(street = street_2)

street_3 <- test_and_train %>% 
  group_by(street_3) %>% 
  summarise(n=n())%>% 
  rename(street = street_3)

street_final <- rbind(street_1, street_2, street_3) %>% 
  group_by(street) %>% 
  summarise(n = sum(n)) %>% 
  filter(street != "")

test_and_train <- merge(test_and_train, street_final, by.x = "street_1", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_2", by.y = "street", all.x = T)
test_and_train <- merge(test_and_train, street_final, by.x = "street_3", by.y = "street", all.x = T)

rm(street_1, street_2, street_3, street_final)

test_and_train <- test_and_train %>% 
  mutate(street_scale = pmax(n, n.y, n.x, na.rm = T)) %>% 
  select(-n, -n.x, -n.y, -street_1, -street_2, -street_3) %>% 
  arrange(test_and_train_ID)


## Make an address_scale as above with street
by_address <- test_and_train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address")
rm(by_address)

# And one for test
by_address <- test %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address", all.x = TRUE)
rm(by_address)

# And one for train
by_address <- train %>% group_by(Address) %>% summarise(address_scale=n())
test_and_train <- merge(test_and_train, by_address, by = "Address", all.x = TRUE)
rm(by_address)

# Get rid of NAs
test_and_train <- test_and_train %>% 
  mutate(address_scale = ifelse(is.na(address_scale), 0, address_scale),
         address_scale.x = ifelse(is.na(address_scale.x), 0, address_scale.x),
         address_scale.y = ifelse(is.na(address_scale.y), 0, address_scale.y))


# Find duplicates
test_and_train_dup <- test_and_train %>% 
  arrange(test_and_train_ID) %>% 
  select(-Dates, -test_and_train_ID) %>% 
  duplicated(fromLast = TRUE)

test_and_train <- test_and_train %>% 
  arrange(test_and_train_ID) %>%
  mutate(is_dup = as.numeric(test_and_train_dup))

rm(test_and_train_dup)



## More data engineering 
test_and_train <- test_and_train %>% 
  arrange(test_and_train_ID) %>% 
  
  # binarize the variable according to the location of the crime (in the street/intersection)
  mutate(AddOf = sapply(Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]}),
         street_or_intersection = ifelse(is.na(AddOf), 0, 1)) %>% 
  select(-AddOf) %>%
  
  # Creating a new variable Period_day which includes 3 categories: morning (5h-14h), 
  # afternoon (14h-20th)and night(20h-5h) 
  mutate(Period_day = ifelse((Hour >= 5) & (Hour < 14), 0, 
                             ifelse((Hour >=14) & (Hour <20), 1, 2))) %>% 
  
  # Rotate X and Ys
  # https://www.kaggle.com/c/sf-crime/forums/t/18853/feature-engineering-of-lat-long-x-y-helps
  mutate(rot45_X = .707 * Y + .707 * X,
         rot45_Y = .707 * Y - .707 * X,
         rot30_X = (1.732/2) * X + (1/2) * Y,
         rot30_Y = (1.732/2) * Y + (1/2) * X,
         rot60_X = (1/2) * X + (1.732/2) * Y,
         rot60_Y = (1/2) * Y - (1.732/2) * X,
         radial_r = sqrt(`^`(Y,2) + `^`(X, 2)))


# one more for |minutes|
date_and_time <- strptime(test_and_train$Dates, '%Y-%m-%d %H:%M:%S')
test_and_train$MinuteAbs30 <- abs(as.numeric(format(date_and_time, '%M')) - 30)

rm(date_and_time)



## Add in the logodds of addresses 
# We will use this table as a feature in the model
C_counts = train %>% group_by(Category) %>% summarise(n=n())
A_C_counts = train %>% group_by(Address, Category) %>% summarise(n=n())
A_counts = train %>% group_by(Address) %>% summarise(n=n())
default_logodds = log(C_counts$n / nrow(train)) - log(1 - (C_counts$n / nrow(train)))

table <- spread(A_C_counts, Category, n)
table[is.na(table)] <- 0
prob_m <- apply(table[2:40], 1, function(x) x / sum(x))
prob_table <- (data.frame(t(prob_m)))

log_m <- apply(prob_table, 1, function(x) log(x))
log_m[is.infinite(log_m)] <- 0
log_table <- (data.frame(t(log_m)))


# These are ones where there was only one categroy at that address
# Or where there were none and so the odds are 0
# We replace both kinds with the default logodds
for (r in 1:nrow(log_table)) {
  for (c in 1:ncol(log_table)) {
    log_table[r,c] <- ifelse(log_table[r,c] == 0 | log_table[r,c] == 1, default_logodds[c], log_table[r,c])
  }
  
}


log_table$Address <- A_counts$Address

rm(A_counts, A_C_counts, C_counts, log_m, prob_m, prob_table, table)


# Put the log odds into the df
# This first line puts them all in, but there are about 3k that don't have log odds 
# Because they are in test but not train
test_and_train <- merge(test_and_train, log_table, by = "Address", all.x = T)

# Create a df of default logodds to merge with a subset of test_and_train that is missing 
default_logodds_df <- data.frame(t(default_logodds))
colnames(default_logodds_df) <- colnames(test_and_train)[28:66]
default_logodds_df$var_to_combine <- 4321

# These are the problem ones with addresses that are in test but not train
need_replacement <- test_and_train %>% 
  filter(is.na(ASSAULT)) %>% 
  select(-ARSON : - WEAPON.LAWS) %>% 
  mutate(var_to_combine = rep(4321))

# Add in the defaults
need_replacement <- merge(need_replacement, default_logodds_df, by = "var_to_combine") %>% 
  select(-var_to_combine)

# Now take out the problem ones
test_and_train <- test_and_train %>% filter(!is.na(ASSAULT))

# And add them back in with the defaults
test_and_train <- rbind(test_and_train, need_replacement) %>% 
  arrange(test_and_train_ID)

# Save for use later
log_table_address_frequencies <- test_and_train %>% 
  select(test_and_train_ID, ARSON : WEAPON.LAWS)

write.csv(log_table_address_frequencies, "./log_table_address_frequencies.csv")


rm(need_replacement, log_table, default_logodds_df, default_logodds, log_table_address_frequencies)



## Add back in the categories
test_and_train <- arrange(test_and_train, test_and_train_ID)

test_and_train$Category <- NA
test_and_train$Category[884263 : nrow(test_and_train)] <- as.character(train$Category)


#### Save the data ####
write.csv(test_and_train, "./test_and_train.csv", row.names = FALSE)
