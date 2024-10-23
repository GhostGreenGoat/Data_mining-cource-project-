#%%
source("02.simulatePanelData.R")
library(lfe)
library(ggplot2)
library(caret)

## export data from period 60 -- middle of the experiment -- to be used for learning heterogenous treatment effects using ML
time_period <- 60
dat <- simulate_data(dt, time_period)
dat[, gender := as.numeric(gender == "m")]
feature_cols <- colnames(dat)
# selecting input features for heterogenous treatement effects
feature_cols <- feature_cols[grepl("pre_|income_|age_|gender", feature_cols)]
# center and scale the input features
dat <- dat[, c("listener_id","tg",feature_cols,"hours","ad_supported_hours","all_hours","activity","sub","num_ads"),with = FALSE]

## split the data into train and test and stratify by treatment group and sub -- to ensure that the test set has a similar distribution of treatment groups and subscriptions 
## we will use the built in function in caret to split the data into train and test
set.seed(0)
dat[, tg_sub_index := paste0(tg,":",sub)]
train_index <- createDataPartition(dat$tg_sub_index, p = 0.5, list = FALSE)
dat$test <- 1
dat[train_index, test := 0]


fwrite(dat,"./Data_generate/ML_data.csv")

