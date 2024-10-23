#%%
library(data.table)
library(ggplot2)
library(lfe)
#%%
# Set the random seed for reproducibility
set.seed(0)

# Define the number of listeners
num_listeners <- 200000

# Generate random listener IDs
listener_ids <- 1:(num_listeners)

# Generate random ages between 13 and 95
ages <- sample(13:95, num_listeners, replace = TRUE)

# Generate lognormal income values
mean <- log(73000)
std <- 0.3 # Adjust the standard deviation as needed
income <- rlnorm(num_listeners, mean, std)

# Generate gender
gender <- sample(c('m', 'f'), num_listeners, replace = TRUE, prob = c(0.45, 0.55))

# Generate random pre-listening hours between 0 and 40
pre_all_hours <- sample(0:999, num_listeners, replace = TRUE)

# skipped track
pre_skipped_tracks <- sample(0:99, num_listeners, replace = TRUE)

# station changed
pre_station_changed <- sample(0:99, num_listeners, replace = TRUE)

# thumbs up
pre_thumbs_up <- sample(0:99, num_listeners, replace = TRUE)

# thumbs down
pre_thumbs_down <- sample(0:19, num_listeners, replace = TRUE)

# thumps
pre_thumbs <- pre_thumbs_up + pre_thumbs_down

# ad supported ours
# Generate random numbers between 0 and 1 with an average of 0.95
random_numbers <- pmin(rnorm(num_listeners, mean = 0.95, sd = 0.05), rep(1, num_listeners))
pre_ad_supported_hours <- random_numbers * pre_all_hours

# Generate random treatment groups (control or one of the other 6 arms)
treatment_groups <- sample(c('control', '3x1', '4x2', '4x3', '5x3', '6x2', '6x3'), num_listeners, replace = TRUE)

# Create the data table
dt <- data.table(
  listener_id = listener_ids,
  tg = treatment_groups,
  pre_all_hours = pre_all_hours,
  pre_ad_supported_hours = pre_ad_supported_hours,
  pre_thumbs = pre_thumbs,
  pre_thumbs_up = pre_thumbs_up,
  pre_skipped_tracks = pre_skipped_tracks,
  pre_station_changed = pre_station_changed,
  age = ages,
  gender = gender,
  income = income
)



# Create a list of tuples with the average realized ad load for each treatment group
df_adload <- data.table(tg = c('3x1', '4x2', '4x3', '5x3', '6x2', '6x3', 'control'),
                   avg_adLoad_tg = c(2.947, 4.659, 5.541, 6.123, 5.602, 6.665, 4.208))

# Merge the average realized ad load with the main data table
dt <- merge(dt, df_adload, by = 'tg', all.x = TRUE)

#%%
# Write the data table to a CSV file
fwrite(dt, "./bigdata/replicationPackage/Data_generate/pre_simulated_data.csv")
#%%
dim(dt) # 200000 12
