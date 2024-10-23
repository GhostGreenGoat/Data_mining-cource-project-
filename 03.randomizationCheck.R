# This script performs a randomization check on pre-simulated data.
# It calculates estimates and standard errors for various variables and generates a LaTeX table.

# Load required libraries
#%%
library(knitr)
library(kableExtra)
library(data.table)

# Read the pre-simulated data from a CSV file
dt <- fread("./bigdata/replicationPackage/Data_generate/pre_simulated_data.csv")
#%%
control_dt <- dt[tg == "control"]
#%%

# Calculate relative values for each variable compared to the control group
dt[, pre_ad_supported_hours := 100*pre_ad_supported_hours/mean(pre_ad_supported_hours[tg == "control"])]
dt[, pre_all_hours := 100*pre_all_hours/mean(pre_all_hours[tg == "control"])]
dt[, pre_thumbs := 100*pre_thumbs/mean(pre_thumbs[tg == "control"])]
dt[, pre_thumbs_up := 100*pre_thumbs_up/mean(pre_thumbs_up[tg == "control"])]
dt[, pre_skipped_tracks := 100*pre_skipped_tracks/mean(pre_skipped_tracks[tg == "control"])]
dt[, pre_station_changed := 100*pre_station_changed/mean(pre_station_changed[tg == "control"])]
dt[, gender := gender == "m"]
#%%
# Define the columns to be used for estimation and standard error calculation
cols <- colnames(dt)[(3:11)]

# Function to calculate standard error
sderr <- function(x) {
    round(1.96*sd(x)/sqrt(length(x)), 3)
}

# Function to round mean values
roundMean <- function(x) {
    round(mean(x), 3)
}

# Calculate mean estimates for each variable by treatment group
dt_est <- dt[, lapply(.SD, roundMean), by = tg, .SDcols = cols]
dt_est$estimate <- "estimate"

# Calculate standard errors for each variable by treatment group
dt_sde <- dt[, lapply(.SD, sderr), by = tg, .SDcols = cols]
dt_sde$estimate <- "sderr"

# Combine the estimates and standard errors into a single table
tab <- rbind(dt_est, dt_sde)
#%%
# Reshape the table for better presentation
tab <- melt(tab, id.vars = c("tg","estimate"))
tab[, value := as.character(value)]
tab[estimate =="sderr", value := paste0("(",value,")")]
tab <- dcast(tab, formula = variable + estimate ~ tg)
tab[estimate =="sderr", variable := ""]
#%%
# Generate a LaTeX table using kable and kableExtra packages
kable(tab, format = "latex", booktabs = TRUE, caption = "Randomization Check") %>%
    kable_styling(latex_options = c("striped", "hover"), full_width = FALSE) %>%
    save_kable(file = "./bigdata/replicationPackage/Data_generate/randomization_check.tex")
