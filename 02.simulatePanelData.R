## This is a helper file -- You don't need to run it, it is called from other subsequent files
library(data.table)
library(ggplot2)
library(cowplot)

dt <- fread("Data/pre_simulated_data.csv")


# Standardize income and age -- and then calculate realized ad load
dt[, income_scaled := (income - mean(income)) / sd(income)]
dt[, age_scaled := (age - mean(age)) / sd(age)]
dt[, hours_scaled := (pre_all_hours - mean(pre_all_hours)) / sd(pre_all_hours)]
dt[, skips_scaled := (pre_skipped_tracks - mean(pre_skipped_tracks)) / sd(pre_skipped_tracks)]
dt[, thumbs_up_scaled := (pre_thumbs_up - mean(pre_thumbs_up)) / sd(pre_thumbs_up)]
dt[, thumbs_scaled := (pre_thumbs - mean(pre_thumbs)) / sd(pre_thumbs)]
dt[, station_scaled := (pre_station_changed - mean(pre_station_changed)) / sd(pre_station_changed)]

theta_income <- 0.1
theta_age <- 0.15

# Note that the lift on ad load is a function of income and age -- because advertisers could have preferences for certain target audiences
dt[, realizedAdload := avg_adLoad_tg * pmax(1 + rnorm(.N, income_scaled * theta_income + age_scaled * theta_age, 0.1), 0)]



# This function determines the rate at which treatment grows over time
treat_time <- function(x, conv_period = 25) {
  alpha <- -log(0.1) / conv_period
  return(1 - exp(-alpha * x))
}

sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

standardize_truncate <- function(x) {
  x <- (x - mean(x)) / sd(x)
  x[x > 1] <- 1
  x[x < -1] <- -1
  return(x)
}

simulate_data <- function(dt, time_period) {
  # Define the coefficients for the simulation
  base_sub <- -2.5
  base_active <- 1
  sub_conv_period <- 15
  activity_conv_period <- 70
  theta_sub_age <- 0.1
  theta_sub_income <- 0.15
  theta_sub_thumps <- 0.1
  theta_sub_stations <- 0.05
  theta_activity_hours <- -0.1
  theta_activity_skips <- 0.2
  theta_activity_stations <- -0.1
  theta_activity_thumbs <- -0.2
  theta_hours_hours <- -0.1
  theta_hours_age <- 0.1
  theta_hours_skips <- 0.05
  theta_hours_stations <- -0.05
  alpha_sub <- 0.1
  beta_sub <- 0.1
  alpha_activity <- -0.05
  beta_activity <- 0.01
  alpha_hours <- -0.01
  beta_hours <- 0.005
  
  if(time_period <= 0){
    alpha_sub <- 0
    beta_sub <- 0
    alpha_activity <- 0
    beta_activity <- 0
    alpha_hours <- 0
    beta_hours <- 0
  }


  dt_sim <- copy(dt)
  dt_sim[, time_period := time_period]
  
  sub_hte <- theta_sub_age * dt_sim[, age_scaled] +
             theta_sub_income * dt_sim[, income_scaled] +
             theta_sub_thumps * dt_sim[, thumbs_up_scaled] +
             theta_sub_stations * dt_sim[, station_scaled]
  
  hours_hte <- theta_hours_age * dt_sim[, age_scaled] +
               theta_hours_skips * dt_sim[, skips_scaled] +
               theta_hours_stations * dt_sim[, station_scaled]
  
  activity_hte <- theta_activity_hours * dt_sim[, hours_scaled] +
                  theta_activity_skips * dt_sim[, skips_scaled] +
                  theta_activity_stations * dt_sim[, station_scaled] +
                  theta_activity_thumbs * dt_sim[, thumbs_up_scaled]
  
  sub_hte <- standardize_truncate(sub_hte)
  hours_hte <- standardize_truncate(hours_hte)
  activity_hte <- standardize_truncate(activity_hte)
  
  dt_sim[, sub := rbinom(nrow(dt_sim), 1, sigmoid(base_sub +
                                                  treat_time(time_period, sub_conv_period) *
                                                  dt_sim[, realizedAdload] *
                                                  (alpha_sub + beta_sub * sub_hte)))]
  
  dt_sim[, activity := rbinom(nrow(dt_sim), 1, sigmoid(base_active +
                                                       treat_time(time_period, activity_conv_period) *
                                                       dt_sim[, realizedAdload] *
                                                       (alpha_activity + beta_activity * activity_hte)))]
  
  dt_sim[, hours := rlnorm(nrow(dt_sim), log(dt_sim[, pre_all_hours] + 1) +
                                         treat_time(time_period, activity_conv_period) *
                                         dt_sim[, realizedAdload] *
                                         (alpha_hours + beta_hours * hours_hte), 0.01)]
  
  dt_sim[, all_hours := hours *activity ]
  dt_sim[, ad_supported_hours := all_hours * activity * (1-sub)]
  dt_sim[, num_ads := round(ad_supported_hours*realizedAdload)]
  if(time_period < 0){
    dt_sim[, num_ads := round(ad_supported_hours*mean(avg_adLoad_tg[tg == "control"]) * pmax(1 + rnorm(.N, income_scaled * theta_income + age_scaled * theta_age, 0.1), 0))]
  }
  
  return(dt_sim)
}