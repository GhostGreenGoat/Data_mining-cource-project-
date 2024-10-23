#%%
library(data.table)
library(ggplot2)
library(lfe)



dat <- fread("Data_generate/ML_test_data_with_sub_propensity.csv")

dat[, rel_sub_prop_6x3 := (sub_prop_6x3-sub_prop_control)/mean(sub_prop_control)]
dat[, rel_sub_prop_5x3 := (sub_prop_5x3-sub_prop_control)/mean(sub_prop_control)]
dat[, rel_sub_prop_4x3 := (sub_prop_4x3-sub_prop_control)/mean(sub_prop_control)]
dat[, rel_sub_prop_6x2 := (sub_prop_6x2-sub_prop_control)/mean(sub_prop_control)]
dat[, rel_sub_prop_4x2 := (sub_prop_4x2-sub_prop_control)/mean(sub_prop_control)]
dat[, rel_sub_prop_3x1 := (sub_prop_3x1-sub_prop_control)/mean(sub_prop_control)]

#%%
## melt it and only select columns that have rel_ in the name 
treat_hist <- dat[, c("listener_id", "rel_sub_prop_6x3", "rel_sub_prop_5x3", "rel_sub_prop_4x3", "rel_sub_prop_6x2", "rel_sub_prop_4x2", "rel_sub_prop_3x1")]
treat_hist <- melt(treat_hist, id.vars = "listener_id")
treat_hist[, median_value := median(value), by = variable]
## remove rel_sub_prop_ from the name and order it so that 3x1 is first then 4x2, 4x3 and so forth 
treat_hist[, variable := factor(gsub("rel_sub_prop_", "", variable), levels = c("3x1", "4x2", "4x3", "5x3", "6x2", "6x3"))]
## Plot the distribution of the relative subscription propensity for each treatment group (in each facet)
ggplot(treat_hist, aes(x = value, fill = value > 0)) +
  geom_histogram(bins = 50, color = "blue", fill = "skyblue", aes(y = ..density..)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_vline(data = treat_hist, aes(xintercept = median_value), color = "black", linetype = "dashed") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "",
       x = "Change in subscription propensity (normalized by average in control)",
       y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Data_generate/subscription_propensity_lift_prediction_hist.png", width = 10, height = 6, dpi = 300)
ggsave("Data_generate/subscription_propensity_lift_prediction_hist.eps", width = 10, height = 6, dpi = 300)

#%%
dat[, lift := sub_prop_6x3 - sub_prop_control]
## put lift into five quintiles 
dat[, lift_quintile := as.numeric(cut(lift, quantile(lift, probs = seq(0, 1, 0.2), na.rm = TRUE)), include.lowest = TRUE)]


## now measure the realized lift of moving someone from control to 6x3 across quintiles
dat <- dat[tg == "control" | tg == "6x3"]   
dat[, treatment := ifelse(tg == "control", 0, 1)]
dat[, sub := sub/mean(sub[treatment == 0])]
reg <- summary(felm(data = dat, sub ~ treatment:as.factor(lift_quintile)))

lift_dat <- data.table(
  lift_quintile = 1:5,
  sub = reg$coefficients[2:6,1],
  se = reg$coefficients[2:6,2]
)

ggplot(lift_dat, aes(x = lift_quintile, y = sub)) +
  geom_point() +
  geom_errorbar(aes(ymin = sub - 1.96*se, ymax = sub + 1.96*se), width = 0.2) +
  labs(title = "",
       x = "Predicted treatment effect (quintile)",
       y = "Lift in subscription propensity (relative to control)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Data_generate/subscription_lift.png", width = 6, height = 4, dpi = 300)
ggsave("Data_generate/subscription_lift.eps", width = 6, height = 4)

#%%
## Now let's do the same for the number of ads (lift)


dat <- fread("Data_generate/ML_test_data_with_num_ads_prediction.csv")

## adding the histogram for the lift in number of ads 
dat[, rel_num_ads_6x3 := (num_ads_pred_6x3-num_ads_pred_control)/mean(num_ads_pred_control)]
dat[, rel_num_ads_5x3 := (num_ads_pred_5x3-num_ads_pred_control)/mean(num_ads_pred_control)]
dat[, rel_num_ads_4x3 := (num_ads_pred_4x3-num_ads_pred_control)/mean(num_ads_pred_control)]
dat[, rel_num_ads_6x2 := (num_ads_pred_6x2-num_ads_pred_control)/mean(num_ads_pred_control)]
dat[, rel_num_ads_4x2 := (num_ads_pred_4x2-num_ads_pred_control)/mean(num_ads_pred_control)]
dat[, rel_num_ads_3x1 := (num_ads_pred_3x1-num_ads_pred_control)/mean(num_ads_pred_control)]

treat_hist <- dat[, c("listener_id", "rel_num_ads_6x3", "rel_num_ads_5x3", "rel_num_ads_4x3", "rel_num_ads_6x2", "rel_num_ads_4x2", "rel_num_ads_3x1")]
treat_hist <- melt(treat_hist, id.vars = "listener_id")
treat_hist[, median_value := median(value), by = variable]
treat_hist[, variable := factor(gsub("rel_num_ads_", "", variable), levels = c("3x1", "4x2", "4x3", "5x3", "6x2", "6x3"))]
ggplot(treat_hist, aes(x = value, fill = value > 0)) +
    geom_histogram(bins = 50, color = "blue", fill = "skyblue", aes(y = ..density..)) +
    geom_vline(xintercept = 0, color = "red") +
    geom_vline(data = treat_hist, aes(xintercept = median_value), color = "black", linetype = "dashed") +
    facet_wrap(~variable, scales = "free") +
    labs(title = "",
             x = "Change in number of ads delivered (normalized by average in control)",
             y = "Density") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(-2, 2)
ggsave("Data_generate/num_ads_lift_prediction_hist.png", width = 10, height = 6, dpi = 300)
ggsave("Data_generate/num_ads_lift_prediction_hist.eps", width = 10, height = 6, dpi = 300)

#%%
dat[, lift := num_ads_pred_6x3 - num_ads_pred_control]
dat[, lift_quintile := as.numeric(cut(lift, quantile(lift, probs = seq(0, 1, 0.2), na.rm = TRUE)), include.lowest = TRUE)]
dat <- dat[tg == "control" | tg == "6x3"]   
dat[, treatment := ifelse(tg == "control", 0, 1)]
dat[, num_ads := num_ads/mean(num_ads[treatment == 0])]
reg <- summary(felm(data = dat, num_ads ~ treatment:as.factor(lift_quintile)))

lift_dat <- data.table(
  lift_quintile = 1:5,
  sub = reg$coefficients[2:6,1],
  se = reg$coefficients[2:6,2]
)

ggplot(lift_dat, aes(x = lift_quintile, y = sub)) +
  geom_point() +
  geom_errorbar(aes(ymin = sub - 1.96*se, ymax = sub + 1.96*se), width = 0.2) +
  labs(title = "",
       x = "Predicted treatment effect (quintile)",
       y = "Lift in number of ads (relative to control)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Data_generate/num_ads_lift.png", width = 6, height = 4, dpi = 300)
ggsave("Data_generate/num_ads_lift.eps", width = 6, height = 4)