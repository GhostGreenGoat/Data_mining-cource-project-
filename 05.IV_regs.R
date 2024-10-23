#%%
source("02.simulatePanelData.R")
library(lfe)
library(ggplot2)
#%%
time_periods <- -25:100
agg_plots <- data.table()

for (time_period in time_periods) {
    agg_plots_temp <- data.table()
    dt_sim <- simulate_data(dt, time_period)
  
    dt_sim[, ":="(sub_norm = 100*sub/mean(sub[tg == "control"]),
                  activity_norm = 100*activity/mean(activity[tg == "control"]),
                  adload_norm = 100*sum(num_ads)/sum(ad_supported_hours),
                  all_hours_norm = 100*all_hours/mean(all_hours[tg == "control"]),
                  ad_supported_hours_norm = 100*ad_supported_hours/mean(ad_supported_hours[tg == "control"]),
                  num_ads = 100*num_ads/mean(num_ads[tg == "control"]) ), by = "time_period"]

    mod_sub <- summary(felm(sub_norm ~ 1 | 0 | (realizedAdload ~ tg), dt_sim))
    mod_activity <- summary(felm(activity_norm ~ 1 | 0 | (realizedAdload ~ tg), dt_sim))
    mod_all_hours <- summary(felm(all_hours_norm ~ 1 | 0 | (realizedAdload ~ tg), dt_sim))
    mod_ad_supp_hours <- summary(felm(ad_supported_hours_norm ~ 1 | 0 | (realizedAdload ~ tg), dt_sim))


    agg_plots_temp <- data.table(
        sub = coef(mod_sub)[2,1], sub_sde = coef(mod_sub)[2,2],
        activity = coef(mod_activity)[2,1], activity_sde = coef(mod_activity)[2,2],
        all_hours = coef(mod_all_hours)[2,1], all_hours_sde = coef(mod_all_hours)[2,2],
        ad_supp_hours = coef(mod_ad_supp_hours)[2,1], ad_supp_hours_sde = coef(mod_ad_supp_hours)[2,2]
    )
    agg_plots_temp[, time_period := time_period] 
    agg_plots <- rbind(agg_plots, agg_plots_temp)
}

ggplot(agg_plots, aes(x = time_period)) +
  geom_line(aes(y = sub), color = "black") +
  geom_errorbar(aes(ymin = sub - 1.96*sub_sde, ymax = sub + 1.96*sub_sde), width = 0.2, color = "black") +
  labs(title = "",
       x = "Time period",
       y = "Percentage change") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed")
#%%
ggsave("./Data_generate/IV_regs_subs.png", width = 6, height = 4)
ggsave("./Data_generate/IV_regs_subs.eps", width = 6, height = 4)
#%%



ggplot(agg_plots, aes(x = time_period)) +
  geom_line(aes(y = activity), color = "red") +
  geom_errorbar(aes(ymin = activity - 1.96*activity_sde, ymax = activity + 1.96*activity_sde), width = 0.2, color = "red") +
  geom_line(aes(y = all_hours), color = "blue") +
  geom_errorbar(aes(ymin = all_hours - 1.96*all_hours_sde, ymax = all_hours + 1.96*all_hours_sde), width = 0.2, color = "blue") +
  geom_line(aes(y = ad_supp_hours), color = "green") +
  geom_errorbar(aes(ymin = ad_supp_hours - 1.96*ad_supp_hours_sde, ymax = ad_supp_hours + 1.96*ad_supp_hours_sde), width = 0.2, color = "green") +
  labs(title = "",
       x = "Time period",
       y = "Percentage change",
       color = "Variable") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(labels = c("Activity", "All Hours", "Ad Supported Hours"),
                     name = "Legend") +
  theme(legend.position = "bottom")
ggsave("./Data_generate/IV_regs_activity.png", width = 6, height = 4)
ggsave("./Data_generate/IV_regs_activity.eps", width = 6, height = 4)

