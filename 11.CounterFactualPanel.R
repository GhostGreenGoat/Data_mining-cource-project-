#%%
library(data.table)
library(ggplot2)
source("02.simulatePanelData.R")

time_periods <- -25:100

assigned_cond <- fread("Data_generate/personalized_control_counterpart_assigned_condition.csv")
#%%
agg_plots <- data.table()

for (time_period in time_periods) {
  dt_sim <- simulate_data(dt, time_period)
  agg_plots_temp_personalized <- merge(dt_sim, assigned_cond, by = "listener_id")
  agg_plots_temp_personalized <- agg_plots_temp_personalized[tg == assigned_condition, .(sub = weighted.mean(sub, ipw), activity = weighted.mean(activity, ipw), adload = weighted.mean(num_ads, ipw)/weighted.mean(ad_supported_hours, ipw),
                       all_hours = weighted.mean(all_hours, ipw), ad_supported_hours = weighted.mean(ad_supported_hours, ipw), num_ads = weighted.mean(num_ads, ipw))]
  agg_plots_temp_personalized$tg <- "Personalized"  
  agg_plots_temp <- dt_sim[, .(sub = mean(sub), activity = mean(activity), adload = sum(num_ads)/sum(ad_supported_hours),
                       all_hours = mean(all_hours), ad_supported_hours = mean(ad_supported_hours), num_ads = mean(num_ads)), by = tg]
  agg_plots_temp <- rbind(agg_plots_temp, agg_plots_temp_personalized)
  agg_plots_temp[, time_period := time_period] 
  agg_plots <- rbind(agg_plots, agg_plots_temp)
}


agg_plots[, ":="(sub_norm = sub/mean(sub[tg == "control"]),
                activity_norm = activity/mean(activity[tg == "control"]),
                adload_norm = adload/mean(adload[tg == "control"]),
                all_hours_norm = all_hours/mean(all_hours[tg == "control"]),
                ad_supported_hours_norm = ad_supported_hours/mean(ad_supported_hours[tg == "control"]),
                num_ads = num_ads/mean(num_ads[tg == "control"]) ), by = "time_period"]

#%%
plot_adLoad <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = adload_norm, color = tg)) +
    geom_line() +
    labs(title = "Audio Ad Load",
            x = "Time Period",
            y = "Audio ad load relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")

plot_subs <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = sub_norm, color = tg)) +
    geom_line() +
    labs(title = "Plus users",
            x = "Time Period",
            y = "Plus users relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")

plot_numAds <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = num_ads, color = tg)) +
    geom_line() +
    labs(title = "Audio ad revenue",
            x = "Time Period",
            y = "Ad revenue relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")

plot_allHours <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = all_hours_norm, color = tg)) +
    geom_line() +
    labs(title = "All hours",
            x = "Time Period",
            y = "All hours relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")

plot_activity <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = activity_norm, color = tg)) +
    geom_line() +
    labs(title = "Active users",
            x = "Time Period",
            y = "Active users relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")

plot_adSuppHours <- ggplot(data = agg_plots[tg %in% c("3x1","6x3","Personalized")], aes(x = time_period, y = ad_supported_hours_norm, color = tg)) +
    geom_line() +
    labs(title = "Ad supported hours",
            x = "Time Period",
            y = "Ad supported hours relative to control") + labs(color = "Condition") + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed")


legend_p <- get_legend(plot_activity)


plot_adLoad <- plot_adLoad + theme(legend.position = "none")
plot_subs <- plot_subs + theme(legend.position = "none")
plot_numAds <- plot_numAds + theme(legend.position = "none")
plot_allHours <- plot_allHours + theme(legend.position = "none")
plot_activity <- plot_activity + theme(legend.position = "none")
plot_adSuppHours <- plot_adSuppHours + theme(legend.position = "none")


plot_grid(plot_grid(plot_adLoad, plot_numAds, plot_activity, plot_subs, plot_allHours, plot_adSuppHours, ncol = 2), plot_grid(legend_p, ncol = 1), rel_widths = c(0.85, 0.15))
ggsave("Data_generate/agg_plots_personalized.png", height = 11.5, width = 11)
ggsave("Data_generate/agg_plots_personalized.eps",height =  11.5, width = 11)
