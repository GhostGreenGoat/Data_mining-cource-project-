#%%
library(data.table)
library(ggplot2)
library(parallel)
library(doParallel)


dat_sub <- fread("Data_generate/ML_test_data_with_sub_propensity.csv")
dat_ads <- fread("Data_generate/ML_test_data_with_num_ads_prediction.csv")


dat_sub <- dat_sub[, c('listener_id', 'sub_prop_3x1','sub_prop_4x2',
                       'sub_prop_4x3','sub_prop_5x3','sub_prop_6x2',
                       'sub_prop_6x3', 'sub_prop_control')]


dat <- merge(dat_ads, dat_sub)


## shadow price range between -10 to 0 vector to loop over
shadow_price_range <- seq(-0.0003, 0, length = 500)
treatment_groups <- c('control','3x1','4x2','4x3','5x3','6x2','6x3')
## make sure tg is ordered by treatment_groups values (as a factor)
## convert tg to an integer between 1 to 7
dat[, tg_num := as.integer(factor(tg, levels = treatment_groups))]

## inverse propensity weights
dat[, ipw := nrow(dat)/.N, by = tg]
#%%

library(foreach)
library(doParallel)

# Set the number of cores to use
num_cores <- detectCores()

# Register the parallel backend
registerDoParallel(num_cores - 2 )

agg_personalized <- foreach(l = shadow_price_range, .combine = rbind) %dopar% {
    temp <- data.table()
    for(t in treatment_groups){
        dat[, paste0("value_",t) := get(paste0("num_ads_pred_",t))*l + get(paste0("sub_prop_",t))]
    }
    ## get the argmax (which condition to assign the user to) that maximizes value
    dat[, max_value := apply(dat[, c("value_control","value_3x1","value_4x2","value_4x3","value_5x3","value_6x2","value_6x3")], 1, which.max)]

    temp <- dat[max_value == tg_num, .(num_ads = weighted.mean(num_ads, ipw),
                                                                         sub = weighted.mean(sub, ipw))]
    temp[, shadow_price := l]
    return(temp)
}

#%%

# Stop the parallel backend
stopImplicitCluster()

agg_uniform <- dat[, .(num_ads = mean(num_ads),
                       sub = mean(sub)), by = "tg"]

agg_personalized[, tg := "Personalized"]
agg_plot <- rbind(agg_uniform, agg_personalized, fill = TRUE)
agg_plot[, ":="(num_ads_norm = num_ads/mean(num_ads[tg == "control"]),
              sub_norm = sub/mean(sub[tg == "control"]))]
agg_plot[tg == "control", tg := "Control"]
#%%
## find the closest personalized point to the the control condtion (num_ads_norm ~ 1)
closest_point <- agg_plot[tg == "Personalized", shadow_price[which.min(abs(num_ads_norm - 1))]]
sub_norm_counterPart <- agg_plot[shadow_price == closest_point & tg == "Personalized", sub_norm]

## plot the pareto frontier. which is to Plot the uniform and personalized ones on the same plot
## use tg as color to plot the performance of both personalized and uniform in agg_plot data.table
ggplot(data = agg_plot) + 
    geom_point(aes(x = num_ads_norm, y = sub_norm, color = tg), size = 3) + 
    geom_hline(yintercept = 1, linetype = "dotted") + 
    geom_vline(xintercept = 1, linetype = "dotted") + 
    geom_smooth(data = subset(agg_plot, tg != "Personalized"), aes(x = num_ads_norm, y = sub_norm), method = "lm", linetype = "dashed", se = FALSE) +
    geom_point(aes(x = 1, y = sub_norm_counterPart), color = "black", size = 4, shape = 11) +  # Add a black star point
    labs(title = "", x = "Average number of ads delivered (relative to control)", y = "Subscription profits (relative to control)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(color = "Condition") +
    geom_text(data = subset(agg_plot, tg != "Personalized"), aes(x = num_ads_norm, y = sub_norm, label = tg), vjust = 0, hjust = -0.2)

ggsave("Data_generate/ParetoFrontier.png", width = 8, height = 6, dpi = 300)
ggsave("Data_generate/ParetoFrontier.eps", width = 8, height = 6, dpi = 300)

#%%

## save the control counterpart and the shadow price
l = closest_point
for(t in treatment_groups){
    dat[, paste0("value_",t) := get(paste0("num_ads_pred_",t))*l + get(paste0("sub_prop_",t))]
}
## get the argmax (which condition to assign the user to) that maximizes value
dat[, max_value := apply(dat[, c("value_control","value_3x1","value_4x2","value_4x3","value_5x3","value_6x2","value_6x3")], 1, which.max)]
dat[, assigned_condition := treatment_groups[max_value]]
dat <- dat[, c('listener_id', 'assigned_condition','ipw')]
#%%
fwrite(dat, "Data_generate/personalized_control_counterpart_assigned_condition.csv")