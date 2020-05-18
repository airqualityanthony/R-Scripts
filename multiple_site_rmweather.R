library(dplyr)
library(rmweather)
library(ranger)
library(lubridate)
library(tidyverse)
# require(devtools)
# install_github('davidcarslaw/openair')
library(openair)
library(gridExtra)
library(caret)

rmweather_output <- function(site,years){
  aq_data <- importAURN(site=site, year = years, pollutant = c("no2","wd","ws","air_temp"), verbose = TRUE) 
  
  data_prepared <- aq_data %>% 
    filter(!is.na(ws)) %>% 
    rename(value = no2) %>% 
    rmw_prepare_data(na.rm = TRUE)
  
  data_prepared_train <- filter(data_prepared, date < as.POSIXct('2020-03-01', format='%Y-%m-%d'))
  data_prepared_predict <- filter(data_prepared, date >= as.POSIXct('2020-03-01', format='%Y-%m-%d'))
  
  data_prepared_train$weekday <- as.numeric(as.character(data_prepared_train$weekday))
  data_prepared_predict$weekday <- as.numeric(as.character(data_prepared_predict$weekday))
  
  train_model <- rmw_train_model(
    data_prepared_train,
    variables = c(
      "date_unix", "day_julian", "weekday","hour", "wd", "ws","air_temp"),
    n_trees = 300,
    keep_inbag = T,
    #se = T,
    verbose = T)
  
  predict_RF <- rmw_predict(train_model, 
                            df = 
                              dplyr::select(data_prepared_predict, date_unix, day_julian, weekday, hour, wd, ws, air_temp), 
                            se = F, 
                            #n_cores = NULL, 
                            verbose = T)
  
  predictions <- cbind(dplyr::select(data_prepared_predict, date, date_unix, day_julian, weekday, hour, wd, ws, air_temp), predict_RF)
  predictions$type <- "prediction"
  colnames(predictions)[9] <- "no2"
  
  original <- dplyr::select(data_prepared_predict,date, date_unix, day_julian, weekday, hour, wd, ws, air_temp, value)
  original$type <- "measurement"
  colnames(original)[9] <- "no2"
  
  train_data <- dplyr::select(data_prepared_train,date, date_unix, day_julian, weekday, hour, wd, ws, air_temp, value)
  train_data$type <- "measurement"
  colnames(train_data)[9] <- "no2"
  
  final_ouput <- rbind(original,predictions)
  
  gg <- ggplot(final_ouput, aes(date, no2, colour = type)) + geom_point() + stat_smooth()
  
  data_prepared_pre_lockdown <- filter(data_prepared, date < as.POSIXct('2020-03-01', format='%Y-%m-%d'))

  train <- data_prepared_pre_lockdown[ which(data_prepared_pre_lockdown$set=='training'),]
  test <- data_prepared_pre_lockdown[ which(data_prepared_pre_lockdown$set=='testing'),]
  
  ## 'rmweather' - Random Forest Model.
  
  model_validation <- rmw_train_model(
    train,
    variables = c(
      "date_unix", "day_julian", "weekday","hour", "wd", "ws","air_temp"),
    n_trees = 300,
    keep_inbag = T,
    #se = T,
    verbose = T)
  
  
  ## using model output to predict for specified period:
  
  
  predict_validation <- rmw_predict(model_validation, 
                                    df = 
                                      dplyr::select(test, date_unix, day_julian, weekday, hour, wd, ws, air_temp), 
                                    se = F, 
                                    #n_cores = NULL, 
                                    verbose = T)
  
  test$predict <- predict_validation
  
  rmw_results <- data.frame(date=test$date,obs=test$value,pred=predict_validation)
  
  R2_rmw <- round(R2(predict_validation,test$value),3)
  RMSE_rmw <- round(RMSE(predict_validation,test$value),3)
  MAE_rmw <- round(MAE(predict_validation,test$value),3)
  
  R2_text_rmw <- as.character(as.expression(paste("R2:",R2_rmw)))
  RMSE_text_rmw <- as.character(as.expression(paste("RMSE:",RMSE_rmw)))
  MAE_text_rmw <- as.character(as.expression(paste("MAE:",MAE_rmw)))
  
  plot_text_rmw <- paste(R2_text_rmw,RMSE_text_rmw,MAE_text_rmw, sep = "\n")
  
  
  gg_validation <- ggplot(rmw_results, aes(obs,pred)) + geom_point() + geom_abline(intercept = 0) + geom_text(x = 20, y = 150, label = plot_text_rmw) + ggtitle(label = paste(site," - rmweather Validation - Train test split 80/20",as.character(years)))
  
  prediction_stats <- data.frame(R2=R2_rmw,RMSE=RMSE_rmw,MAE=MAE_rmw)
  
  results <- list(plot=gg_validation,model_stats=prediction_stats,obs_pred=rmw_results)
  
  print(results$model_stats)
  return(results)
}

meta_data <- importMeta(all = TRUE) %>% 
  filter(variable == "NO2", end_date == "ongoing",start_date <= "2018-01-01", site_type == "Urban Traffic")



results_list <- list()

for (i in meta_data$code){
  results_list[[length(results_list)+1]] <- rmweather_output(site = i,2018:2020)
}

## return all model stats
stats_df <- data.frame()

for (i in seq(1,length(results_list),1)){
stats_df <- rbind(stats_df,as.data.frame(results_list[[i]][2]))
}

densityplot(stats_df$model_stats.RMSE)
densityplot(stats_df$model_stats.R2)


ggplot(stats_df) + geom_density(aes(model_stats.RMSE)) + geom_rug(aes(model_stats.RMSE))
