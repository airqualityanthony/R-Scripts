## AQEval Change point detection
library(AQEval)
library(openair)
library(ggplot2)
library(dplyr)

AQEvalMultipleSite <- function(site, years){
  data <- importAURN(site = site, year = years)
  
  data$iso <- AQEval::isolateContribution(data = data, pollutant = "no2")
  
  
  data_daily <- timeAverage(data,avg.time = "day")
  
  bps <- data.frame(lower="blank",bpt="blank",upper="blank")
  bps <- rbind(bps,findBreakPoints(data_daily, pollutant = "iso"))
  bps$site <- site
  
  quant <- quantBreakPoints(data_daily,"iso",breaks = bps)
  
  output <- list(bps=bps,plot = quant$plot, report = quant$report)
  return(output)
}

meta_data <- importMeta(all = TRUE) %>% 
  filter(variable == "NO2", end_date == "ongoing",start_date <= "2018-01-01", site_type == "Urban Traffic")

aqeval_output <- list()


for (i in meta_data$code){
  aqeval_output[[length(aqeval_output)+1]] <- AQEvalMultipleSite(i,years = 2019:2020)
}

## return all model stats
aqeval_stats <- data.frame()

for (i in seq(1,length(aqeval_output),1)){
  aqeval_stats <- rbind(aqeval_stats,as.data.frame(aqeval_output[[i]][1]))
}

saveRDS(aqeval_stats, "aqeval_bps.rds")
saveRDS(aqeval_output, "aqeval_output.rds")