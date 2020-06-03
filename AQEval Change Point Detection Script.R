## AQEval Change point detection
library(AQEval)
library(openair)
library(ggplot2)
library(dplyr)
require(tidyr)
require(lubridate)

## function to pull AQ data, isolate contribution, time average for lockdown period (Jan-present), 
## then run findBreakPoints (h = 0.1), then Quant, return list with bps, plot, report
AQEvalMultipleSite <- function(site, years){
  data <- importAURN(site = site, year = years)
  
  data$iso <- isolateContribution(data = data, pollutant = "no2")
  
  
  data_daily <- data[data$date>"2019-12-31",] %>% 
    timeAverage(avg.time = "day") %>% 
    filter(!is.na(iso))
  
  bps <- data.frame(lower=NULL,bpt=NULL,upper=NULL)
  bps_temp <- findBreakPoints(data_daily,pollutant = "iso", h = 0.1)
  if (is.null(bps_temp)){
    return()
  }
  bps <- rbind(bps,findBreakPoints(data_daily, pollutant = "iso", h = 0.1))
  bps$site <- site
  
  quant <- quantBreakPoints(data_daily,"iso",breaks = bps)
  
  output <- list(bps=bps,plot = quant$plot, report = quant$report)
  return(output)
}

## pull in meta data for live Urban Traffic sites. 
meta_data <- importMeta(all = TRUE) %>% 
  filter(variable == "NO2", end_date == "ongoing",start_date <= "2018-01-01", site_type == "Urban Traffic")

## create empty list
aqeval_output <- list()

## loop AQEvalMultipleSite function across Urban Traffic Sites
for (i in meta_data$code){
  aqeval_output[[length(aqeval_output)+1]] <- AQEvalMultipleSite(i,years = 2019:2020)
}

## empty stats data.frame
aqeval_stats <- data.frame()

## return all model stats
for (i in seq(1,length(aqeval_output),1)){
  temp_df <- as.data.frame(aqeval_output[[i]][1])
  temp_df <- cbind(temp_df,as.data.frame(aqeval_output[[i]][3]))
  aqeval_stats <- rbind(aqeval_stats,temp_df)
}

saveRDS(aqeval_stats, "aqeval_bps.rds")
saveRDS(aqeval_output, "aqeval_output.rds")


## plot histogram of breakpoints over lockdown period across urban traffic sites.
ggplot(aqeval_stats, aes(x=as.Date(report.date, origin = "2019-12-31"))) + geom_histogram(binwidth = 1) + xlab("Date of breakpoint") + ggtitle("Changepoints detected in Urban Traffic AURN sites using the 'AQEval' package,",subtitle = "for the period Jan - May 2020")

## Read KR detected breakpoints file.
break.point.table.no2 <- readRDS("AQEvalCOVIDAnalysisKR/break.point.table.no2.rds")

## select only Urban Traffic sites.
KR.bps <- break.point.table.no2 %>% filter(site_type == "Urban Traffic")
## rename to AW.bps
AW.bps <- aqeval_stats

KR.bps$source <- "KR"
bps_check <- select(KR.bps,c(code,bp.date,source))

AW.bps$code <- AW.bps$bps.site
AW.bps$bp.date <- as.Date(AW.bps$bps.bpt,origin = "2019-12-31")
AW.bps$source <- "AW"
## create combined data.frame with "source" column to highlight who's run the data is from.
bps_check <- rbind(bps_check,select(AW.bps,c(code,bp.date,source)))

## plot breakpoint histogram with colour set to source of run.
ggplot(bps_check, aes(x=as.Date(bp.date, origin = "2019-12-31"), colour = source)) + geom_histogram(binwidth = 1) + xlab("Date of breakpoint") + ggtitle("Changepoints detected in Urban Traffic AURN sites using the 'AQEval' package,",subtitle = "for the period Jan - May 2020")




