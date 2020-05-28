devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
library(raster)
library(sf)
library(sp)
library(ncdf4)
library(ggplot2)

## Most of this distilled from following tutorial; https://notes.stefanomattia.net/2018/02/14/Plotting-Sentinel-5P-NetCDF-products-with-R-and-ggplot2/



services_avail()
## Define an AOI (either matrix, sf or sp object)
birmingham_aoi <- data.frame

set_aoi()

getSpatialData::get_aoi()


login_CopHub(username = "") # Input username. Asks you for password. Sign up here; https://scihub.copernicus.eu/dhus/#/self-registration
set_archive("~/R/sentinel/archive")

## Use getSentinel_query to search for data (using the session AOI)
records <- getSentinel_query(username = "s5pguest",password = "s5pguest",hub = "s5p", time_range = c("2019-01-01", "2020-01-01"), 
                             platform = "Sentinel-5P") #or "Sentinel-1" or "Sentinel-3"

## Filter the records
colnames(records) #see all available filter attributes
unique(records$processinglevel) #use one of the, e.g. to see available processing levels
unique(records$processingmode) #use one of the, e.g. to see available processing levels

records_filtered <- records[which(records$producttype == "L2__NO2___"),] #filter by Level
#records_filtered <- records_filtered[which(records_filtered$processingmode == "Near real time"),] #filter by Level
records_filtered <- records_filtered[which(records_filtered$processingmode == "Offline"),] #filter by Level

datasets <- getSentinel_data(records = records_filtered[1,]) # download the data (currently set to row 1, remove to download all). Takes a while, and file sizes large!

aois_df <- data.frame(longitude = c(15,15,15), latitude = c(20,20,20))
