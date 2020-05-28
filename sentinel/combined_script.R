#devtools::install_github("16EAGLE/getSpatialData") ## comment out to install getSpatialData from github.
library(getSpatialData)
library(raster)
library(sf)
library(sp)
library(ncdf4)
library(ggplot2)


services_avail()
## Define an AOI (either matrix, sf or sp object)
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

ncpath = "~/R/sentinel/archive/get_data/Sentinel-5 Precursor/"


no2df = NULL

# get filenames
no2files = list.files(ncpath, patter="*nc", full.names=TRUE)

# save start time
start.time <- Sys.time()

# loop over filenames, open each one and add to dataframe
for (i in seq_along(no2files)) {
  nc <- nc_open(no2files[i])
  # get variables of interest
  no2tc <- ncvar_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column")  
  # apply multiplication factor for unit conversion
  no2tc <- no2tc*mfactor$value
  lat <- ncvar_get(nc, "PRODUCT/latitude")
  lon <- ncvar_get(nc, "PRODUCT/longitude")
  # concatenate the new data to the global data frame
  no2df <- rbind(no2df, data.frame(lat=as.vector(lat), 
                                   lon=as.vector(lon), 
                                   no2tc=as.vector(no2tc)))
  # close file
  nc_close(nc)
}

# measure elapsed time
stop.time <- Sys.time()
time.taken <- stop.time - start.time

print(paste(dim(no2df)[1], "observations read from", length(no2files), 
            "files in", time.taken, "seconds"))

## define the function to plot the dataset
PlotRegion <- function(df, latlon, title) {
  # Plot the given dataset over a geographic region.
  #
  # Args:
  #   df: The dataset, should include the no2tc, lat, lon columns
  #   latlon: A vector of four values identifying the botton-left and top-right corners 
  #           c(latmin, latmax, lonmin, lonmax)
  #   title: The plot title
  
  # subset the data frame first
  df_sub <- subset(df, no2tc!=fillvalue & lat>latlon[1] & lat<latlon[2] & lon>latlon[3] & lon<latlon[4])
  subtitle = paste("Data min =", formatC(min(df_sub$no2tc, na.rm=T), format="e", digits=2), 
                   "max =", formatC(max(df_sub$no2tc, na.rm=T), format="e", digits=2))
  
  ggplot(df_sub, aes(y=lat, x=lon, fill=no2tc)) + 
    geom_tile(width=1, height=1) +
    borders('world', xlim=range(df_sub$lon), ylim=range(df_sub$lat), 
            colour='gray90', size=.2) + 
    theme_light() + 
    theme(panel.ontop=TRUE, panel.background=element_blank()) +
    scale_fill_distiller(palette='Spectral', 
                         limits=c(quantile(df_sub, .7, na.rm=T), 
                                  quantile(df_sub, .999, na.rm=T))) +
    coord_quickmap(xlim=c(latlon[3], latlon[4]), ylim=c(latlon[1], latlon[2])) +
    labs(title=title, subtitle=subtitle, 
         x="Longitude", y="Latitude", 
         fill=expression(molecules~cm^-2))
} 


GB.coords = c(50, 60, -10, 3) ## set the coordinates box for GB
PlotRegion(no2df, GB.coords, expression(NO[2]~total~vertical~column~over~Great Britain)) ## run the plot function


aoi <- get_aoi(type = "sp") ## get the aoi previously set from drawn box

bbox <- as.data.frame(aoi@bbox)

aoi.coords <- c(bbox$min[2],bbox$max[2],bbox$min[1],bbox$max[1])

PlotRegion(no2df, aoi.coords, expression(NO[2]~total~vertical~column~over~aoi))
