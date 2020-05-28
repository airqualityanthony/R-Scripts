library(ncdf4)
library(ggplot2)

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
  mfactor = ncatt_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column", 
                      "multiplication_factor_to_convert_to_molecules_percm2")
  fillvalue = ncatt_get(nc, "DETAILED_RESULTS/nitrogendioxide_total_column", 
                        "_FillValue")
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
