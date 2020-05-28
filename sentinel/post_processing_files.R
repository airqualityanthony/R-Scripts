library(ncdf4)
library(ggplot2)


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
} ## define the function to plot the dataset


GB.coords = c(50, 60, -10, 3) ## set the coordinates box for GB
PlotRegion(no2df, GB.coords, expression(NO[2]~total~vertical~column~over~Europe)) ## run the plot function


aoi <- get_aoi(type = "sp") ## get the aoi previously set from drawn box

bbox <- as.data.frame(aoi@bbox)

aoi.coords <- c(bbox$min[2],bbox$max[2],bbox$min[1],bbox$max[1])

PlotRegion(no2df, aoi.coords, expression(NO[2]~total~vertical~column~over~Europe))
