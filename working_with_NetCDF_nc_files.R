setwd("C:/.../ClimateResearch")
getwd()

library(ggplot2) # package for plotting
library(ncdf4) # package for netcdf manipulation
library(sf) # package for raster manipulation
library(terra) # package for geospatial analysis
library(data.table)

#nc_data <- nc_open('nc_sample_data.nc')
nc_data <- nc_open('HadSST.4.0.1.0_median.nc')
# Save the print(nc) dump to a text file
sink('nc_sample_data_metadata.txt')
print(nc_data)
sink()
#from metadate we can see that there are
#tos[lon,lat], 2 dimensions, near surface air temperature in °C 
#lon, logitude, x-axis, size = 360
#lat, latitude, y-axis, size = 180
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
head(lon)
#get the tas values
tos.array <- ncvar_get(nc_data, "tos") # store the data in a 2-dimensional array
dim(tos.array)
mtx <- apply(X = tos.array, MARGIN = 2, FUN = c) #convert to matrix
dim(mtx)
summary(mtx)
#value was used for missing data, also shown in metadata txt file
fillvalue <- ncatt_get(nc_data, "tos", "missing_value")
fillvalue
#all data read, file can be closed
nc_close(nc_data) 

#working with data
#replace fillvalue with NA
#tas.array[tas.array == fillvalue$value] <- NA
r <- rast(t(mtx)) #t(..) performs array transpose
r <- flip(r, direction='vertical')
plot(r)

