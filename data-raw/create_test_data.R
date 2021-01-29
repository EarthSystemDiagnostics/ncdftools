##
## aim: create an example netcdf file for the tests and examples.
## relation: ncdftools package
##
## Thomas Muench, AWI, 2021
##

library(ncdf4)

## define variables
lon <- seq(0, 350, 10)
lat <- seq(90, -90, -10)
tim <- 2011 : 2020

v1 <- rnorm(n = length(lon) * length(lat) * length(tim), mean = 15, sd = 5)
v2 <- rep(seq(length(lat) * length(lon)), length(tim))


## define dimensions
londim <- ncdim_def(name = "longitude", units = "degrees_east",
                    vals = lon, longname = "longitude")
latdim <- ncdim_def(name = "latitude", units = "degrees_north",
                    vals = lat, longname = "latitude")
timdim <- ncdim_def(name = "time", units = "years",
                    tim, longname = "time", unlim = TRUE,
                    calendar = "standard")

## define variables
nc.d1 <- ncvar_def(name = "t2m", units = "degree Celsius",
                   dim = list(londim, latdim, timdim), missval = NA,
                   longname = "2m temperature")
nc.d2 <- ncvar_def(name = "grid_index", units = "",
                   dim = list(londim, latdim, timdim), missval = NA,
                   longname = "grid cell index running along space")

## create file
file <- system.file("extdata", "test_data.nc", package = "ncdftools",
                    mustWork = TRUE)
ncout <- nc_create(file, vars = list(nc.d1, nc.d2))

## put variables
ncvar_put(ncout, varid = nc.d1, vals = v1)
ncvar_put(ncout, varid = nc.d2, vals = v2)

## put additional attributes
ncatt_put(ncout, "longitude", "axis", "X")
ncatt_put(ncout, "latitude", "axis", "Y")
ncatt_put(ncout, "time", "axis", "T")

ncatt_put(ncout, varid = 0, attname = "R-package", attval = "ncdftools")
ncatt_put(ncout, varid = 0, attname = "history",
          attval = as.character(Sys.Date()))

## write file
nc_close(ncout)
