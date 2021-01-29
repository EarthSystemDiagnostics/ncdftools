#' Extract nearest neighbour timeseries
#'
#' Extract from a netcdf file the timeseries from the grid cells which are
#' nearest to a set of requested coordinates. This function is useful if you
#' want to analyse only a relatively small number of grid cells from a netcdf
#' file and you do not necessarily need the entire spatial field.
#'
#' @param filename path to the netcdf file.
#' @param req.coords matrix or dataframe of requested longitude (1st column) and
#'   latitude (2nd column) coordinates.
#' @param req.var name of the timeseries variable to extract from the netcdf
#'   file.
#' @param time.var name of the time variable in the netcdf file.
#' @param lon.var name of the longitude variable in the netcdf file.
#' @param lat.var name of the latitude variable in the netcdf file.
#' @param verbose logical; if \code{TRUE} processing and run time information
#'   are printed to the terminal.
#'
#' @return A data frame of n rows, where n is the product of the number of
#'   requested coordinates and the time steps in the netcdf file, and five
#'   columns with the requested longitude, requested latitude, index number of
#'   the requested coordinate as character vector, time step, and timeseries
#'   value for the requested variable at each time step.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @author Andrew Dolman with contributions from Thomas Muench
#' @examples
#'
#' filename <- system.file("extdata", "test_data.nc", package = "ncdftools")
#' req.coords = data.frame(lon = c(18, 13), lat = c(59, 52))
#'
#' GetNetCDFAtCoords(filename, req.coords = req.coords, req.var = "t2m")
#'
#' ## print information on the processing step
#' \dontrun{
#' GetNetCDFAtCoords(filename, req.coords = req.coords, req.var = "dummy",
#'                   verbose = TRUE)
#' }
#' @export
GetNetCDFAtCoords <- function(filename, req.coords, req.var, time.var = "time",
                              lon.var = "longitude", lat.var = "latitude",
                              verbose = FALSE) {

  nc1 <- ncdf4::nc_open(filename, readunlim = FALSE)

  if (verbose) {
    cat("\n", "Processing NetCDF file:", "\n")
    print(nc1)
  }

  lats <- ncdf4::ncvar_get(nc1, varid = lat.var)
  lons <- ncdf4::ncvar_get(nc1, varid = lon.var)
  lons <- ifelse(lons > 180, lons - 360, lons)

  nc.coords <- expand.grid(lons = lons, lats = lats)

  ind <- sapply(1 : nrow(req.coords), function(x) {
    which.min(geosphere::distHaversine(req.coords[x, ], nc.coords))
  })

  if (verbose) {
    cat("\n", "Nearest grid cell indices found:", ind, "\n")
  }

  req.nc.coords <- nc.coords[ind, ]

  lon.inds.to.get <- sapply(req.nc.coords[, 1], function(x) which(lons == x))
  lat.inds.to.get <- sapply(req.nc.coords[, 2], function(x) which(lats == x))

  coord.inds.to.get <- cbind(lon.inds.to.get, lat.inds.to.get)

  t1 <- proc.time()
  dat.out <- coord.inds.to.get %>%
    tibble::as_tibble() %>%
    dplyr::mutate(row = 1 : dplyr::n()) %>%
    dplyr::rowwise() %>%
    dplyr::do({
      tmp <- ncdf4::ncvar_get(nc1, varid = req.var,
                              start = c(.data$lon.inds.to.get,
                                        .data$lat.inds.to.get, 1),
                              count = c(1, 1, -1)
                              )
      data.frame(location = as.character(.data$row),
                 date = 1 : length(tmp), req.var = tmp)
    })
  t2 <- proc.time()

  if (verbose) {
    cat("\n", "Run time:\n", t2 - t1, "\n\n")
  }

  dates <- ncdf4::ncvar_get(nc1, varid = time.var, start = c(1), count = c(-1))

  dat.out <- req.coords %>%
    dplyr::mutate(location = as.character(1 : nrow(req.coords))) %>%
    dplyr::left_join(dat.out, by = "location") %>%
    dplyr::mutate(date = rep(dates, nrow(req.coords)),
                  req.var = c(.data$req.var))

  ncdf4::nc_close(nc1)

  return(dat.out)
}
