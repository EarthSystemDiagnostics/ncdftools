test_that("coordinate extraction works", {

  filename <- system.file("extdata", "test_data.nc", package = "ncdftools")

  # test to extract certain coordinates
  lat <- 90
  lon <- 0
  actual <- GetNetCDFAtCoords(filename,
                              req.coords = data.frame(lon = lon, lat = lat),
                              req.var = "grid_index")
  expected <- data.frame(
    lon = rep(lon, 10),
    lat = rep(lat, 10),
    location = as.character(rep(1, 10)),
    date = 2011 : 2020,
    grid_index = rep(1, 10),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)

  lat <- -20
  lon <- 180
  actual <- GetNetCDFAtCoords(filename,
                              req.coords = data.frame(lon = lon, lat = lat),
                              req.var = "grid_index")
  n <- 11 * 36 + 19
  expected <- data.frame(
    lon = rep(lon, 10),
    lat = rep(lat, 10),
    location = as.character(rep(1, 10)),
    date = 2011 : 2020,
    grid_index = rep(n, 10),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)

  # test extracting the other field variable
  expect_error(
    actual <- GetNetCDFAtCoords(filename,
                                req.coords = data.frame(lon = lon, lat = lat),
                                req.var = "t2m"), NA)

  expect_equal(colnames(actual), c("lon", "lat", "location", "date", "t2m"))

})
