test_that("coordinate extraction works", {

  filename <- system.file("extdata", "test_data.nc", package = "ncdftools")

  # test to extract certain coordinates
  lat <- 90
  lon <- 0
  actual <- GetNetCDFAtCoords(filename,
                              req.coords = data.frame(lon = lon, lat = lat),
                              req.var = "dummy")
  expected <- data.frame(
    lon = rep(lon, 10),
    lat = rep(lat, 10),
    location = as.character(rep(1, 10)),
    date = 2011 : 2020,
    req.var = rep(1, 10),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)

  lat <- -20
  lon <- 180
  actual <- GetNetCDFAtCoords(filename,
                              req.coords = data.frame(lon = lon, lat = lat),
                              req.var = "dummy")
  n <- 11 * 36 + 19
  expected <- data.frame(
    lon = rep(lon, 10),
    lat = rep(lat, 10),
    location = as.character(rep(1, 10)),
    date = 2011 : 2020,
    req.var = rep(n, 10),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)

  # test extracting the other field variable
  expect_error(GetNetCDFAtCoords(filename,
                                 req.coords = data.frame(lon = lon, lat = lat),
                                 req.var = "t2m"), NA)

})
