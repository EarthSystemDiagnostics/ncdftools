# ncdftools 0.1.1

* fixed missing package reference to dplyr which caused `GetNetCDFAtCoords()` to
  fail when package dplyr has not been explicitly loaded before;
* fixed and cleaned package dependencies;
* set up unit test infrastructure;
* cleaned source code and improved function documentation;
* the name of the requested netcdf variable is now returned in the output from
  `GetNetCDFAtCoords()`.

# ncdftools 0.1.0

* Initial package version including from deprecated package 'ecustools' only the
  function `GetNetCDFAtCoords()` in its original form regarding source code and
  documentation. No other netcdf-related function from 'ecustools' has been
  classified as suitable for the new 'ncdftools' package.
