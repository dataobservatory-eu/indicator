# indicators 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added vignettes `Naming Conventions` and `Data Acquisition from Eurostat`.
* Create the `test_db.db` from the `README.md` example.

# indicators 0.0.0.9001
* na.approx works with unit tests, and has working examples, except for forecasting. Forecasting, and potentially all approximation should be based in the future on [timetk](https://business-science.github.io/timetk/).

# indicators 0.0.0.9002
* indicator_forecast now works, but with dependency forecast.

# indicators 0.0.0.9003
* tidy_indicator() and get_eurostat_indicator() is tested against a wide range of real-life data, and it is now passing a number of unit tests. 

# indicators 0.0.0.9004
* tidy_indicator and get_eurostat_indicator is tested against a wide range of real-life data, and it is now passing a number of unit tests. 

# indicators 0.0.0.9005
* create_observatory_palette() and add_country_groups() to create consistent visualizations in the long-form documentation. An example of creating a new indicator in data-raw.

# indicators 0.0.0.9008
* is_unique_observations() is user-facing and test_unique_observations() only in approximation flows.
* [![Codecov test coverage](https://codecov.io/gh/dataobservatory-eu/indicator/branch/master/graph/badge.svg)](https://codecov.io/gh/dataobservatory-eu/indicator?branch=master)
* The indicator S3 class is added for manual indicator creation or imports from Zenodo with a vignette.
