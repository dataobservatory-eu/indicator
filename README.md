
<!-- README.md is generated from README.Rmd. Please edit that file -->

# indicators

<!-- badges: start -->

[![R-CMD-check](https://github.com/dataobservatory-eu/indicator/workflows/R-CMD-check/badge.svg)](https://github.com/dataobservatory-eu/indicator/actions)
<!-- badges: end -->

The goal of indicators is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dataobservatory-eu/indicator")
```

## Create a mini database

This code will create a mini database from select Eurostat indicators:

``` r
library(indicators)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
require(DBI)
#> Loading required package: DBI
#> Warning: package 'DBI' was built under R version 4.0.3
require(dplyr)
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.0.4
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
# Make sure that it will work in a vignette, too:
data_raw_dir <- ifelse (dir.exists('data-raw'), 'data-raw', file.path('..', 'data-raw'))

create_eurostat_database (
  ids = c("ISOC_R_BLT12_I", "isoc_cicce_use", "teicp090", "tin00028"),
  db_path = file.path(data_raw_dir, "test_db.db")
)
#> Warning: Closing open result set, pending rows
#> Warning: Closing open result set, pending rows

#> Warning: Closing open result set, pending rows
#> Table isoc_r_blt12_i cached at C:\Users\DANIEL~1\AppData\Local\Temp\RtmpsrwOkq/eurostat/isoc_r_blt12_i_date_code_FF.rds
#> Table isoc_cicce_use cached at C:\Users\DANIEL~1\AppData\Local\Temp\RtmpsrwOkq/eurostat/isoc_cicce_use_date_code_FF.rds
#> Table teicp090 cached at C:\Users\DANIEL~1\AppData\Local\Temp\RtmpsrwOkq/eurostat/teicp090_date_code_FF.rds
#> Table tin00028 cached at C:\Users\DANIEL~1\AppData\Local\Temp\RtmpsrwOkq/eurostat/tin00028_date_code_FF.rds
```

``` r
disc_con <- dbConnect(RSQLite::SQLite(), file.path(data_raw_dir, "test_db.db") )

message  ("The following tables are present in the database: ")
#> The following tables are present in the database:
DBI::dbListTables(disc_con)
#> [1] "indicator" "labelling" "metadata"
```

``` r
db_metadata <- DBI::dbReadTable(disc_con, "metadata")

internet_use_indicators <- db_metadata %>% 
  select ( all_of(c("indicator_code", "description_indicator", "actual", "missing"))) %>%
  filter ( grepl("internet use", tolower(.data$description_indicator)) ) %>%
  relocate ( .data$indicator_code, .after = .data$missing )
```

And now select the indicators:

``` r
test_indicators <- DBI::dbGetQuery(
  conn = disc_con, 
  "SELECT indicator_code, value, geo, time, estimate FROM indicator 
   WHERE indicator_code = ?", 
  params = list(internet_use_indicators$indicator_code)
)

head(test_indicators,12)
#>                         indicator_code value       geo  time estimate
#> 1  eurostat_tin00028_i_ilt12_ind_total    73        AT 14245   actual
#> 2  eurostat_tin00028_i_ilt12_ind_total    76        BE 14245   actual
#> 3  eurostat_tin00028_i_ilt12_ind_total    45        BG 14245   actual
#> 4  eurostat_tin00028_i_ilt12_ind_total    50        CY 14245   actual
#> 5  eurostat_tin00028_i_ilt12_ind_total    64        CZ 14245   actual
#> 6  eurostat_tin00028_i_ilt12_ind_total    79        DE 14245   actual
#> 7  eurostat_tin00028_i_ilt12_ind_total    87        DK 14245   actual
#> 8  eurostat_tin00028_i_ilt12_ind_total    68        EA 14245   actual
#> 9  eurostat_tin00028_i_ilt12_ind_total    72        EE 14245   actual
#> 10 eurostat_tin00028_i_ilt12_ind_total    45        EL 14245   actual
#> 11 eurostat_tin00028_i_ilt12_ind_total    62        ES 14245   actual
#> 12 eurostat_tin00028_i_ilt12_ind_total    68 EU27_2007 14245   actual
```

Another simple query:

``` r
DBI::dbGetQuery(conn = disc_con, 
                "SELECT COUNT(*) FROM indicator 
                WHERE indicator_code = ?", 
                params = list(internet_use_indicators$indicator_code)
                )
#>   COUNT(*)
#> 1      445
#> 2      445
#> 3      445
```

``` r
DBI::dbDisconnect(disc_con)
```

See the vignettes:

``` r
vignette( package="indicators")
#> no vignettes found
```

Please note that the indicators project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
