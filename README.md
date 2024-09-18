
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sola

<!-- badges: start -->
<!-- badges: end -->

The goal of `sola` is to provide tools for solar resource assessment and
utilization. The package implements functions for calculating solar
radiation based on sunshine hours, latitude, and other related
parameters. The main advantage of `sola` is the use of vectorized
operations, allowing efficient computation of large datasets.
\>\>\>\>\>\>\> Stashed changes

## Installation

You can install the development version of sola from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("renliang1996/sola")
```

## Example

Here is a simple example of calculating solar radiation:

``` r
library(sola)

# Calculate solar radiation for March 15, 2023, at latitude 35 with 8 hours of sunshine
sol_rad(lat = 35.0, date = as.Date("2023-03-15"), ssd = 8)
#> [1] 17.3613
```

``` r
#> [1] 17.3613
```

### Vectorization calculation

To address the limitation of certain packages that can only input a
single value, we can input a vector for computation:

``` r
latitudes <- c(35, -15, 50)
dates <- as.Date(c("2023-03-15", "2024-06-21", "2023-11-01"))
sunshine_hours <- c(8, 10, 6)

sol_rad(lat = latitudes, date = dates, ssd = sunshine_hours)
#> [1] 17.361305 18.591773  7.255484
```

``` r
#> [1] 17.361305 18.591773  7.255484
```

### Performance Testing with Large Datasets

The following example demonstrates how `sola` can handle a dataset with
10,000 records efficiently:

``` r
set.seed(123)
test_lat <- runif(10000, -90, 90)
test_date <- as.Date("2023-01-01") + sample(1:365, 10000, replace = TRUE)
test_ssd <- runif(10000, 0, 24)

system.time({
  results <- sol_rad(lat = test_lat, date = test_date, ssd = test_ssd)
})
#> 用户 系统 流逝 
#> 0.05 0.00 0.05
```

``` r
#> user system elapsed 
#> 0.04 0.00 0.05
# View the first few results
head(results)
#> [1] 24.729763 29.402079 23.245299  1.090254  0.000000  4.183973
```

``` r
#> [1] 24.729763 29.402079 23.245299  1.090254  0.000000  4.183973
```
