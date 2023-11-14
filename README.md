
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recalibratiNN

<!-- badges: start -->
<!-- badges: end -->

This package aims to provide post processing methods to recalibrate
fitted Gaussian models.

<img src="man/figures/recalibratiNN.png" style="image-align: center;"
width="200" />

## Installation

You can install the development version of recalibratiNN from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cmusso86/recalibratiNN")
#> Using github PAT from envvar GITHUB_PAT
#> Skipping install of 'recalibratiNN' from a github remote, the SHA1 (14be5faa) has not changed since last install.
#>   Use `force = TRUE` to force installation

library(recalibratiNN)
#> Warning: replacing previous import 'dplyr::lag' by 'stats::lag' when loading
#> 'recalibratiNN'
#> Warning: replacing previous import 'dplyr::filter' by 'stats::filter' when
#> loading 'recalibratiNN'
```

Alternately, one can use the `pacman` package to both install and
download.

``` r
if(!require(pacman)) install.packages("pacman")
#> Loading required package: pacman
pacman::p_load_current_gh("cmusso86/recalibratiNN")
```

## Example

### Diagnosing miscalibration

#### Global Calibration

This is a basic example which shows you how to solve a common problem of
miscalibration. To do so, we created an heterocedastic model and fitted
with a simple linear regression.

``` r
library(recalibratiNN)

## basic example code


set.seed(42)
n <- 10000
split <- 0.8

# Auxiliary functions
mu <- function(x1){
10 + 5*x1^2
}

sigma_v <- function(x1){
30*x1
}

# generating heterocedastic data (true model)

x <- runif(n, 1, 10)
y <- rnorm(n, mu(x), sigma_v(x))


# slipting data 
x_train <- x[1:(n*split)]
y_train <- y[1:(n*split)]

x_cal <- x[(n*split+1):n]
y_cal <- y[(n*split+1):n]

# fitting a simple linear model
model <- lm(y_train ~ x_train)
```

The points, the true mean and the regression can be observed in the
graph below.

using the fuction PIT_values() to obtain pit-values for the fitted model
for a calibration set.

``` r
y_hat <- predict(model, newdata=data.frame(x_train=x_cal))

MSE_cal <- mean((y_hat - y_cal)^2)

pit <- PIT_global(ycal=y_cal, yhat=y_hat, mse=MSE_cal)

head(pit)
#> [1] 0.04551257 0.42522358 0.81439164 0.69119416 0.44043239 0.99770918
```

Then, one can proceed with visualizing this the histogram and testing if
it fits a uniform distribution.

``` r
gg_PIT_global(pit)
```

<img src="ggP.png" width="80%" style="display: block; margin: auto;" />

In this case, since we are fiting an lm() to an heterocedastic model,
the histogram seems shifted indication a misscalibration. In the image
we also present the p_value from the hispothesis testing of
Kolmogorov-Smirnov test, performed with the `ks.test()` function from
`stats` package.

One can also want to visualize the miscalibration as QQ-plot-like graph,
showing the cumulative predictive distribution in the x-axis versus the
empirical cumulative distribution.

``` r
gg_QQ_global(pit, y_cal, y_hat, MSE_cal)
```

<img src="man/figures/ggQ.png" width="80%" style="display: block; margin: auto;" />

#### Local Calibration

``` r
pit_local <- PIT_local(xcal = x_cal, ycal=y_cal, yhat=y_hat, mse=MSE_cal)

gg_PIT_local(pit_local)
```

<img src="man/figures/plot1PL.png" width="80%" style="display: block; margin: auto;" />

Or you can facet the graph:

``` r
gg_PIT_local(pit_local, facet=T)
```

<img src="man/figures/plot2PL.png" width="80%" style="display: block; margin: auto;" />

Alternatively you can observe the miscalibration in the QQ-graph.

``` r
gg_QQ_local(pit_local)
gg_QQ_local(pit_local, facet=T)
```

<img src="man/figures/plotQL1.png" width="80%" style="display: block; margin: auto;" /><img src="man/figures/plotQL2.png" width="80%" style="display: block; margin: auto;" />
