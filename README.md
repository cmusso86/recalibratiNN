
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recalibratiNN

<!-- badges: start -->
<!-- badges: end -->

This package aims to provide a post processing method to recalibrate
fitted Gaussian models.

<img src="man/figures/recalibratiNN.png" style="image-align: center;"
width="200" />

## Installation

You can install the current version of recalibratiNN from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cmusso86/recalibratiNN")

library(recalibratiNN)
```

Alternately, one can use the `pacman` package to both install and
download.

``` r
if(!require(pacman)) install.packages("pacman")
pacman::p_load_current_gh("cmusso86/recalibratiNN")
```

## Understanding calibration/miscalibration

This is a basic example which shows a common problem of miscalibration.
To do so, we provide a visualization of what miscalibration is, we
generated an heterocedastic model and fitted with a simple linear
regression.

``` r

## basic artificial model example


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
y <- rnorm(n,mu(x), sigma_v(x))


# slipting data 
x_train <- x[1:(n*split)]
y_train <- y[1:(n*split)]

x_cal <- x[(n*split+1):n]
y_cal <- y[(n*split+1):n]

# fitting a simple linear model
model <- lm(y_train ~ x_train)
```

The points, the true mean and the regression can be observed in the
graph below. We can see that the linear model (dashed black line)
underestimates the mean for both small values of x or greater values of
x. Furthermore, the linear model overestimates de variance for lower
values of x (all points fell within tha IC). On the other hand, for
higher values, the model is under estimating the true variance. Thus,
you cannot correctly quantify your uncertainty. This is an example of a
miscalibrated model.

``` r
pacman::p_load(tidyverse)
pacman::p_load_gh("AllanCameron/geomtextpath")

# use predict to get the confidence intervals
data_predict <- predict(model, 
                        newdata=data.frame(x_train=x_cal), 
                        interval = "prediction") %>% 
  as_tibble() %>% 
  dplyr::mutate(x_cal=x_cal, 
                y_cal=y_cal,
          IC=ifelse(y_cal<=upr&y_cal>=lwr, "in", "out")) 

data_predict %>% 
  ggplot(aes(x_cal))+
  geom_point(mapping=aes(x_cal, y_cal, color=IC), alpha=0.6)+
  geom_labelline(aes( y=mu(x_cal), label="True Mean" ), 
                 size=1.8, hjust=-0.01, linewidth=0.7, color="red" )+
  geom_smooth(aes( y=y_cal ), color="black",se=F,
                   method="lm", formula=y~x,linetype="dashed" )+
  scale_color_manual("IC 95%", values=c("#00822e", "#2f1d86"))+
  theme_classic()
   
```

<img src="man/figures/disp.png" width="80%" style="display: block; margin: auto;" />

## Using the recalibratiNN package

With real data, and specially with higher dimention models, we will not
be able to evaluate miscalibration the way we described above. In this
sense, one commonly employed strategy for analyzing global calibration
is the examination of the histogram of Probability Integral Transform
(PIT) values. PIT values represent the estimated cumulative probability
in the predicted distribution for each observed value. This technique
involves constructing a histogram/density plot of the cumulative
distribution functions estimated by the model applied to each
corresponding observation. If the model is well specified, this
distribution will approach the Uniform distribution.

### Observing global calibration/miscalibration

Using the fuction `PIT_values()` function to obtain pit-values for the
fitted model for a calibration set. To do so, we first calcultae some
values that the PIT_values() function will require, such as the
predicted values, prof the fitted model, to new observations. Then the
Meas Squared Erros of the calibration (or the validation set that we
will call here “calibration set”).

``` r
library(recalibratiNN)

# predictions for the calibraion set
y_hat <- predict(model, 
                 data.frame(x_train = x_cal))

# MSE from calibration set
MSE_cal <- mean((y_hat - y_cal)^2) # a little different from MSE from training set

# USE tha recalibratiNN::PIT_local() to calculate the pit-values.

pit <- PIT_global(ycal=y_cal, 
                  yhat=y_hat, 
                  mse=MSE_cal)

head(pit)
#> [1] 0.04551257 0.42522358 0.81439164 0.69119416 0.44043239 0.99770918
```

Then, one can proceed with visualizing this the histogram and testing if
it fits a uniform distribution using `gg_PIT_global()`. Refer to the
documentation to learn other oarameters for customization.

``` r
gg_PIT_global(pit)
```

<img src="man/figures/ggP.png" width="80%" style="display: block; margin: auto;" />

In this case, since we are fiting an lm() to an heterocedastic model,
the histogram show clear indication a misscalibration. In the image we
also present the p_value from the hispothesis testing of
Kolmogorov-Smirnov test, performed with the `ks.test()` function from
`stats` package.

It is also to use other visualization function of the package with the
`gg_QQ_global`. This graph shows the cumulative predictive distribution
in the x-axis versus the empirical cumulative distribution and require
four parameters.

``` r
gg_QQ_global(pit,
             y_cal, 
             y_hat, 
             MSE_cal)
```

<img src="man/figures/ggQ.png" width="80%" style="display: block; margin: auto;" />

#### Local Calibration

However, global calibration can be misleading. A model can look
calibrated globally, but can show drastic problems locally. For
instance, the presented model shows a coverage of nearly 95%, which is
what was expected in a 95% confidence interval. However, locally we see
that this spread of “errors” is not uniform, that is, there are regions
the model systematically makes more mistake.

This is observed in the graphs bellow. First, one needs to calculate
local pit-values with `PIT_local()` function. This function partitions
the covariate spaces in n clusters (the default is 6) with a k-means
algorithm and then search for neighbours to the centroids of each
cluster.

``` r
# calculating local PIT 
pit_local <- PIT_local(xcal = x_cal, 
                       ycal=y_cal, 
                       yhat=y_hat, 
                       mse=MSE_cal)

gg_PIT_local(pit_local)
```

Observing this graph, we notice the model is uncalibrated in different
ways thoughout the covariates space.

<img src="man/figures/plot1PL.png" width="80%" style="display: block; margin: auto;" />

Or you can facet the graph, and include ther customizations. Please
refer to documentation to learn more.

``` r
gg_PIT_local(pit_local, 
             pal="Purples",
             alpha=0.9,
             facet=T)
```

In the first part, the model is overestimating the variance and
underestimating the mean. In the middle reagion the model is more
calibrated. By the end ir is under estimating the variance. This is a
clear case that would benefit from local calibration, since each
partition behaves differently.

<img src="man/figures/plot2PL.png" width="80%" style="display: block; margin: auto;" />

Alternatively you can observe the local miscalibration in the QQ-graph.

``` r
gg_QQ_local(pit_local)
gg_QQ_local(pit_local, facet=T)
```

<img src="man/figures/plotQL1.png" width="80%" style="display: block; margin: auto;" /><img src="man/figures/plotQL2.png" width="80%" style="display: block; margin: auto;" />

# Recalibration

The quantile recalibration will generate Monte Carlo samples of an
unknown (but more calibrated) predictive distribution. It will also
provide recalibrated weighted mean and variance.

The `recalibrate()` function is an implementation of the method
described by Torres et al. (2023), which is inspired in the Aproximate
Bayesian Computation. This method can be either applied globally or
locally. In this heterocedastic example you will see the local
calibration performs better. The local calibration uses a KNN algorithm
to search for neighbors from the calibration set that are the nearest to
the new/test set provided.

To perform recalibration you will need to provide the pit-values (always
the global pit-values, regardless of the type of recalibration) and the
Mean Squared Error of the calibration set. The search for new neighbors
can be performed in the covariates level, of any intermediate layer (in
case of a Neural network) or even the output layer.

The function will then calculate the pit-values and use the Inverse
Transform Theorem to produce recalibrated samples. The size of the
vicinity is set to be 10% of the calibration set, but you can custom it
with the p_neighbours argument.

``` r
# new data 
x_new <- runif(n/5, 1, 10)
y_hat_new <- predict(model,
                     data.frame(
                       x_train=x_new)
                     )

# recalibration
rec <- recalibrate(yhat_new=y_hat_new,
                   space_new = x_new,
                   space_cal= x_cal,
                   pit_values=pit,
                   mse=MSE_cal,
                   type="local",
                   p_neighbours=0.2)
```

Because in this artificial example we know the true model/process that
generated this data, so we can calculate the empirical pit-values and
verify if the predictions are now better calibrated. Notice that is an
exercise and in real life you probably wont be able to do so, unless you
chose to look ate you test set.

``` r
# what youd be the real observations in this example
y_new_real <- rnorm(n/5, 
                    mu(x_new), 
                    sigma_v(x_new))

# retrieving the weighted samples
y_new_recalib <- rec$y_samples_calibrated_wt

# empiric p-value distribution
pit_new <- purrr::map_dbl(
  1:length(y_new_real), ~{
    mean(y_new_recalib[.,] <=y_new_real[.] )
  })

gg_PIT_global(pit_new)
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="80%" style="display: block; margin: auto;" />

We see now that the pit-values are aproximatedly uniform, at least
globally. However, we will se the local calibration is also improved.

``` r
#gg_PIT_local(PIT_local()
```
