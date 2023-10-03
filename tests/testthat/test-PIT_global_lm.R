test_that("output is numeric", {
  expect_equal(class(PIT_values_lm(
    y_cal=c(3,4,5),
    cdf=pnorm)), "numeric")
})



