test_that("output is numeric", {
  expect_equal(class(PIT_global(
    ycal=c(3,4,5), yhat=c(31,41,51),mse=1)), "numeric")
})



