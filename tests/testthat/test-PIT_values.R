test_that("output is a vector", {
  expect_equal(class(PIT_values(x_cal=c(1,2,3),
                          y_cal=c(0.27, -0.37,  1.1),
                          model=lm(c(1,2,3) ~ c(0.27, -0.37,  1.1)))),
         "numeric")
})
