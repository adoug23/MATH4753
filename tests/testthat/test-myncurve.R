library(testthat)
library(FALL24DOUG0060)
#testthat::test_check("FALL24DOUG0060")


test_that("mu is correct", {
  result <- FALL24DOUG0060::myncurve(0, 1, 2)
  expect_equal(result$mu, 0)
})

test_that("sigma is correct", {
  result <- FALL24DOUG0060::myncurve(0, 1, 2)
  expect_equal(result$sigma, 1)
})

test_that("area is correct for P(X <= a)", {
  result <- FALL24DOUG0060::myncurve(0, 1, 2)
  expected_area <- pnorm(2, mean = 0, sd = 1)
  expect_equal(result$area, expected_area)
})
