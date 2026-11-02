test_that("regressions return two coefficients", {
 set.seed(1); d <- getData(50, sigmaU=0.1, sigmaQ=1)
 expect_length(naiveRegression(d),2); expect_length(correctedRegression(d),2)
})
