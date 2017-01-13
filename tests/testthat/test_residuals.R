context("residuals.blm")

test_that("Residuals correctly calculated", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testblm = blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), beta, d2)
  testres = residuals(testblm)
  expect_true(class(testres) == "numeric")
  expect_true(length(testres) == 5)
  expect_error(residuals(testblm), NA)
})