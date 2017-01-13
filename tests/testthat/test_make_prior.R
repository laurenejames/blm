context("make_prior")

test_that("Error messages work", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testprior = make_prior(lauren, alpha, mu = c(0,0))
  expect_error(make_prior(lauren, -alpha, mu = c(0,0)))
  expect_error(make_prior(lauren, alpha, mu = c(0,0)), NA)
})

test_that("We can make a prior distribution", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testprior = make_prior(lauren, alpha, mu = c(0,0))
  expect_true(all(dim(testprior$Sigma) == 3))
})