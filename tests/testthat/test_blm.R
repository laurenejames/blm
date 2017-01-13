context("blm")

test_that("we can fit blm objects", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testblm = blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), beta, d2)
  expect_true(all(!sapply(testblm, is.null)))
  expect_true(all(dim(testblm$Sigma)==3))
  expect_true(all(dim(testblm$mean)== c(3,1)))
  expect_true(class(testblm) == "blm")
  expect_error(blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), beta, d2), NA)
})

test_that("correct error messages", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testblm = blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), beta, d2)
  expect_error(blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), -beta, d2))
})