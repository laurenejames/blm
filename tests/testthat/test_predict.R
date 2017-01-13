context("predict.blm")

test_that("Predictions calculated correctly", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testblm = blm(lauren, make_prior(lauren,alpha, mu = c(0,0)), beta, d2)
  newdata = data.frame(x = rnorm(5), z = rnorm(5))
  newdata$y = rnorm(5, w0+w1*newdata$x+w3*newdata$z, 1/beta)
  testpred = predict(testblm, newdata, beta)
  expect_true(class(testpred) == "data.frame")
  expect_true(all(dim(testpred) == c(5,2)))
  expect_error(predict(testblm, newdata, beta), NA)
})