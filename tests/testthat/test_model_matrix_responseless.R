context("model_matrix_responseless")

test_that("The model matrix can be constructed", {
  alpha = 1; beta = 0.2
  w0 = 0.2; w1 = 1.2; w3 = 0.5
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w3*d2$z, 1/beta)
  lauren = y ~ x + z
  testmatrix = model_matrix_responseless(lauren, d2)
  expect_true(all(dim(testmatrix)== c(5,3)))
  expect_error(model_matrix_responseless(lauren, d2), NA)
})