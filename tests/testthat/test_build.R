library(UBayFS)

test_that("loading demo dataset is possible",{
  d <- loadWisconsin()
  expect_equal(names(d), c("data", "labels"))
  expect_equal(dim(d$data), c(569, 30))
  expect_equal(length(d$labels), 569)
})

test_that("correct input types in build.UBaymodel",{

  d <- loadWisconsin() # dataset
  c <- buildConstraints("max_size", list(10), ncol(d$data), rho = 1) # prior constraints
  w <- rep(1, ncol(d$data)) # weights

  # run with wrong input
  expect_error(build.UBaymodel(
     data = NULL,
     target = d$labels,
     constraints = c,
     weights = w
  ))
  expect_error(build.UBaymodel(
    data = d$data,
    target = NULL,
    constraints = c,
    weights = w
  ))
  expect_error(build.UBaymodel(
    data = d$data,
    target = d$labels,
    constraints = 1,
    weights = w
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = d$labels,
    constraints = c,
    weights = -3
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = d$labels,
    constraints = c,
    weights = w,
    M = 0
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = d$labels,
    constraints = c,
    weights = w,
    tt_split = 1
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = d$labels,
    constraints = c,
    weights = w,
    nr_features = 0
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = d$labels,
    constraints = c,
    weights = w,
    method = "none"
  ))

})

test_that("ensemble is trained correctly",{

  set.seed(1)
  d <- loadWisconsin() # dataset

  model <- build.UBaymodel(
    data = d$data,
    target = d$labels,
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(0, 10, 74, 0, 0, 0, 100, 100, 0, 0, 0, 0, 6, 100, 0, 0, 6, 0, 0, 0, 100, 86, 100, 100, 2, 16, 100, 100, 0, 0))

  model <- build.UBaymodel(
    data = d$data,
    target = d$labels,
    tt_split = 0.9,
    nr_features = 2,
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 100, 0, 0))

  model <- build.UBaymodel(
    data = d$data,
    target = d$labels,
    M = 10,
    method = "Laplacian score",
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(10, 0, 10, 10, 0, 0, 10, 10, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 10, 0, 10, 10, 0, 0, 0, 10, 0, 0))

})
