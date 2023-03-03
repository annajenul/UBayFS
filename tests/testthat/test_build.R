library(UBayFS)

test_that('loading demo dataset is possible',{
  data(bcw)
  expect_equal(names(bcw), c('data', 'labels','blocks'))
  expect_equal(dim(bcw$data), c(569, 30))
  expect_equal(length(bcw$labels), 569)
})

test_that('correct input types in build.UBaymodel',{

  data(bcw) # dataset
  c <- buildConstraints('max_size', list(10), ncol(bcw$data), rho = 1) # prior constraints
  w <- rep(1, ncol(bcw$data)) # weights

  # run with wrong input
  expect_error(build.UBaymodel(
     data = NULL,
     target = bcw$labels,
     constraints = c,
     weights = w
  ))
  expect_error(build.UBaymodel(
    data = bcw$data,
    target = NULL,
    constraints = c,
    weights = w
  ))
  expect_error(build.UBaymodel(
    data = bcw$data,
    target = bcw$labels,
    constraints = 1,
    weights = w
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = bcw$labels,
    constraints = c,
    weights = -3
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = bcw$labels,
    constraints = c,
    weights = w,
    M = 0
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = bcw$labels,
    constraints = c,
    weights = w,
    tt_split = 1
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = bcw$labels,
    constraints = c,
    weights = w,
    nr_features = 0
  ))
  expect_error(build.UBaymodel(
    data = NULL,
    target = bcw$labels,
    constraints = c,
    weights = w,
    method = 'none'
  ))

})

test_that('ensemble is trained correctly',{

  set.seed(1)
  data(bcw) # dataset
  c <- buildConstraints('max_size', list(10), ncol(bcw$data), rho = 1) # prior constraints
  model <- build.UBaymodel(
    data = bcw$data,
    target = bcw$labels,
    nr_features = 10,
    constraints = c
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(0,9,76,0,0,0,100,100,0,0,0,0,3,100,0,0,4,0,0,0,100,89,100,100,1,18,100,100,0,0))

  set.seed(1)
  model <- build.UBaymodel(
    data = bcw$data,
    target = bcw$labels,
    constraints = c,
    tt_split = 0.9,
    nr_features = 2,
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0, 100, 0, 0))

  set.seed(1)
  model <- build.UBaymodel(
    data = bcw$data,
    target = bcw$labels,
    M = 10,
    nr_features = 10,
    constraints = c,
    method = 'fisher',
  )
  expect_equal(unname(model$ensemble.params$output$counts),
               c(10, 0, 10, 10, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 10, 10, 0, 0, 10, 10, 0, 0))

})
