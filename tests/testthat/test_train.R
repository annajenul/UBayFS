library(UBayFS)

test_that('correct results in train.UBaymodel',{

  set.seed(1)
  data(bcw) # dataset
  c <- buildConstraints('max_size', list(10), ncol(bcw$data), rho = 1) # prior constraints
  w <- rep(1, ncol(bcw$data)) # weights
  model <- build.UBaymodel(
    data = bcw$data,
    target = bcw$labels,
    constraints = c,
    weights = w,
    M = 100,
    tt_split = 0.75,
    nr_features = 10,
    method = 'mRMR',
    optim_method = 'GA',
    popsize = 50,
    maxiter = 100,
    shiny = FALSE
  )

  # run with wrong input
  expect_error(train(list(data = cbind(c(1,2),c(2,1)),
                                label = c(0,1),
                                user.param = list(),
                                ensemble.params = list(),
                                optim.params = list())))

  # train model (standard)
  set.seed(1)
  model <- UBayFS::train(model)
  expect_s3_class(model, 'UBaymodel')
  expect_equal(unname(which(model$output$feature_set == 1)), c(3,7,8,14,21,22,23,24,27,28))

  # train model (with distinct weights)
  set.seed(1)
  model <- setWeights(model, rep(c(1,100,10,20,100,20,50,30,40,90), 3))
  model <- UBayFS::train(model)
  expect_equal(any(apply(model$output$feature_set==1, 1, which) == c(2,7,8,14,21,22,23,24,27,28)), TRUE)

  # train model (with distinct constraints)
  set.seed(1)
  const_new <- buildConstraints('cannot_link', constraint_vars = list(c(7,8,14)), 30, Inf)
  c <- build.UBayconstraint(A = rbind(c$A, const_new$A),
           b = c(c$b, const_new$b),
           rho = c(c$rho, const_new$rho))
  model <- setConstraints(model, c)
  model <- UBayFS::train(model)
  expect_equal(any(apply(model$output$feature_set==1, 1, which) == c(2,8,21,22,23,24,25,27,28,30)), TRUE)
})
