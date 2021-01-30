testthat("wrong class in train.UBaymodel",{
  expect_error(train_model(list(data = cbind(c(1,2),c(2,1)),
                                label = c(0,1),
                                user.param = list(),
                                ensemble.params = list(),
                                optim.params = list())))
})
