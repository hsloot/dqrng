context("C++")

Rcpp::sourceCpp("cpp/default.cpp")

seed <- 1234567890

test_that("consecutive calls yield different random numbers", {
  expect_false(consecutive_calls(seed))
})

test_that("setting seed produces identical uniformly distributed numbers", {
  expect_true(seed_uniform(seed))
})

test_that("setting seed produces identical uniformly distributed scalar numbers", {
  expect_true(seed_uniform_scalar(seed))
})

test_that("Min and max can be equal for scalar numbers", {
  expect_true(seed_uniform_scalar_min_eq_max(seed))
})

test_that("Min must not be larger than max", {
  expect_error(seed_uniform_scalar_min_gt_max(seed),
               "'min' must not be larger than 'max'!")
})

test_that("setting seed produces identical normaly distributed numbers", {
  expect_true(seed_normal(seed))
})

test_that("setting seed produces identical normaly distributed scalar numbers", {
  expect_true(seed_normal_scalar(seed))
})

test_that("setting seed produces identical exponenetially distributed numbers", {
  expect_true(seed_exponential(seed))
})

test_that("setting seed produces identical exponenetially distributed scalar numbers", {
  expect_true(seed_exponential_scalar(seed))
})

Rcpp::sourceCpp("cpp/xoshiro-jump.cpp")

test_that("jump() for xoroshiro128+ works", {
  expect_true(xoroshiro_jump())
})

test_that("jump() for xoshiro256+ works", {
  expect_true(xoshiro_jump())
})

Rcpp::sourceCpp("cpp/global_rng.cpp")

test_that("global RNG is accessible and works as expected", {
  n <- 1e2L
  rate <- 0.4
  use_seed <- 1623

  dqset.seed(use_seed)
  expected <- dqrexp(n, rate)
  dqset.seed(use_seed)
  actual <- test_dqrexp(n, rate)
  expect_equal(actual, expected)

  dqset.seed(use_seed)
  expected2 <- expected
  actual2 <- sapply(1:n, function(x) test_dqrexp(1, rate))
  expect_equal(actual2, expected2)

  cl <- parallel::makeCluster(2)
  expected3 <- parallel::clusterApply(cl, 1:8, function(stream, seed, N, rate) {
    dqrng::dqRNGkind("Threefry")
    dqrng::dqset.seed(seed, stream)
    dqrng::dqrexp(N, rate)
  }, use_seed, 1e6L, rate)
  parallel::stopCluster(cl)
  cl <- parallel::makeCluster(2)
  actual3 <- parallel::clusterApply(cl, 1:8, function(stream, seed, N, rate) {
    Rcpp::sourceCpp("cpp/global_rng.cpp") ## must be recompiled
    dqrng::dqRNGkind("Threefry")
    dqrng::dqset.seed(seed, stream)
    test_dqrexp(N, rate)
  }, use_seed, 1e6L, rate)
  parallel::stopCluster(cl)
  expect_equal(actual3, expected3)
})
