test_that("%||% returns the first non-NULL value", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_null(NULL %||% NULL)
  expect_equal("a" %||% "b", "a")
})

test_that("this_pkg reports the package name", {
  expect_equal(this_pkg(), "vecalc")
})

test_that("weighted_binom reduces to raw counts with equal weights", {
  out <- weighted_binom(infected = c(1, 0, 1, 0), weights = rep(1, 4))
  expect_equal(out$n, 4)
  expect_equal(out$r, 2)
})

test_that("weighted_binom deflates the sample size when weights are unequal", {
  w <- c(2, 2, 1, 1)
  out <- weighted_binom(infected = c(1, 1, 0, 0), weights = w)

  # Kish effective sample size is below the summed weights, propagating the
  # extra uncertainty from unequal weighting
  expect_lt(out$n, sum(w))
  expect_equal(out$n, round(sum(w)^2 / sum(w^2)))
  expect_lte(out$r, out$n)
})
