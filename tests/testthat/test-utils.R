test_that("%||% returns the first non-NULL value", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_null(NULL %||% NULL)
  expect_equal("a" %||% "b", "a")
})

test_that("this_pkg reports the package name", {
  expect_equal(this_pkg(), "vecalc")
})
