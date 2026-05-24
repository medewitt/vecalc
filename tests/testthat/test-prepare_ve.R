make_sample <- function(n = 1500) {
  d <- as.data.frame(test_cohort)
  d[seq_len(min(n, nrow(d))), ]
}

test_that("prepare_ve_data returns a vedata object", {
  out <- prepare_ve_data(vaccinated ~ age + race + sex, data = make_sample())

  expect_s3_class(out, "vedata")
  expect_named(out, c("model_dat", "matched_dat"))
})

test_that("model_dat carries the Stan inputs in the expected shape", {
  out <- prepare_ve_data(vaccinated ~ age + race + sex, data = make_sample())
  md <- out$model_dat

  expect_named(md, c("r_c", "r_t", "n_c", "n_t", "a"))

  # counts are single non-negative whole numbers
  for (nm in c("r_c", "r_t", "n_c", "n_t")) {
    expect_length(md[[nm]], 1)
    expect_gte(md[[nm]], 0)
    expect_equal(md[[nm]], round(md[[nm]]))
  }

  # events cannot exceed the number at risk in either arm
  expect_lte(md$r_c, md$n_c)
  expect_lte(md$r_t, md$n_t)

  # prior hyperparameters are passed through unchanged
  expect_equal(md$a, c(0.7, 1))
})

test_that("a character formula is accepted", {
  out <- prepare_ve_data("vaccinated ~ age + race + sex", data = make_sample())
  expect_s3_class(out, "vedata")
})

test_that("the method argument is honored", {
  # method lives in $info$method (MatchIt >= 4) or $method (older)
  matchit_method <- function(x) x$info$method %||% x$method

  cem <- prepare_ve_data(vaccinated ~ age + race + sex,
                         data = make_sample(), method = "cem")
  nearest <- prepare_ve_data(vaccinated ~ age + race + sex,
                             data = make_sample(), method = "nearest")

  expect_s3_class(cem$matched_dat, "matchit")
  expect_equal(matchit_method(cem), "cem")
  expect_equal(matchit_method(nearest), "nearest")
})
