test_that("fit_ve rejects input that is not a vedata object", {
  expect_error(fit_ve(list()))
  expect_error(fit_ve(data.frame(x = 1)))
})

test_that("fit_ve estimates VE on prepared data", {
  skip_if_not_installed("cmdstanr")
  skip_if_not(
    tryCatch(!is.null(cmdstanr::cmdstan_version()), error = function(e) FALSE),
    "CmdStan not installed"
  )

  d <- prepare_ve_data(vaccinated ~ age + race + sex,
                       data = as.data.frame(test_cohort)[1:1500, ])

  out <- fit_ve(d, stan_opts = list(
    iter_warmup = 200, iter_sampling = 200,
    parallel_chains = 1, refresh = 0
  ))

  expect_named(out, "sumz")
  expect_true(all(c("VE", "effect") %in% out$sumz$variable))
})
