#' Fit Vaccine Efficiency
#'
#' A function for fitting vaccine efficiency
#'
#' @param ve_data an object of class vedata
#' @param stan_opts a list, any additional parameters to pass to
#'     Stan.
#' @importFrom stats as.formula weights
#' @export
#'
fit_ve <- function(ve_data, stan_opts = list()){

	assertthat::are_equal(class(ve_data),c("vedata","list"))

	dat <- ve_data[["model_dat"]]

	requireNamespace("cmdstanr")
	local_location <- rappdirs::user_cache_dir(appname = this_pkg())

	if (length(list.files(local_location, pattern = ".stan")) > 1) {
		cli::cli_alert_info("Using cached Stan models")
		cli::cli_alert_info(
			"Use `vecalc::clear_cache` if you need to refresh")
	} else {
		cli::cli_alert_info("Copying Stan models to cache")
		staninside::copy_models(this_pkg())
		cli::cli_alert_success("Models copied!")
	}


	model_file_path <- file.path(local_location, paste0("ve", ".stan"))
	mod <- cmdstanr::cmdstan_model(model_file_path)
	fit <- mod$sample(data = dat,
										parallel_chains = stan_opts$iter_sampling %||% 2,
										iter_sampling = stan_opts$iter_sampling %||% 1000,
										iter_warmup = stan_opts$iter_warmup %||% 1000,
										refresh = stan_opts$refresh %||% 250,
										init = stan_opts$refresh %||% NULL,
										seed  = stan_opts$seed %||% 336
	)

	out_contents <- list(sumz = fit$summary(c("VE", "effect")))

	return(out_contents)

}
