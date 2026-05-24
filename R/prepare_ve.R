#' Prepare Data for Matching
#'
#' This function returns objects that be used for calculating
#' Vaccine Efficacy
#'
#' @param form the formula to be used in the matching algorithm
#' @param data a data.frame with the data objects to be used
#' @param method a string indicating the matching algorithm to be
#'     used.
#' @param a a length-2 numeric vector of beta shape parameters for the
#'     prior on the relative-risk scale of vaccine efficacy. Defaults to
#'     `c(0.7, 1)`, a weakly informative prior that mildly favors an
#'     effective vaccine.
#' @export

prepare_ve_data <- function(form, data, method = "cem", a = c(0.7, 1)){

	form <- as.formula(form)

	assertthat::assert_that(is.numeric(a), length(a) == 2, all(a > 0))

	data.table::setDT(data)

	matched_sample <- MatchIt::matchit(form,
														data = data, method = method)

	matched_data <- MatchIt::match.data(matched_sample)
	data.table::setDT(matched_data)

	control   <- matched_data[vaccinated == 0]
	treatment <- matched_data[vaccinated == 1]

	c_arm <- weighted_binom(control$infected, control$weights)
	t_arm <- weighted_binom(treatment$infected, treatment$weights)

	model_dat <- list(
		r_c = c_arm$r,
		r_t = t_arm$r,
		n_c = c_arm$n,
		n_t = t_arm$n,
		a = a
	)


	o <- list(model_dat = model_dat,
						matched_dat = matched_sample)

	class(o) <- append("vedata", class(o))
	return(o)


}
