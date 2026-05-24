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


	(model_dat <- matched_data[,list(infected = sum(infected*weights),
																total = sum(weights)), by = "vaccinated"])

	model_dat <- list(
		r_c = round(model_dat[vaccinated==0]$infected),
		r_t = round(model_dat[vaccinated==1]$infected),
		n_c = round(model_dat[vaccinated==0]$total),
		n_t = round(model_dat[vaccinated==1]$total),
		a = a
	)


	o <- list(model_dat = model_dat,
						matched_dat = matched_sample)

	class(o) <- append("vedata", class(o))
	return(o)


}
