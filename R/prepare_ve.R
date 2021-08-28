#' Prepare Data for Matching
#'
#' This function returns objects that be used for calculating
#' Vaccine Efficiency
#'
#' @param form the formal to be used in the matching algorithm
#' @param data a data.frame with the data objects to be used
#' @param method a string indicating the matching algorithm to be
#'     used.
#' @export

prepare_ve_data <- function(form, data, method = "cem"){

	form <- as.formula(form)

	matched_sample <- matchit(form,
														data = data,method = "cem")

	matched_data <- match.data(matched_sample)


	(model_dat <- matched_data[,.(infected = sum(infected*weights),
																total = sum(weights)), by = "vaccinated"])

	model_dat <- list(
		r_c = round(model_dat[vaccinated==0]$infected),
		r_t = round(model_dat[vaccinated==1]$infected),
		n_c = round(model_dat[vaccinated==0]$total),
		n_t = round(model_dat[vaccinated==1]$total),
		a = c(.7,1)
	)


	o <- list(model_dat = model_dat,
						matched_dat = matched_sample)

	class(o) <- append("vedata", class(o))
	return(o)


}
