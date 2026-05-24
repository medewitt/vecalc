`%||%` <- function(a, b) {
	if (is.null(a)) b else a
}

this_pkg <- function() {
	"vecalc"
}

# Summarise a matched arm for the binomial likelihood while accounting for the
# matching weights. Treating the summed weights as the binomial denominator
# overstates precision when weights are unequal, so the sample size is deflated
# to Kish's effective sample size, n_eff = (sum w)^2 / sum(w^2). The weighted
# infection proportion is then scaled to that effective size. When all weights
# are equal this reduces to the raw counts.
weighted_binom <- function(infected, weights) {
	w_sum <- sum(weights)
	n_eff <- w_sum^2 / sum(weights^2)
	p_hat <- sum(infected * weights) / w_sum
	list(r = round(p_hat * n_eff), n = round(n_eff))
}

cmd_stan_defaults <- list(
	seed = NULL,
	refresh = NULL,
	init = NULL,
	save_latent_dynamics = FALSE,
	output_dir = NULL,
	output_basename = NULL,
	sig_figs = NULL,
	chains = 4,
	parallel_chains = getOption("mc.cores", 1),
	threads_per_chain = NULL,
	opencl_ids = NULL,
	iter_warmup = NULL,
	iter_sampling = NULL,
	save_warmup = FALSE,
	thin = NULL,
	max_treedepth = NULL,
	adapt_engaged = TRUE,
	adapt_delta = NULL,
	step_size = NULL,
	metric = NULL,
	metric_file = NULL,
	inv_metric = NULL,
	init_buffer = NULL,
	term_buffer = NULL,
	window = NULL,
	fixed_param = FALSE,
	validate_csv = TRUE,
	show_messages = TRUE,
	cores = NULL,
	num_cores = NULL,
	num_chains = NULL,
	num_warmup = NULL,
	num_samples = NULL,
	save_extra_diagnostics = NULL,
	max_depth = NULL,
	stepsize = NULL
)
