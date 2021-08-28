`%||%` <- function(a, b) {
	if (is.null(a)) b else a
}

this_pkg <- function() {
	"vecalc"
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
