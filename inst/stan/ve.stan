data {
  int<lower=1> n_c;          // trials (subjects), control / unvaccinated
  int<lower=1> n_t;          // trials (subjects), treatment / vaccinated
  int<lower=0, upper=n_c> r_c; // events (infections), control
  int<lower=0, upper=n_t> r_t; // events (infections), treatment
  array[2] real<lower=0> a;  // beta shape parameters for the VE prior
}
parameters {
  real<lower=0, upper=1> p_c; // infection probability, control
  real<lower=0, upper=1> p_t; // infection probability, treatment
}
transformed parameters {
  real VE = 1 - p_t / p_c;    // vaccine efficacy: 1 - relative risk
}
model {
  // Prior on the relative-risk scale: (VE-1)/(VE-2) == RR/(RR+1) in (0, 1).
  // Applied as a soft prior on this combination of p_t and p_c (no Jacobian
  // adjustment, so the implied marginal on the transform is approximate).
  (VE - 1) / (VE - 2) ~ beta(a[1], a[2]);
  r_c ~ binomial(n_c, p_c); // likelihood, control
  r_t ~ binomial(n_t, p_t); // likelihood, treatment
}
generated quantities {
  real effect   = p_t - p_c;                                  // risk difference
  real log_odds = log(p_t / (1 - p_t)) - log(p_c / (1 - p_c)); // log odds ratio
}
