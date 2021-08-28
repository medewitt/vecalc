library(MatchIt)
library(data.table)
library(vecalc)
#https://www.robertkubinec.com/post/vaccinepval/
test_cohort <- vecalc::test_cohort

setDT(test_cohort)

convenience_sample <- sample(1:nrow(test_cohort[cohort_id==3]),2000)

convenience_sample <- test_cohort[cohort_id==3 & convenience_sample]

with(convenience_sample, table(vaccinated,infected))

matched_sample <- matchit(vaccinated~age + race + sex,
													data = convenience_sample,method = "cem")

par(col.main = c("firebrick"))
plot(summary(matched_sample))
title("Matched Results", adj = 0)

matched_data <- match.data(matched_sample)


(model_dat <- matched_data[,.(infected = sum(infected*weights),
															total = sum(weights)), by = "vaccinated"])


library(cmdstanr)
model_txt <- readLines(file.path(getwd(),"inst", "stan", "ve.stan"))
tmp <- tempfile(fileext = ".stan")
writeLines(model_txt, tmp)
mod <- cmdstan_model(tmp)
setDT(matched_data)

dat <- list(
	r_c = round(model_dat[vaccinated==0]$infected),
	r_t = round(model_dat[vaccinated==1]$infected),
	n_c = round(model_dat[vaccinated==0]$total),
	n_t = round(model_dat[vaccinated==1]$total),
	a = c(.7,1)
)

fit <- mod$sample(dat)

fit$summary("VE")

d <- prepare_ve_data(form = vaccinated~age + race + sex,
								data = convenience_sample)

fit_ve(d)
