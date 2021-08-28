## code to prepare `test_cohort` dataset goes here
## Generate a Cohort that can be used for testing vaccine efficiency
##

# Some Demographics
cohorts <- 5

n <- 10*10000

sex <- sample(x = c("M", "F"), size = n, replace = TRUE)

race <- sample(x = c("Asian","Black", "Latinx","White"),size = n,
							 prob = c(.06,.34,.20, .4), replace = TRUE)

age <- sample(12:100, size = n, replace = TRUE)

vaccine_propensity <rnorm(n, --1.2+age * .02  - as.numeric(factor(race))*.01 + as.numeric(as.factor(sex))*.01)

vaccinated <- ifelse(arm::invlogit(vaccine_propensity)>.5,1,0)

table(vaccinated,race)

infected_propensity <- rnorm(n, -.3+vaccinated*-.65 + age*.01 + as.numeric(factor(race))*.01 + as.numeric(as.factor(sex))*.01,0)

infected <- ifelse(arm::invlogit(infected_propensity)>.5,1,0)

(results <- table(vaccinated,infected))

1-(results[2,2]/sum(results[2,]))/(results[1,2]/sum(results[1,]))

test_cohort <- data.frame(age = age, sex = sex, race = race,
													vaccinated = vaccinated, infected = infected,
													stringsAsFactors = FALSE)
test_cohort$sex <- with(test_cohort, factor(sex, c("M", "F")))
test_cohort$race <- with(test_cohort, factor(race, c("White", "Asian", "Black", "Latinx")))
test_cohort$cohort_id <- rep(1:cohorts, each= 10000)

usethis::use_data(test_cohort, overwrite = TRUE)
