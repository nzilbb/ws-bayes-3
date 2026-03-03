library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(priorsense)

# Exercise: get comfortable plotting random samples from probabillity
# distributions

# e.g.:
hist(rnorm(100, mean=0, sd=1))
hist(rnorm(100, mean=4, sd=30))
hist(rbinom(100, size=10, prob=0.5))

# Look up some probability distributions on wikipedia to see what
# their parameters are, and play around with some.

# Student t distributions have heavier tails than normal distributions.
# What happens if you increase or decrease the 'df' value here?
hist(rstudent_t(n=100, df=20, mu=0, sigma=2))

# Exercise: fit the following model. 

qb2 <- read_rds(here('data', 'session_1.rds'))

trap_sub <- qb2 |> 
  filter(
    school %in% c(
      "Avonside Girls' High School", 
      "St Margaret's College"
    )
  ) |> 
  select(
    age, school, F1_lob2, part_id
  )

trap_prior <- c(
  prior(student_t(3, 0, 2), class = "Intercept"),
  prior(normal(0, 2), class = "sigma"),
  prior(normal(0, 0.5), class = "b")
)

trap_fit_1 <- brm(
  F1_lob2 ~ school,  
  data = trap_sub,
  prior = trap_prior
)

# Use the `prior_summary()` function to extract the prior.
prior_summary(trap_fit_1)

# Note that the first row is a generic row for all coefficients. In 
# this model, there's only one actual coefficient.

# Exercise: note differences from default prior.
default_prior(trap_fit_1)

# Exercise: change the prior above and fit a new model. 
# note, sigma controls the variance expected from the model residuals.
# One option: try a very strong prior by tightening it.

# Exercise: go back to last weeks model and try to determine what the
# priors are. 
# On my computer, I use this code (which also does a sensitivity analysis)
# choir_mod <- read_rds(here('models', 'KG_front_f1_mod.rds'))
# prior_summary(choir_mod)
# choir_vars <- get_variables(choir_mod)
# choir_fixed <- choir_vars[str_detect(choir_vars, "^b_vowelTRAP:director")]
# test_sensitivity <- powerscale_sensitivity(choir_mod, variables = choir_fixed)
# View(test_sensitivity)

# Exercise: do a prior predictive check using the trap model you fit
# above. Hint: use `sample_prior = "only"` with `brm()`

# Exercise: draw from the prior predictive using something like:
prior_draws <- your_model_name_goes_here |>
  predicted_draws(newdata = trap_sub, ndraws = 100)

# Exercise: plot the draws however you see fit.
## An overall plot: the code for the plot in the slides is:
# prior_draws |> 
#   ggplot(
#     aes(
#       x = school,
#       y = .prediction
#     )
#   ) +
#   geom_boxplot()

## Plot iterations separately (allows you do see difference between
# schools in individual cases)), e.g.
# pick out random sample of 8 draws.
# e.g.:
# to_plot <- sample(1:100, size=9)
# prior_draws |> 
#   filter(
#     .draw %in% to_plot
#   ) |> 
#   ggplot(
#     aes(
#       x = school,
#       y = .prediction
#     )
#   ) +
#   geom_boxplot() +
#   facet_wrap(
#     vars(.draw)
#   )

