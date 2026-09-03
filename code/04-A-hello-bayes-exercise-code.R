# # Clear environment?
# rm(list=ls())

# List of packages necessary to run this script:
require(librarian)
shelf(tidyverse, here, sf, brms, maps, terra, tidybayes,
      lib = tempdir())

# Set path
repo_url <- "https://raw.githubusercontent.com/LivingLandscapes/Course_EcologicalModeling/master/data/"

# Set seed
set.seed(2252)

# Load data
dat_list <- 
  list(RouteDictionary = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteDictionary.csv")),
       RouteUTMs = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteUTMs.csv")),
       RouteWhistleData = read_csv(paste0(repo_url, "QuailWhistle_Nebraska_RouteWhistleData.csv")))

#=============================================================================
## Tidying data

# Tidy whistle data
whistle <- 
  dat_list$RouteWhistleData %>%
  pivot_longer(cols = Cass:Lancaster_YH,
               names_to = "Route.Name",
               values_to = "WhistleCount") %>%
  mutate(Route.Name = case_when(Route.Name == "Cass" ~ "Cass North",
                                Route.Name == "GageN" ~ "Gage North",
                                Route.Name == "GageS" ~ "Gage South",
                                Route.Name == "Jeffersn" ~ "Jefferson",
                                Route.Name == "JohnsonN" ~ "Johnson North",
                                Route.Name == "JohnsonS" ~ "Johnson South",
                                Route.Name == "Lancaster.East" ~ "Lancaster East",
                                Route.Name == "Lancaster.North" ~ "Lancaster North",
                                Route.Name == "Lancaster.Branched.Oak" ~ "Lancaster-Branched Oak",
                                Route.Name == "OtoeN" ~ "Otoe North",
                                Route.Name == "OtoeS" ~ "Otoe South",
                                Route.Name == "Richardn" ~ "Richardson",
                                .default = Route.Name)) %>%
  left_join(dat_list$RouteDictionary %>% mutate(Route.Name = `Route Name`)) %>%
  mutate(Route.Number = `Route Number`) %>%
  filter(Route.Name != "Lancaster_YH") %>%
  left_join(dat_list$RouteUTMs %>%
              mutate(Route.Number = Route) %>%
              group_by(Route.Number) %>%
              filter(row_number() == 1)) %>%
  mutate(across(c(Year, UTMYE, UTMXN),
                function(X) (X - mean(X)) / sd(X),
                .names = "{.col}_scaled"))

#=============================================================================
## Frequentist vs. Bayesian

# Fit a linear regression with the frequentist method
fit_lm <-
  lm(WhistleCount ~ Year_scaled + UTMYE_scaled * UTMXN_scaled, 
     data = whistle)

# Fit the same model with Bayesian methods using the `brms` package!
fit_brm <-
  # Workhorse function: `brm`
  brm(
      # Formula argument--just like `lm` or `glm`!
      formula = WhistleCount  ~ Year_scaled + UTMYE_scaled * UTMXN_scaled,
      # Family argument is just like `glm` but with more options.
      family = gaussian,
      # Data is same too
      data = whistle,
      # This is new: it's the number of MCMC chains to run. 3 - 4 are
      # sufficient, but if you're just playing around and want speed, you can
      # run one.
      chains = 3,
      # Number of "warmup" iterations to get the MCMC chain started. Warmup
      # iterations are not used for posterior distributions. The default is
      # iter/2.
      warmup = 500, 
      # Number of iterations to run on each MCMC chain. This includes warmup.
      iter = 1000,
      # How many iterations to save. Can be useful to increase if iter is very
      # large to reduce memory and computation time.
      thin = 1,
      # Number of cores to use. Can speed up processing if you're using >1 chain.
      cores = 3)

# Check out the summary
fit_brm

### Challenge #1: 

# Compare the frequentist and Bayesian model outputs/summary tables,
# particularly the coefficient estimates and standard errors. Are there
# differences? If so, what are they?

#=============================================================================
## Model diagnostics

# Posterior predictive check. Ideally, the `yrep` lines should overlap the black
# `y` line. If there are systemmatic problems, the model is probably not a great
# fit. Do you see any issues?
pp_check(fit_brm)

# Look at traceplots for each parameter estimate
plot(fit_brm)

# Bayesian R-squared
bayes_R2(fit_brm)

#=============================================================================
## Predicting

# Predict whistle counts. Note how similar to frequentist syntax!
pred_df <- 
  cbind(whistle,
        predict(fit_brm, 
                whistle,
                probs = c(0.025, 0.975)))

# Plot predictions
ggplot(pred_df,
       aes(x = Year, 
           y = Estimate, 
           ymin = Q2.5, 
           ymax = Q97.5,
           color = Route.Name, 
           group = Route.Name,
           fill = Route.Name)) + 
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  geom_ribbon(alpha = 0.25, color = NA) + 
  geom_line(linewidth = 1.5) +
  theme_bw() + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14))

### What's the problem with these predictions?!

#=============================================================================
## brms has a ton of bells and ...whistles... 

# We can fix a key issue by truncating the normal distribution at zero!
fitTrunc0_brm <-
  brm(WhistleCount | trunc(lb = 0)  ~ Year_scaled + UTMYE_scaled * UTMXN_scaled,
      family = gaussian(),
      data = whistle,
      chains = 3,
      iter = 1000,
      threads = 3)

# Predict whistle counts with new zero truncated model
predTrunc0_df <- 
  cbind(whistle,
        predict(fitTrunc0_brm, 
                whistle,
                probs = c(0.025, 0.975)))

# Is the nonsense issue solved?
ggplot(predTrunc0_df,
       aes(x = Year, 
           y = Estimate, 
           ymin = Q2.5, 
           ymax = Q97.5,
           color = Route.Name, 
           group = Route.Name,
           fill = Route.Name)) + 
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  geom_ribbon(alpha = 0.25, color = NA) + 
  geom_line(linewidth = 1.5) +
  theme_bw() + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14))

### Challenge #2:

# Compare the posterior predictive checks between the simple brms model and the
# zero truncated model. Which is a better fit?

#=============================================================================
## Generalized linear models in brms

### Challenge #3:

# 1. There's a potential problem with the zero truncated model. Explore the data
# to figure out what it is.

# 2. "Extra hard" challenge: how can we solve this potential problem with
# frequentist OR Bayesian methods?
