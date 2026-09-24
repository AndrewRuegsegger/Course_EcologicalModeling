################ 
# Model selection: Bayesian approaches

# Purpose: Learn Bayesian approaches to model selection. 

# List of packages necessary to run this script:
require(librarian, 
        quietly = TRUE)
shelf(tidyverse, 
      brms,
      MuMIn, # For AICc and model selection table
      cmdstanr,
      lib = tempdir(),
      quiet = TRUE)

# Scale helper function
scale2 <- 
  function(X) {
    
    (X - mean(X)) / sd(X)
    
  }


# Load the data
pineRidge <- 
  read_csv("D:/Ecological Modeling/Course_EcologicalModeling/data/PineRidge_Trees_cleaned.csv") %>%
  # Forcing severity and YSF to be factors and ordering them for convenience.
  mutate(BurnSeverity = factor(BurnSeverity,
                               levels = c("U", "L", "M", "H")),
         YearsSinceFire = factor(YearsSinceFire,
                                 levels = c("10", "27")),
         # And scaling the two continuous variables
         CWD_z = scale2(CWD_volume_sum),
         NTree_z = scale2(N_trees)) %>%
  # Remove grasslands... because they will have low mean DBH regardless.
  filter(CoverType != "G") %>%
  # Remove unnecessary columns
  select(-CoverType, -burn, -Severity_Num)

### NOTES: 

# # plot.code: unique identifier for a sampling location. These are the sampling
# units.

# # burn: burn = "D" is the Dawes Fire that burnt 10 years prior to data
# collection. burn = "FR" is the Fort Robinson Fire that burnt 27 years prior
# to data collection.

# # severity: first letter = cover type (forest = F, grassland = G); second letter
# = burn severity (U = unburned, L = low severity, M = moderate severity, H =
# high severity, B = burned grassland)

# Check out the pairs plots
pairs(pineRidge[ , 2:ncol(pineRidge)],
      lower.panel = NULL)

# Reminder: Don't put correlated variables in the same model. Pairs plots don't
# show any clear correlations between predictors, so we'll call it good for this
# exercise.

#=============================================================================
## Model selection - frequentist fits

# Model list
mods <- 
  list(Global = DBH_mean ~ BurnSeverity * YearsSinceFire + CWD_z + NTree_z,
       AllFire = DBH_mean ~ BurnSeverity * YearsSinceFire,
       Biotic = DBH_mean ~ CWD_z + NTree_z,
       Severity = DBH_mean ~ BurnSeverity,
       YSF = DBH_mean ~ YearsSinceFire,
       Null = DBH_mean ~ 1)

#### Challenge #1: 

# 1. Fit all models in `mods` with a "single" function. Fit models with `lm()`,
# and save as an object named `fits_lm`.

fits_lm <- 
  lapply(mods, lm, data = pineRidge)
names(fits_lm) <- names(mods)

# (Yes, we should probably be using another distribution, but there's a problem
# with doing this. Can you spot it? Regardless, let's just do it the easy way
# for now.)

## There are true zeroes in the data
range(pineRidge$DBH_mean)

# 2. Critical step: check the fit for the "Global" model. Since this is the most
# complex model, if it's a reasonable fit, the others *should* be too.

par(mfrow = c(2,2))
plot(fits_lm$Global)
summary(fits_lm$Global)

# 3. Make an AICc model ranking table with the fit models

MuMIn::model.sel(fits_lm)

# 4. Interpret the model ranking table. 

#=============================================================================
## Bayesian fits

#### Challenge #2:

# 1. Using the same data, `mods` list, and Gaussian(identity link) distribution
# as in the frequentist fits, run Bayesian models. Set iter = 600 and backend =
# "cmdstanr". Save all the fits in an object named `fits_brms`.
fits_brms <-
  lapply(mods,
         function(X) {
           brm(X,
               family = gaussian,
               iter = 600,
               chains = 3,
               cores = 6,
               data = pineRidge,
               backend = "cmdstanr")
         })

# 2. Now that you have all the models run, you still need to check the
# most complex model's diagnostics. How to do that with Bayesian models?

pp_check(fits_brms$Global, ndraws = 100)
# underfitting - not predicting zeroes well 
fits_brms$Global
# R-hats all converged (1.00)
plot(fits_brms$Global)
# Trace plots: visual representation of Rhats
# with model selection always check diagnostics, at least of global

#=============================================================================
## Model selection - Bayesian

# There are several information criteria you could use for Bayesian model
# selection, but the two dominate ones are the "Widely Applicable Information
# Criterion" (WAIC) and "Leave-One-Out Cross Validation" (LOOCV).

### If you really want to read more about these information criteria:
# Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
# evaluation using leave-one-out cross-validation and WAIC. Statistics and
# computing, 27(5), 1413-1432.

# Per the Veharti et al. (2017) article (cited >7,000 times!!), LOOCV is
# prefered because "Although WAIC is asymptotically equal to LOO, we demonstrate
# that PSIS-LOO is more robust in the finite case with weak priors or
# influential observations." But people use both (for now), so don't fret too
# much.

### IMPORTANTLY: like everything else in Bayes-land, these information criteria
### are generated as *distributions*, and you can get credible intervals on
### them. That also means they can take a long time to compute--especially for
### model runs with many iterations!


#### Challenge #3:

# 1. Run WAIC or LOOCV on all the models in `fits_brms`. Try do run a different
# criteria from your neighbor so you can compare. Any warnings or errors?

loos <- 
  lapply(fits_brms,
         brms::waic)

allfire <- brms::loo(fits_brms$AllFire)
nullmod <- brms::loo(fits_brms$Null)
severity <- brms::loo(fits_brms$Severity)

# 2a. Make a nice model ranking table (top-ranked model as first row, then
# descending). Make columns for the model name, elpd, and information criterion.



# 2b. Try using the "loo_compare()" function to do this--this function will work
# for LOOCV and WAIC. How is this different from the above table?

loo_compare(list(AllFire = allfire,
                 Severity = severity,
                 Null = nullmod))

# if elpd_diff < 6, then the model is "significantly" better

# #############################################################################
# ## Code to generate data. Keep commented.
# 
# 
# # 
# trees_selected <- 
#   trees_raw %>%
#   select(plot.code:severity, species, decay.class, dbh.cm)
# 
# #
# trees_selected <-
#   trees_selected %>%
#   #
#   mutate(YearsSinceFire = case_when(burn == "D" ~ "10",
#                                     burn == "FR" ~ "27",
#                                     .default = ">27"),
#          #
#          CoverType = str_sub(severity, 1, 1),
#          #
#          BurnSeverity = str_sub(severity, 2, 2))
# 
# 
# # 
# trees_summarized <- 
#   trees_selected %>%
#   na.omit() %>%
#   # 
#   filter(decay.class == "L") %>%
#   group_by(plot.code, burn, CoverType, BurnSeverity, YearsSinceFire) %>%
#   #
#   summarize(DBH_mean = mean(dbh.cm, na.rm = TRUE),
#             N_trees = n()) %>%
#   # 
#   ungroup()
# 
# cwd_summarized <- 
#   cwd_raw %>%
#   mutate(YearsSinceFire = case_when(burn == "D" ~ "10",
#                                     burn == "FR" ~ "27",
#                                     .default = ">27"),
#          #
#          CoverType = str_sub(severity, 1, 1),
#          #
#          BurnSeverity = str_sub(severity, 2, 2)) %>%
#   select(plot.code, 
#          CoverType, BurnSeverity, YearsSinceFire, burn,
#          line.cover.cm, diameter1.width.cm, diameter2.height.cm) %>%
#   mutate(CWD_volume = line.cover.cm * diameter1.width.cm * diameter2.height.cm) %>%
#   group_by(plot.code, CoverType, BurnSeverity, YearsSinceFire, burn) %>%
#   summarize(CWD_volume_sum = sum(CWD_volume, na.rm = TRUE))
# 
# pineRidge <- 
#   left_join(cwd_summarized,
#             trees_summarized) %>%
#   filter(BurnSeverity != "B") %>%
#   mutate(CWD_volume_sum = ifelse(is.na(CWD_volume_sum), 0, CWD_volume_sum),
#          DBH_mean = ifelse(is.na(DBH_mean), 0, DBH_mean),
#          N_trees = ifelse(is.na(N_trees), 0, N_trees),
#          Severity_Num = as.numeric(factor(BurnSeverity,
#                                           levels = c("U", "L", "M", "H"))))
# 
# write_csv(pineRidge,
#           "~/GitHub/Course_EcologicalModeling/data/PineRidge_Trees_cleaned.csv") 