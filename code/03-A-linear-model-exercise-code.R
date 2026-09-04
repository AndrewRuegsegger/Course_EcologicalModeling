# Code for Linear models lab
# Author: Dr. Roberts
# Date: Aug 28, 2026
# Description: Challenges for the linear model lab

### Citations:

# Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago
# (Antarctica) penguin data. R package version 0.1.0.
# https://allisonhorst.github.io/palmerpenguins/. doi: 10.5281/zenodo.3960218.

### Overall hypotheses:

# In penguins, males will have greater body mass than females, and body mass is
# also allometrically related to bill length and depth. We could also add that
# body mass will vary across species.

#=============================================================================
## Preparations

# If librarian package not installed, install it.
list.of.packages <- c("librarian")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load librarian ...
require(librarian, 
        quietly = TRUE)

# .. and then install packages
shelf(tidyverse, 
      cowplot, 
      palmerpenguins, 
      performance, 
      lib = tempdir(),
      quiet = TRUE)

#=============================================================================
## Load data

# Load cleaned penguins data:
data(package = 'palmerpenguins', verbose = FALSE)

#=============================================================================
## Familiarize yourself with the data

# Check out the data
head(penguins)

# Read through the data description for the penguins dataset
?penguins

#### Challenge 1: 

# 1. Create conditional histograms and boxplots of body mass by Species and
# Island.

# 2. Do a couple more data explorations relevant to our hypotheses.

#=============================================================================
## Fitting our first model

# To test our hypotheses, let's create a 'global' model containing all the
# variables that our hypotheses predict will influence body mass:
fit_global <- 
  lm(body_mass_g ~ sex + bill_length_mm * bill_depth_mm + species,
     data = penguins)

# Check out the global model's coefficients, R^2 values, and p-values.
summary(fit_global)

### Challenge 2:

# 1. Go line-by-line through the summary table and--in words--communicate if
# each hypothesis is supported and why.

# 2. Two things are "missing" from the model summary table. What are they? Why
# are they "missing"?

#=============================================================================
## Checking for multi-collinearity in our model

# Use performance::check_collinearity to calculate Variance Inflation Factors
# (VIFs):
check_collinearity(fit_global)

# Uh oh. Looks like we have >>> 10 VIFs for all predictor variables except sex!
# Are our species and allometric hypotheses scuppered?!
  
# Before we get too morose, let's try a few things to double-check these VIFs
# are giving us accurate information. First, it stands to reason that if we
# have interaction terms such as 'x', 'y' and 'xy,' then 'x' and 'y' will be
# correlated with their product 'xy,' right? One way to demonstrate this is by
# standardizing (i.e., centering or scaling) the offending numeric predictor
# variables and recalculating VIFs. This should reduce the collinearity at
# least for the interaction terms. Let's try it:

### Challenge 3a:

# 1. Scale (i.e., subtract mean and divide by standard deviation) all continuous
# predictor variables.

# 2. Rerun the model and re-check for collinearity.

# Okay, so far so good. The second thing to check is whether coefficient,
# p-values, and $R^2$ values have changed between the un-standardized global
# model and the standardized global model. 

### Challenge 3b:

# 1. Compare coefficient estimates between scaled and unscaled models.

# 2. Compare R^2 values between scaled and unscaled models.

# 3. If there are no issues with the scaled model, use it going forward and
# iteratively remove high VIF covariates until our max VIF < 10.

#=============================================================================
## Model diagnostics

# As a last step, we need to make sure our final model meets all required
# assumptions for a linear regression:
#  
# - Linear relationship between response and predictor variables
# - Reasonable levels of collinearity (e.g., VIF < 3 or 5 or 10)
# - Homogeneity of variances between groups (i.e., homoscedasticity 
# [i.e., NOT heteroscedasticity])
# - Errors are normally distributed
# - Independent sampling (i.e., no autocorrelation or pseudoreplication)

# We've already dealt with collinearity, we know there is a linear relationship
# between response and predictor variables. 

# We will wait until our Autocorrelation topic to check for independence. For
# now, let's check for homogeneity of variances and normal error distributions:

### Challenge #4: 

# Plot model diagnostics, checking the homogeneity of variance and normality
# assumptions.

#=============================================================================
## Visualizing model outputs: Dot plots for coefficient estimates

# We don't want stop at the model summary table: we need to visualize the model
# outputs. This is critical for publications, reports, science communication,
# and just helping us understand the model. We also need to be honest about the
# uncertainty (variation) in our estimates.

# First of all, let's make some dot-and-whisker plots to display our coefficient
# estimates and uncertainty (i.e., confidence intervals). We'll need to extract
# the coefficient estimates and calculate confidence intervals.

# There are multiple ways to extract coefficient estimates
coef(fit) # Just coefficient estimates
summary(fit)$coefficients # whole model table. It's a matrix, so you can index!
summary(fit)$coefficients[ , 1] # First column is just coefficient estimates.
summary(fit)$coefficients[ , 2] # Second column is standard error estimates.

##  For all models that use the normal (Gaussian) distribution and the "identity"
##  link function, confidence intervals are very easy to estimate:
# - Step 1: Find a t-score table and look for your desired level of confidence.
# - Step 2: Multiply the t-score by the Standard Error estimate.
# - Step 3: Subtract/Add the product from the coefficient estimate for the upper/
#           lower confidence limit.

## Here's a by-hand example for 95% confidence. NOTE: 95% confidence means the
## interval ranges from 2.5% - 97.5%. Why?
dotplot_df <- 
  data.frame(CI_0.975 = coef(fit) + ((summary(fit)$coefficients[, 2]) * 1.96),
             CI_0.025 = coef(fit) - ((summary(fit)$coefficients[, 2]) * 1.96),
             CoefEsts = coef(fit)
  )

# ... and now, here's a short-cut:
confint(fit)

### Challenge #5: 

# 1. Create a dotplot with the coefficient estimates as dots (geom_point) and
# confidence intervals as whiskers (geom_errorbar). Add a dashed straight line
# at zero.

# 2. Interpret the plot. What does it mean if the whiskers overlap zero?

#=============================================================================
## Visualizing model outputs: effect plots + interactions

# Dotplots are fantastic, but showing trend lines can be even more powerful.
# This can get a little tricky when there are multiple covariates--especially
# when there are interactions. It can also be hard to interpret an interactive
# effect just from dotplots. Let's create effects plots with the
# interactive effects.

### Here's the general workflow: 

# Step 1: Create a "new" data.frame with ranges of predictor variable values on
#         which to make predictions. NOTE: I strongly suggest you constrain the 
#         ranges to the range of the data to avoid the perils of extrapolation.
# Step 2: Use the model fit to predict response variable estimates and standard
#         errors at the predictor variable values in the new data.frame.
# Step 3: Calculate confidence intervals.
# Step 4: Plot the estimated responses + confidence intervals

# Let's start by using the handy "expand.grid" function to create the new
# data.frame. Where there are empty comments below, annotate with what the code
# is doing:
nd <- 
  #
  with(penguins,
       #
       expand.grid(
         # 
         sex = c("male", "female"),
         # 
         bill_length_mm = seq(min(bill_length_mm, na.rm = TRUE), 
                              max(bill_length_mm, na.rm = TRUE),
                              #
                              length.out = 50),
         #
         bill_depth_mm = c(min(bill_depth_mm, na.rm = TRUE), 
                           median(bill_depth_mm, na.rm = TRUE),
                           max(bill_depth_mm, na.rm = TRUE)))
  )

# But wait: we can't predict with this yet! Try this code:
predict(fit, nd, se.fit = TRUE)
# What does that error mean?? 

### Challenge #6:

# 1. Your task: get "predict" to work.

# 2. Once you've successfully predicted response + uncertainty, create effect
# plots! HINT: group/facet by sex, color/fill by bill depth, use geom_line for
# the fit, and use geom_ribbon for the confidence intervals.

