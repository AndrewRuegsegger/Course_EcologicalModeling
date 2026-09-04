# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, # for tidying data
      mgcv, # for qq.gam
      performance,
      arm,
      quiet = TRUE,
      lib = tempdir())

# Load data:
bleach <- 
  read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/master/data/bleach.csv")

bleach

### Question 1 - fit lm and glm
## Linear model w/ LOGBLEACH as response
bleach_sst <- lm(LOGBLEACH ~ SST,
                 data = bleach)

## Generalized linear model w/ MASS as response, using a binomial distribution
## with the logit link function because MASS is a binary response
mass_sst <- glm(MASS ~ SST,
                family = binomial(link = logit),
                data = bleach)

### Question 2 - assess model assumptions
## Collinearity should not be an issue since these are single-predictor models

## Linear relationship b/t response and predictor
# LOGBLEACH model has an SST estimate of 1.318 and a p-value < 0.001,
# indicating there is a a linear relationship
ggplot(bleach,
       aes(SST, LOGBLEACH)) +
  geom_point() +
  geom_smooth(method = "lm")
summary(bleach_sst)
# MASS model has an SST estimate of 8.612 and a p-value = 0.05,
# indicating there is likely a linear relationship
ggplot(bleach,
       aes(x = MASS, y = SST, group = MASS)) +
  geom_boxplot()
summary(mass_sst)

## Homoscedasticity and normally distributed errors
# Residuals v. fitted plot does not show funnel shape = homoscedastic
# QQ-plot shows the tails falling away from the line = data is not super
# normally distributed, but it might not be a problem.
plot(bleach_sst)
# Points mostly stick close to the fitted line, but they do deviate a lot 
# toward the higher quantiles.
qq.gam(mass_sst, pch = 1)

### Question 3 - summarize models and diagnostics
# For every 1 increase in SST, LOGBLEACH is predicted to increase by 1.318,
# or an exp(1.31841) change in the original scale. SST explains ~84% of the
# variance in LOGBLEACH. Heteroscedasticity does not seem to be a problem
# and the data is more-or-less normally distributed.
summary(bleach_sst)

# For every 1 increase in SST, the odds of mass bleaching are multiplied by 
# exp(8.612), though this effect is marginally significant (p = 0.05).  
summary(mass_sst)

### Question 4 - back-transform and other log stuff
## Back-transform
bleach <- bleach %>%
  mutate(BLEACH = 10^LOGBLEACH)
# Pivoting long for scatterplots and histograms
bleach_long <- bleach %>%
  pivot_longer(cols = c(BLEACH, LOGBLEACH),
               names_to = "variables",
               values_to = "values")
bleach_longer <- bleach %>%
  pivot_longer(cols = c(SST, BLEACH, LOGBLEACH),
               names_to = "variables",
               values_to = "values")

## Scatterplot of SST and back-transformed LOGBLEACH
ggplot(bleach_long,
       aes(SST, values, color = variables)) +
  geom_point()
## Histogram of SST and back-transformed LOGBLEACH
ggplot(bleach_longer,
       aes(values, fill = variables)) +
  geom_histogram() +
  facet_wrap(~ variables, scales = "free")
## Does a log-transformation make ecological sense?

### Question 5 - What range of values predicts 50% of bleaching?
# At what range of values does the linear regression model (still using the LOGBLEACH response variable) predict 50% bleaching will occur? Provide an estimate of uncertainty for this prediction (Hint: use the predict.lm() function with options newdata and interval=”prediction”. See help(predict.lm) ).
nd <- 
  with(bleach, 
       expand.grid(
         SST = seq(min(SST),
                   max(SST),
                   length = 100)
       ))

pred <- predict.lm(bleach_sst, nd, interval = "prediction")
nd <- cbind(nd, pred)

nd <- nd %>%
  mutate(fit50 = nd$fit >= 0.5,
         lwr50 = nd$lwr >= 0.5,
         upr50 = nd$upr >= 0.5)
sst_at_fit <- nd$SST[which.min(abs(nd$fit - 0.5))]
sst_at_lwr <- nd$SST[which.min(abs(nd$lwr - 0.5))]
sst_at_upr <- nd$SST[which.min(abs(nd$upr - 0.5))]

c(lower_bound = sst_at_upr, point_estimate = sst_at_fit, upper_bound = sst_at_lwr)


nd$fit <- pred$fit
nd$se <- pred$se.fit
nd$lower <- nd$fit - 1.96 * nd$se
nd$upper <- nd$fit + 1.96 * nd$se

nd$depth_level <- factor(nd$sc_depth,
                         levels = sort(unique(nd$sc_depth)),
                         labels = c("Min", "Median", "Max"))

ggplot(nd,
       aes(x = sc_length, y = fit, 
           color = depth_level, fill = depth_level)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(x = "Scaled bill length", y = "Predicted body mass (g)",
       color = "Bill depth", fill = "Bill depth")




