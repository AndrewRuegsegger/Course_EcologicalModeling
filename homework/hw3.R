# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, cowplot,
      AICcmodavg, # for aic.tab, model averaging, etc.
      mgcv, # for qq.gam
      quiet = TRUE,
      lib = tempdir())

# Load data:
mlrbird <- 
  read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/master/data/mlrbird.csv")
mlrland <- 
  read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/master/data/mlrland.csv")

mlrbird
mlrland

## Covariates: * = Westphal et al; ** local covariates
# Patch - patch code
# Name - Name of survey location
# *TLA - total landscape area
# NumP - number of patches
# MPS - mean patch size
# LPI - largest patch index (% of TLA covered by largest patch)
# *LSI - landscape shape index (measure of edge) = 1.0 if one circular patch, increases with edge
# PSSD - patch size std. Dev.
# *MNN - mean nearest neighbor distance (not if NumP is 0, it should be NA not 0 - check)
# TE - total edge
# *MPAR = (TE/TLA/NumP) = mean patch perimeter area ratio. Note that MPAR is a derived variable not found in the provided dataset.
# **MRF - mean annual rainfall
# **PA - patch area
# **PP - patch perimeter

# Landscape: TLA, NumP, MPS, LPI, LSI, PSSD, MNN, TE, MPAR
# Local: MRF, PA, PP
# Some covariates are likely correlated

## Twice as many rows in bird data than variables b/c surveys conducted
## over two years. 
# How to deal with this? Three options:
# 1) collapse years into one observation; if a sp was observed either year = 1.
# 2) perform separate analyses for each year.
# 3) use year as a random intercept ...
# I'm gonna go with option 1.

mlrbird <- aggregate(
  mlrbird[6:24],
  by = list(Name = mlrbird$Name, Patch = mlrbird$Patch),
  FUN = max
)

# Joining the bird and covariate data
bird_land <- mlrbird %>%
  left_join(mlrland)

# Creating richness column ... just cause.
bird_land <- bird_land %>%
  mutate(
    Richness = rowSums(select(., 3:21) > 0)
  )

# Creating MPAR
bird_land <- bird_land %>%
  mutate(
    MPAR2K = (TE2K / TLA2K / NumP2K),
    MPAR5K = (TE5K / TLA5K / NumP5K),
    MPAR10K = (TE10K / TLA10K / NumP10K)
  )

# Does MNN = NA (not 0) when NumP = 0?
any(bird_land$NumP2K == 0, na.rm = TRUE)
any(bird_land$NumP5K == 0, na.rm = TRUE)
any(bird_land$NumP10K == 0, na.rm = TRUE)
# No zeroes in NumP so... all good. 

### Question 1: Describe 7-10 hypotheses that compare patch vs. landscape 
### explanations for variation in patch use of woodland birds, and estimate
### the best scale for landscape effects.

# 1) Global landscape model (2K): all landscape covariates (after checking for collinearity).
# 2) Global landscape model (5K): all landscape covariates (after checking for collinearity).
# 3) Global landscape model (10K): all landscape covariates (after checking for collinearity).
# 4) Global patch model: all local covariates (after checking for collinearity).
# 5) Null model. 
# 6) Patch area & perimeter: Patch area and patch perimeter will have an effect on bird occurrence (PA, PP).
# 7) Number and size of patches (2K): The number and size of patches will affect bird occurrence (NumP, MPS, and LPI).
# 8) Number and size of patches (10K): The number and size of patches will affect bird occurrence (NumP, MPS, and LPI).
# 9) Ratio of edge to landscape area to the number of patches (2K): This ratio will affect bird occurrence (MPAR).
# 10) Ratio of edge to landscape area to the number of patches (10K): This ratio will affect bird occurrence (MPAR).

mods <- 
  list(
    global_2K_grcu = "grcu ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + TE2K + MPAR2K + PSSD2K",
    global_2K_silv = "silv ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + TE2K + MPAR2K + PSSD2K",
    global_5K_grcu = "grcu ~ TLA5K + NumP5K + MPS5K + LPI5K + LSI5K + MNN5K + TE5K + MPAR5K + PSSD5K",
    global_5K_silv = "silv ~ TLA5K + NumP5K + MPS5K + LPI5K + LSI5K + MNN5K + TE5K + MPAR5K + PSSD5K",
    global_10K_grcu = "grcu ~ TLA10K + NumP10K + MPS10K + LPI10K + LSI10K + MNN10K + TE10K + MPAR10K + PSSD10K",
    global_10K_silv = "silv ~ TLA10K + NumP10K + MPS10K + LPI10K + LSI10K + MNN10K + TE10K + MPAR10K+ PSSD10K",
    global_patch_grcu = "grcu ~ MRF + PA + PP",
    global_patch_silv = "silv ~ MRF + PA + PP",
    null_grcu = "grcu ~ 1",
    null_silv = "silv ~ 1",
    area_perim_grcu = "grcu ~ PA + PP",
    area_perim_silv = "silv ~ PA + PP",
    num_size_grcu_2K = "grcu ~ NumP2K + MPS2K + LPI2K",
    num_size_silv_2K = "silv ~ NumP2K + MPS2K + LPI2K",
    num_size_grcu_10K = "grcu ~ NumP10K + MPS10K + LPI10K",
    num_size_silv_10K = "silv ~ NumP10K + MPS10K + LPI10K",
    mpar_grcu_2K = "grcu ~ MPAR2K",
    mpar_silv_2K = "silv ~ MPAR2K",
    mpar_grcu_10K = "grcu ~ MPAR10K",
    mpar_silv_10K = "silv ~ MPAR10K"
  )

fits <- 
  lapply(mods,
    glm, family = binomial(link = "logit"), data = bird_land
  )

### Checking for collinearity
## If I remove a predictor from a model at any scale, I'll remove at all scales
## so that the global models of different scales are comparable to one another.
## 2K global model
check_collinearity(fits$global_2K_grcu)
check_collinearity(fits$global_2K_silv)
# removing TE2K
mods[["global_2K_grcu"]] <- "grcu ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K + PSSD2K"
mods[["global_2K_silv"]] <- "silv ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K + PSSD2K"
# removing PSSD2K
mods[["global_2K_grcu"]] <- "grcu ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
mods[["global_2K_silv"]] <- "silv ~ TLA2K + NumP2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
# And NumP2K
mods[["global_2K_grcu"]] <- "grcu ~ TLA2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
mods[["global_2K_silv"]] <- "silv ~ TLA2K + MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
# And TLA2K
mods[["global_2K_grcu"]] <- "grcu ~ MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
mods[["global_2K_silv"]] <- "silv ~ MPS2K + LPI2K + LSI2K + MNN2K + MPAR2K"
# VIF looks good now.

## 5K global model
check_collinearity(fits$global_5K_grcu)
check_collinearity(fits$global_5K_silv)
# removing TE5K, PSSD5K, NumP5K, and TLA5K to keep models consistent so that scales
# can be compared
mods[["global_5K_grcu"]] <- "grcu ~ MPS5K + LPI5K + LSI5K + MNN5K + MPAR5K"
mods[["global_5K_silv"]] <- "silv ~ MPS5K + LPI5K + LSI5K + MNN5K + MPAR5K"
# removing MPAR5K
mods[["global_5K_grcu"]] <- "grcu ~ MPS5K + LPI5K + LSI5K + MNN5K"
mods[["global_5K_silv"]] <- "silv ~ MPS5K + LPI5K + LSI5K + MNN5K"
# and from 2K models
mods[["global_2K_grcu"]] <- "grcu ~ MPS2K + LPI2K + LSI2K + MNN2K"
mods[["global_2K_silv"]] <- "silv ~ MPS2K + LPI2K + LSI2K + MNN2K"

## 10K global model
check_collinearity(fits$global_10K_grcu)
check_collinearity(fits$global_10K_silv)
# removing TE10K, PSSD10K, NumP10K, TLA10K, and MPAR10K to keep models consistent so that scales
# can be compared
mods[["global_10K_grcu"]] <- "grcu ~ MPS10K + LPI10K + LSI10K + MNN10K"
mods[["global_10K_silv"]] <- "silv ~ MPS10K + LPI10K + LSI10K + MNN10K"

fits <- 
  lapply(mods,
         glm, family = binomial(link = "logit"), data = bird_land
  )

## Checking collinearity of other models
# These all look good
check_collinearity(fits$num_size_silv_2K)
check_collinearity(fits$num_size_grcu_2K)
check_collinearity(fits$num_size_silv_10K)
check_collinearity(fits$num_size_grcu_10K)

# PA and PP are highly correlated — makes sense.
check_collinearity(fits$global_patch_silv)
check_collinearity(fits$global_patch_grcu)
check_collinearity(fits$area_perim_silv)
check_collinearity(fits$area_perim_grcu)
# A check confirms they are nearly perfectly correlated.
pred_vars <- c("PP", "PA")
cor(bird_land[pred_vars])
# Since I have landscape models about the size of patches,
# and we want to compare patch-specific vs. landscape effects, I'll keep patch
# area as its own model and remove it from the "global" patch model, which is
# now asking whether patch perimeter and mean annual rainfall are important.

### Question 2: Fitting selected models for Grey Currawong and Silvereye
## Final model set
mods_grcu <- 
  list(
    global_2K_grcu = "grcu ~ MPS2K + LPI2K + LSI2K + MNN2K",
    global_5K_grcu = "grcu ~ MPS5K + LPI5K + LSI5K + MNN5K",
    global_10K_grcu = "grcu ~ MPS10K + LPI10K + LSI10K + MNN10K",
    null_grcu = "grcu ~ 1",
    patch_area_grcu = "grcu ~ PA",
    patch_rain_perim_grcu = "grcu ~ MRF + PP",
    num_size_grcu_2K = "grcu ~ NumP2K + MPS2K + LPI2K",
    num_size_grcu_10K = "grcu ~ NumP10K + MPS10K + LPI10K",
    mpar_grcu_2K = "grcu ~ MPAR2K",
    mpar_grcu_10K = "grcu ~ MPAR10K"
  )
mods_silv <- 
  list(
    global_2K_silv = "silv ~ MPS2K + LPI2K + LSI2K + MNN2K",
    global_5K_silv = "silv ~ MPS5K + LPI5K + LSI5K + MNN5K",
    global_10K_silv = "silv ~ MPS10K + LPI10K + LSI10K + MNN10K",
    null_silv = "silv ~ 1",
    patch_area_silv = "silv ~ PA",
    patch_rain_perim_silv = "silv ~ MRF + PP",
    num_size_silv_2K = "silv ~ NumP2K + MPS2K + LPI2K",
    num_size_silv_10K = "silv ~ NumP10K + MPS10K + LPI10K",
    mpar_silv_2K = "silv ~ MPAR2K",
    mpar_silv_10K = "silv ~ MPAR10K"
  )

fits_grcu <- 
  lapply(mods_grcu,
         glm, family = binomial(link = "logit"), data = bird_land
  )

fits_silv <- 
  lapply(mods_silv,
         glm, family = binomial(link = "logit"), data = bird_land)

modTab_grcu <- aictab(fits_grcu,
                      second.ord = TRUE)
modTab_silv <- aictab(fits_silv,
                      second.ord = TRUE)

modTab_grcu
## For grcu, out of the defined models, the highest weighted model by far was
## the model using only patch area (PA) as a fixed effect (AICcWt = 0.74). The 
## second highest weighted model included patch perimeter (PP) and mean annual 
## rainfall as its fixed effects. It was > 2 delta AICc from the top model
## (dAICc = 3.40) and its weight accounted for the majority of the remaining 
## probability of being the best-fitting model (AICcWt = 0.13). These results 
## indicate that PA (and PP, given its near-perfect correlation to PA) and patch
## effects on the whole explain grcu presence better than landscape effects.
modTab_silv
## For silv, the highest weighted model included PP and mean annual rainfall
## as its fixed effects (AICcWt = 0.67) and the null model was the second best
## fitting and parsimonious model (dAICc = 3.04) and comprised the virtual
## majority of the remaining weight (AICcWt = 0.15). A landscape model 
## (MPAR at 2km) was tied next with a model of patch area (dAICc = 5.30) and
## these models contained 2 parameters to the highest weighted model's 3, 
## offering convincing evidence that PP and mean annual rainfall together are
## much better at explaining silv presence than landscape effects.

### Question 4: Calculate a model averaged estimate for one or more of the biologically important parameters in your models. For example, the effect of patch area (PA) is directly relevant to estimating the effects of proposed land use changes (e.g. vegetation clearance) on bird species.

## Only a handful of variables show up more than once in my model set (MPS and
## LPI) and those don't seem super biologically important to me, so I am going 
## to add a couple  models for a known biologically important variable ...

mods_pa_grcu <- 
  list(
    global_2K_grcu = "grcu ~ MPS2K + LPI2K + LSI2K + MNN2K",
    global_5K_grcu = "grcu ~ MPS5K + LPI5K + LSI5K + MNN5K",
    global_10K_grcu = "grcu ~ MPS10K + LPI10K + LSI10K + MNN10K",
    null_grcu = "grcu ~ 1",
    patch_area_grcu = "grcu ~ PA",
    patch_rain_perim_grcu = "grcu ~ MRF + PP",
    patch_rain_area_grcu = "grcu ~ MRF + PA", # added 
    patch_rain_area_perim_grcu = "grcu ~ MRF + PA + PP", # added
    patch_rain_grcu = "grcu ~ MRF", # added
    num_size_grcu_2K = "grcu ~ NumP2K + MPS2K + LPI2K",
    num_size_grcu_10K = "grcu ~ NumP10K + MPS10K + LPI10K",
    mpar_grcu_2K = "grcu ~ MPAR2K",
    mpar_grcu_10K = "grcu ~ MPAR10K"
  )
mods_pa_silv <- 
  list(
    global_2K_silv = "silv ~ MPS2K + LPI2K + LSI2K + MNN2K",
    global_5K_silv = "silv ~ MPS5K + LPI5K + LSI5K + MNN5K",
    global_10K_silv = "silv ~ MPS10K + LPI10K + LSI10K + MNN10K",
    null_silv = "silv ~ 1",
    patch_area_silv = "silv ~ PA",
    patch_rain_perim_silv = "silv ~ MRF + PP",
    patch_rain_area_silv = "silv ~ MRF + PA", # added 
    patch_rain_area_perim_silv = "silv ~ MRF + PA + PP", # added
    patch_rain_silv = "silv ~ MRF", # added
    num_size_silv_2K = "silv ~ NumP2K + MPS2K + LPI2K",
    num_size_silv_10K = "silv ~ NumP10K + MPS10K + LPI10K",
    mpar_silv_2K = "silv ~ MPAR2K",
    mpar_silv_10K = "silv ~ MPAR10K"
  )

# Fitting new model set ...
fits_pa_grcu <- 
  lapply(mods_pa_grcu,
         glm, family = binomial(link = "logit"), data = bird_land)
fits_pa_silv <- 
  lapply(mods_pa_silv,
         glm, family = binomial(link = "logit"), data = bird_land)

## Calculating model averages for PA
mod_avg_grcu <- modavg(cand.set = fits_pa_grcu,
       parm = "PA",
       second.ord = TRUE)
mod_avg_silv <- modavg(cand.set = fits_pa_silv,
       parm = "PA",
       second.ord = TRUE)

### Question 5: Create model ranking tables and marginal effects plots for 
### model-averaged parameters

## Model ranking tables
mod_avg_grcu$Mod.avg.table
mod_avg_silv$Mod.avg.table


## Marginal effects plots
# Hold everything but PA at its means
vars_grcu <- unique(unlist(lapply(mods_pa_grcu, 
                                  function(f) all.vars(as.formula(f))[-1])))
vars_grcu <- setdiff(vars_grcu, "PA")
means_grcu <- as.list(colMeans(bird_land[vars_grcu], na.rm = TRUE))
vars_silv <- unique(unlist(lapply(mods_pa_silv, 
                             function(f) all.vars(as.formula(f))[-1])))
vars_silv <- setdiff(vars_silv, "PA")
means_silv <- as.list(colMeans(bird_land[vars_silv], na.rm = TRUE))

# Create new data frame with new PA values
nd_pa <- with(bird_land,
              expand.grid(
                PA = seq(min(PA),
                            max(PA),
                            length.out = 100)
              ))
# Put the means of other values back with the new values
nd_pa_grcu <- cbind(nd_pa, means_grcu)
nd_pa_silv <- cbind(nd_pa, means_silv)

pred_pa_grcu

# Generate predicted coefficient and error estimates
pred_pa_grcu <- modavgPred(
  cand.set = fits_pa_grcu, 
  newdata = nd_pa_grcu, 
  type = "link")
pred_pa_silv <- modavgPred(
  cand.set = fits_pa_silv, 
  newdata = nd_pa_silv, 
  type = "link")

# Create predictions from distribution
nd_pa_grcu <- nd_pa_grcu %>%
  mutate(fit_prob = plogis(pred_pa_grcu$mod.avg.pred),
         lwr_prob = plogis(pred_pa_grcu$mod.avg.pred - 1.96 * pred_pa_grcu$uncond.se),
         upr_prob = plogis(pred_pa_grcu$mod.avg.pred + 1.96 * pred_pa_grcu$uncond.se))
nd_pa_silv <- nd_pa_silv %>%
  mutate(fit_prob = plogis(pred_pa_silv$mod.avg.pred),
         lwr_prob = plogis(pred_pa_silv$mod.avg.pred - 1.96 * pred_pa_silv$uncond.se),
         upr_prob = plogis(pred_pa_silv$mod.avg.pred + 1.96 * pred_pa_silv$uncond.se))

ggplot(nd_pa_grcu,
       aes(PA, fit_prob)) +
  geom_ribbon(aes(ymin = lwr_prob, ymax = upr_prob), alpha = 0.2) +
  geom_line()
ggplot(nd_pa_silv,
       aes(PA, fit_prob)) +
  geom_ribbon(aes(ymin = lwr_prob, ymax = upr_prob), alpha = 0.2) +
  geom_line()

