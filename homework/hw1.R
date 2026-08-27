# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, sf, vegan, quiet = TRUE)

# Load riparian vegetation monitoring dataset:
# I was getting an error including "repo_url" so I removed it.
dat <- 
  read_csv("data/rip.dat.02-14.csv")

# After running my first ggplot, I got an error that some rows were outside
# the scale range. I looked around the data frame and found that data is
# missing for all plots in 2014.

dat <- dat %>%
  # Filtering out 2014 observations (see above)
  filter_out(year == 2014) %>%
  # Plots have 3-character IDs and either a 1- or 2-character code.
  # Extract 4th and 5th characters and put it into a plotCode column after site. 
  mutate(plotCode = str_sub(site, 4, 5),
         # Then keep the first three characters in the site column.
         site = str_sub(site, 1, 3),
         # Sum all the species columns where the number is greater than 0
         Richness_total = rowSums(select(., 5:180) > 0),
         # Use diversity() to calculate inv Simpson's from abundance data 
         Diversity_InvSimp = diversity(select(., 5:180), index = "invsimpson")) %>%
  # Ordering columns; might be a cleaner way of doing this
  relocate(plotCode, .after = site) %>%
  relocate(Richness_total, .after = treatment) %>%
  relocate(Diversity_InvSimp, .after = Richness_total)

# Pivoting data long to view plots at the same time
dat_long <- dat %>%
  select(site, year, treatment, Richness_total, Diversity_InvSimp) %>%
  pivot_longer(cols = c(Richness_total, Diversity_InvSimp),
               names_to = "measures",
               values_to = "values")
# Boxplots to check for outliers
# Boxplots show 3 outliers for inverse Simpson's and 1 for richness
ggplot(dat_long,
       aes(y = values)) +
  geom_boxplot() +
  facet_wrap(~ measures, scale = "free")

# Histograms to check for outliers
# Histograms show the same outlier numbers
ggplot(dat_long,
       aes(x = values)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ measures, scale = "free")


# Comparing the number of plots per site per year per treatment
table(dat$year, dat$site, dat$treatment)
# The data is not completely balanced. 
# Between groups: First, the control group has data for two more sites than UE
# and LE. Next, the LE group has two sites missing from 2014. Finally, CRS1 has
# more plots than the other groups.
# Between years: The number of plots in CRS1 varies from year to year, and the
# sites in 2014 have more plots than the other years. 
# Between sites: CRS1 has more plots than other sites do.

# Boxplots indicate that inverse Simpson's does not seem to have a strong 
# trend across years. However, there does appear to be a slight positive linear 
# relationship b/t richness and year, up until 2008 where the values drop. 
ggplot(dat_long,
       aes(x = year, y = values, group = year)) +
  geom_boxplot() +
  facet_wrap(~ measures, scale = "free")
# Adding a linear regression line, though, suggests there might be a slight
# positive relationship b/t year and both diversity measures.
ggplot(dat_long,
       aes(x = year, y = values)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ measures, scale = "free")
  
# Both box and scatterplots show that some sites have far less variation in
# richness and inverse Simpson's than other sites, and the values for these
# measures themselves vary b/t sites. 
ggplot(dat_long,
       aes(x = site, y = values, group = site)) +
  geom_boxplot() +
  facet_wrap(~ measures, scale = "free")

ggplot(dat_long,
       aes(x = site, y = values)) +
  geom_point() +
  facet_wrap(~ measures, scale = "free")

# No huge differences b/t treatments for either diversity measure, but 
# sites that excluded livestock might have slightly lower inverse Simpson's
# than either of the other treatments. Furthermore, sites that excluded
# ungulates appear may have slightly higher richness than other sites.
ggplot(dat_long,
       aes(x = treatment, y = values, group = treatment)) +
  geom_boxplot() +
  facet_wrap(~ measures, scale = "free")
# Scatterplots indicate that the potential richness differences b/t ungulate-
# excluded sites and other sites may be smaller than the boxplots indicated,
# but livestock-excluded sites may still have lower inverse Simpson's.  
ggplot(dat_long,
       aes(x = treatment, y = values)) +
  geom_point() +
  facet_wrap(~ measures, scale = "free")

# Collinearity becomes an issue when dropping one covariate causes another 
# covariate to become significant or significantly change estimated parameters, 
# because those two covariates contain redundant information due to correlation.
# It is unlikely that year will be correlated with either site or treatment for 
# this study. It also seems unlikely that site and treatment are correlated.



ggplot(dat,
       aes(x = year, y = treatment)) +
  geom_point()

