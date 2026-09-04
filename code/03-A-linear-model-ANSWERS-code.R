df <- penguins
df <- 
  penguins %>%
  mutate(across(where(is.numeric), 
                function(X) (X - mean(X, na.rm = TRUE)) / sd(X, na.rm = TRUE),
                .names = "{.col}_z"))
fit <- 
  lm(body_mass_g ~ sex + bill_length_mm_z * bill_depth_mm_z, 
     data = df)
check_collinearity(fit)

summary(fit)

nd_new <- 
  nd %>%
  mutate(bill_length_mm_z = as.numeric((bill_length_mm - mean(df$bill_length_mm, na.rm = T)) / sd(df$bill_length_mm, na.rm = T)),
         bill_depth_mm_z = as.numeric((bill_depth_mm - mean(df$bill_depth_mm, na.rm = T)) / sd(df$bill_depth_mm, na.rm = T)))
pred_df <- 
  data.frame(nd_new,
             as.data.frame(predict(fit, nd_new, se.fit = TRUE))) %>%
  mutate(CI_0.975 = fit + (se.fit * 1.96),
         CI_0.025 = fit - (se.fit * 1.96))
ggplot(pred_df, 
       aes(x = bill_length_mm,
           y = fit,
           color = as.factor(bill_depth_mm), 
           fill = as.factor(bill_depth_mm),
           group = as.factor(bill_depth_mm),
           ymin = CI_0.025,
           ymax = CI_0.975)) + 
  facet_wrap(~ sex) + 
  geom_ribbon(alpha = 0.3,
              color = NA) + 
  geom_line() + 
  scale_colour_viridis_d(name = "Bill Depth") + 
  scale_fill_viridis_d(name = "Bill Depth") + 
  theme_bw() + 
  ylab("Penguin Body Mass (g)")
