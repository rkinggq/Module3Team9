#setwd("/Users/shiningdarklight/Documents/BIOL3140/Module3Team9")

#Load libraries
library(tidyverse)
library(ape)
library(nlme)
library(MuMIn)

#Question 1
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole %>%
  left_join(anole.eco) %>%
  filter(!Ecomorph %in% c("U", "CH")) %>%
  na.omit()

anole.log <- anole2 %>%
  mutate_at(c("SVL", "HTotal", "PH", "ArbPD"), log)

#Question 2
lm_ph <- lm(HTotal ~ SVL + PH, data = anole.log)
lm_pd <- lm(HTotal ~ SVL + ArbPD, data = anole.log)

#Question 3
anole.log <- anole.log %>%
  mutate(
    res_ph = residuals(lm_ph),
    res_pd = residuals(lm_pd)
  )

#Plot for perch height
p_ph <- ggplot(anole.log, aes(x = Ecomorph2, y = res_ph)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Residuals of Hindlimb-SVL Model with Perch Height",
       x = "Ecomorph", y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_ph)

#Plot for perch diameter
p_pd <- ggplot(anole.log, aes(x = Ecomorph2, y = res_pd)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Residuals of Hindlimb-SVL Model with Perch Diameter",
       x = "Ecomorph", y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_pd)

#Question 4
anole.tree <- read.tree("anole.tre")

pgls_ph <- gls(HTotal ~ SVL + PH, 
               correlation = corBrownian(1, phy = anole.tree, form = ~Species),
               data = anole.log, method = "ML")

pgls_pd <- gls(HTotal ~ SVL + ArbPD, 
               correlation = corBrownian(1, phy = anole.tree, form = ~Species),
               data = anole.log, method = "ML")

pgls_both <- gls(HTotal ~ SVL + PH + ArbPD, 
                 correlation = corBrownian(1, phy = anole.tree, form = ~Species),
                 data = anole.log, method = "ML")

#Question 5
aic_results <- AICc(pgls_ph, pgls_pd, pgls_both)
aic_weights <- aicw(aic_results$AICc)

print(aic_weights)

#Determine the best model
best_model <- list(pgls_ph, pgls_pd, pgls_both)[[which.min(aic_results$AICc)]]

#Print summary of the best model
summary(best_model)

#Question 6
anole.log$best_residuals <- residuals(best_model)

p_final <- ggplot(anole.log, aes(x = PH, y = best_residuals, color = Ecomorph2)) +
  geom_point(aes(size = ArbPD)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Ecomorph2) +
  labs(title = "Effects of Perch Height and Diameter on Hindlimb Length",
       x = "Log Perch Height", y = "Residuals",
       color = "Ecomorph", size = "Log Perch Diameter") +
  theme_minimal() +
  theme(legend.position = "right")

print(p_final)