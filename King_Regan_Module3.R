library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

setwd("/Users/reganking/Downloads/Module3Team9")
anole <- read.csv("anole.dat.csv")
anole.eco <- read.csv("anole.eco.csv")
tree <- read.tree("anole.tre")

##Objective 1

anole2 <- anole %>%
  left_join(anole.eco) %>%
  filter(!Ecomorph%in%c("U","CH")) %>%
  na.omit() 
  
anole.log <- anole2 %>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)


##Objective 2
# Simple linear model: hindlimb ~ perch height
lm_height <- lm(HTotal ~ PH, data = anole.log)

# Simple linear model: hindlimb ~ perch diameter
lm_diameter <- lm(HTotal ~ ArbPD, data = anole.log)

##Objective 3

# Add residuals to the tibble
anole.log <- anole.log %>%
  mutate(resid_height = residuals(lm_height),
         resid_diameter = residuals(lm_diameter))

# Plot residuals for perch height
ggplot(anole.log, aes(x = Ecomorph, y = resid_height)) +
  geom_boxplot() +
  labs(title = "Residuals of Hindlimb-SVL vs Perch Height", y = "Residuals", x = "Ecomorph")

# Plot residuals for perch diameter
ggplot(anole.log, aes(x = Ecomorph, y = resid_diameter)) +
  geom_boxplot() +
  labs(title = "Residuals of Hindlimb-SVL vs Perch Diameter", y = "Residuals", x = "Ecomorph")

##Objective 4

##fit the model for hindlimb ~ perch height 
pgls_height <- gls(HTotal ~ PH, 
      correlation = corBrownian(1, phy = tree, form = ~Species),
      data = anole.log)

##fit the mocel for hindlimb ~ perch diameter 
pgls_diameter <- gls(HTotal ~ ArbPD, 
                     correlation = corBrownian(1, phy = tree, form = ~ Species), 
                     data = anole.log)

# Fit the PGLS model for hindlimb ~ perch height + perch diameter
pgls_both <- gls(HTotal ~ PH + ArbPD, 
                 correlation = corBrownian(1, phy = tree, form = ~ Species), 
                 data = anole.log)

##Objective 5

#Extract AICc values
AICc_height <- AIC(pgls_height, k = log(nrow(anole.log)))
AICc_diameter <- AIC(pgls_diameter, k = log(nrow(anole.log)))
AICc_both <- AIC(pgls_both, k = log(nrow(anole.log)))

#Function to compute AIC weights
AICweights <- function(AIC_vals) {
  delta_AIC <- AIC_vals - min(AIC_vals)
  exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
}

#Compute AIC weights
AIC_values <- c(AICc_height, AICc_diameter, AICc_both)
AICw_values <- AICweights(AIC_values)

#Output AICc values and AIC weights
AIC_summary <- data.frame(Model = c("Perch Height", "Perch Diameter", "Both Covariates"),
                          AICc = AIC_values,
                          AICw = AICw_values)

print(AIC_summary)

##Objective 6

anole.log <- anole.log %>%
  mutate(resid_best = residuals(pgls_both))  # Assuming pgls_both is your best model

# Create a plot of residuals against ecomorph
ggplot(anole.log, aes(x = Ecomorph, y = resid_best, fill = Ecomorph)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 1, alpha = 0.5, position = position_jitter(0.2)) +  # Add points for individual observations
  labs(title = "Hindlimb Residuals by Ecomorph from Best PGLS Model",
       y = "Residuals",
       x = "Ecomorph") +
  theme_minimal() +
  scale_fill_viridis_d()  # Using viridis color scale for better color visibility


