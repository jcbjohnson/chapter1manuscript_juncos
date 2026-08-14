#calculator for the sma of juncos which received mass and tarsus measurements.
#lmodel2 used for sma (Legendre 2026)

install.packages("lmodel2")
library(lmodel2)

df <- read_csv("Downloads/junco_morphology_dataset - mass_metrics (1).csv")

#standard major axis (SMA) regression
sma_model <- lmodel2(log(mass) ~ log(tarsus), data = df, nperm = 99)

#get the SMA slope (b_SMA)
b_sma <- sma_model$regression.results[sma_model$regression.results$Method == "SMA", "Slope"]

#get mean tarsus length
L_0 <- mean(df$tarsus)

#calculate the Scaled Mass Index (SMI) for all birds
df$SMI <- df$mass * ((L_0 / df$tarsus) ^ b_sma)

df$SMI


