library(iccCounts)
library(rptR)
library(dplyr)
library(ggplot2)
library(readr)


junco_dataset_corrected_2025 <- read_csv("Downloads/junco_dataset - corrected_2025 (2).csv")

df <- junco_dataset_corrected_2025

y   <- "songs"      
id  <- "birdID"    
GRP <- "burn"      

#obtaining the ICC for "songs" using the package iccCounts (Carrasco 2022).
#this is necessary instead of "rptr", which struggles with overdispersed count data.


icc_counts(
  
  df,
  y,
  id,
  met = NULL,
  type = "rep",
  fam = "nbinom2",
  conf = 0.95
)


#switching to rptr for "FID", which fits a normal distribution.

df2 <- read_csv("Downloads/fid_repeatability - fid_analysis.csv")


df2$daily.cosine <- cos(2*pi*df2$hour)
df2$daily.sine <- sin(2*pi*df2$hour) #paired harmonics for daily periodic trends
df2$yearly.cosine <- cos(2*pi*df2$calendar_date/365)
df2$yearly.sine <- sin(2*pi*df2$calendar_date/365)
df2$site_num <- as.integer(as.factor(df2$site))


df2 <- df2 %>%
  mutate(
    burn     = factor(burn,     levels = c(0, 1), labels = c("unburned", "burned")),
    stage    = factor(stage,    levels = c(0, 1), labels = c("non-provisioning", "provisioning")),
    playback = factor(playback, levels = c(0, 1), labels = c("first playback", "repeat playback"))
  )


#keeping the same covariates for FID as in the glmmtmb model for consistency.
#removed "site" as a random effect to focus on birdID as the effect of interest.
rptfid <- rpt(
  fid ~ burn + year + daily.sine + daily.cosine +
    stage + playback + yearly.sine + yearly.cosine + (1|birdID),
  grname   = "birdID",
  data     = df2,
  datatype = "Gaussian",
  nboot    = 1000,
  npermut  = 0,
  ratio    = TRUE
)

summary(rptfid)

