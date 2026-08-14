library(dplyr)
library(glmmTMB)
library(readr)
library(tidyverse)
library(car)

junco_dataset_corrected_2025 <- read_csv("Downloads/junco_dataset - corrected_2025.csv")

df <- junco_dataset_corrected_2025



#Data prep.

df$daily.cosine <- cos(2*pi*df$hour)
df$daily.sine <- sin(2*pi*df$hour) #paired harmonics for daily periodic song trends
df$yearly.cosine <- cos(2*pi*df$calendar_date/365)
df$yearly.sine <- sin(2*pi*df$calendar_date/365)
df$site_num <- as.integer(as.factor(df$site))

#set year as an ordered factor
df <- df %>%
  mutate(
    burn     = factor(burn,     levels = c(0, 1), labels = c("unburned", "burned")),
    stage    = factor(stage,    levels = c(0, 1), labels = c("non-provisioning", "provisioning")),
    playback = factor(playback, levels = c(0, 1), labels = c("first playback", "repeat playback")),
    year     = factor(year,     levels = c(2023, 2024, 2025), ordered = TRUE),
    year_polynomial   = poly(as.numeric(year), 2)[, 1],
  )


nestedm1.tmb <- glmmTMB(
  songs ~ burn * year +
    daily.cosine + daily.sine + 
    yearly.cosine + yearly.sine + 
    stage +
    playback +
    (1 | site/birdID),
  family=nbinom2,
  data = df,
  verbose = TRUE
)

summary(nestedm1.tmb)


nestedm0.tmb <- glmmTMB(
  songs ~ burn + year +
    daily.cosine + daily.sine +
    yearly.cosine + yearly.sine +
    stage + 
    playback + 
    (1 | site/birdID),
  family=nbinom2,
  data = df,
  verbose = TRUE
)

summary(nestedm0.tmb)

#compare additive and interactive models with an anova
anova(nestedm1.tmb, nestedm0.tmb)


#plots of the residuals vs. predictions
m0.residuals <- residuals(nestedm0.tmb, type = "pearson")
m0.predictions <- predict(nestedm0.tmb, type = "link")

plot(m0.predictions, m0.residuals)


df2 <- read_csv("Downloads/junco_dataset - fid_analysis.csv")


df2$daily.cosine <- cos(2*pi*df2$hour)
df2$daily.sine <- sin(2*pi*df2$hour) #paired harmonics for daily periodic song trends
df2$yearly.cosine <- cos(2*pi*df2$calendar_date/365)
df2$yearly.sine <- sin(2*pi*df2$calendar_date/365)
df2$site_num <- as.integer(as.factor(df2$site))


df2 <- df2 %>%
  mutate(
    burn     = factor(burn,     levels = c(0, 1), labels = c("unburned", "burned")),
    stage    = factor(stage,    levels = c(0, 1), labels = c("non-provisioning", "provisioning")),
    playback = factor(playback, levels = c(0, 1), labels = c("first playback", "repeat playback"))
)


#I used "playback" here, because FIDs and playbacks were repeated in tandem with each other.

df2 <- df2 |> mutate(year = factor(year, levels = c(2024, 2025)))

fid.tmb <- glmmTMB(
  fid ~ burn + year + 
    daily.cosine + daily.sine +
    yearly.cosine + yearly.sine + stage + playback + 
    (1 | site/birdID),
  data       = df2,
  family     = gaussian()
)

summary(fid.tmb)


df3 <- read_csv("Downloads/junco_dataset - condition_updated (2).csv")

df3$daily.cosine <- cos(2*pi*df3$hour)
df3$daily.sine <- sin(2*pi*df3$hour) #paired harmonics for daily periodic song trends
df3$yearly.cosine <- cos(2*pi*df3$calendar_date/365)
df3$yearly.sine <- sin(2*pi*df3$calendar_date/365)
df3$site_num <- as.integer(as.factor(df3$site))


df3 <- df3 %>%
  mutate(
    burn     = factor(burn,     levels = c(0, 1), labels = c("unburned", "burned")),
    stage    = factor(stage,    levels = c(0, 1), labels = c("non-provisioning", "provisioning"))
  )

df3 <- df3 |> mutate(year = factor(year, levels = c(2023, 2024)))


condition.tmb.burn <- glmmTMB(
  condition ~ burn + year +
    yearly.cosine + yearly.sine + stage + daily.sine + daily.cosine + 
    (1 | site),
  data       = df3,
  family     = gaussian(),
)

summary(condition.tmb.burn)


#testing for differences in variance in songs, FID, and condition between burned and unburned.
library(car)

leveneTest(df$songs, df$burn)
leveneTest(df2$fid, df2$burn)
leveneTest(df3$condition, df3$burn)



