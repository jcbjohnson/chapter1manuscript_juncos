#comparison of ndvi and lst between burned and unburned sites


library(ggplot2)
library(car)

df <- read_csv("Downloads/zonaldata.xlsx - sites_data_burn (1).csv")
df2 <- read_csv("Downloads/zonaldata.xlsx - sites_data_temp.csv")


df$burn <- as.factor(df$burn)
df2$burn <- as.factor(df2$burn)


#lm to look at differences in overall NDVI and LST
ndvilm <- lm(average_NDVI_all_years ~ burn, data = df)
lstlm <- lm(average_LST ~ burn, data = df2)

summary(ndvilm)
summary(lstlm)

#Levene's tests to compare variance in NDVI and LST between burned and unburned:

leveneTest(average_NDVI_all_years ~ burn, data = df)
leveneTest(average_LST ~ burn, data = df2)


#burned sites have higher LST, lower NDVI, and are more variable for both.


boxplots <- ggplot(df, aes(x = burn, y = average_NDVI_all_years, fill = burn)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.85) +
  geom_jitter(width = 0.12, height = 0, size = 1.6,
              alpha = 0.45, colour = "black") +
  scale_fill_manual(values = c("0" = "#4c9f70",
                               "1"   = "#c1666b")) +
  labs(x = "Burn Presence",
       y = "Mean NDVI",
       title = "NDVI in burned vs unburned") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(boxplots)



boxplots2 <- ggplot(df2, aes(x = burn, y = average_LST, fill = burn)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.85) +
  geom_jitter(width = 0.12, height = 0, size = 1.6,
              alpha = 0.45, colour = "black") +
  scale_fill_manual(values = c("0" = "#4c9f70",
                               "1"   = "#c1666b")) +
  labs(x = "Burn Presence",
       y = "Mean LST",
       title = "LST in burned vs unburned") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(boxplots2)

