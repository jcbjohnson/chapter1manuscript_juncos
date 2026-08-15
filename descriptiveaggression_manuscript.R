#descriptive behavioral analyses on junco songs, time within five, and flights.

library(psych)

junco_dataset_corrected_2025 <- read_csv("Downloads/junco_dataset - corrected_2025.csv")

df <- junco_dataset_corrected_2025

df$burn.factor <- factor(df$burn, levels = c(0,1), labels = c('unburned', 'burned'))


describeBy(songs ~ burn, data = df)

describeBy(flights ~ burn, data = df)

describeBy(tw5 ~ burn, data = df)

#in all instances, these behavioral measures in burned habitat are more than 2 se from the unburned mean.
#this functions as a 95% confidence interval, suggesting that all main metrics for aggression are higher.




#simple denstiy with rug plots

ggplot(df,aes(x=songs,fill=burn.factor))+
  geom_density(alpha = 0.5) +     # Overlaid densities with transparency
  geom_rug(aes(color = burn.factor), alpha = 0.3) + # Rug plot colored by group
  scale_fill_manual(values  = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  scale_color_manual(values = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  labs(title = "Overlaid Density with Rug Plot", x = "Songs", y = "Density") +
  theme_classic()

ggplot(df,aes(x=tw5,fill=burn.factor))+
  geom_density(alpha = 0.5) +     # Overlaid densities with transparency
  geom_rug(aes(color = burn.factor), alpha = 0.3) + # Rug plot colored by group
  scale_fill_manual(values  = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  scale_color_manual(values = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  labs(title = "Overlaid Density with Rug Plot", x = "Time within 5", y = "Density") +
  theme_classic()


ggplot(df,aes(x=flights,fill=burn.factor))+
  geom_density(alpha = 0.5) +     # Overlaid densities with transparency
  geom_rug(aes(color = burn.factor), alpha = 0.3) + # Rug plot colored by group
  scale_fill_manual(values  = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  scale_color_manual(values = c("unburned" = "#0077BB", "burned" = "#EE7733")) +
  labs(title = "Overlaid Density with Rug Plot", x = "Flights", y = "Density") +
  theme_classic()

