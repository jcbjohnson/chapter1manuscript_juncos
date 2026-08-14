#walkthrough for hierarchical distance sampling comparing junco density between burned and unburned
#10 sites (5 burned, 5 unburned), 10 point-count stations/site, 3 visits/station.
#distances recorded continuously (0-100 m) for each detected individual.
#focuses on unmarked::gdistsamp (Chandler, Royle & King 2011)

library(dplyr)
library(unmarked)

data <- "/Users/jacobjohnson/Point Counts Scoring - points_code (8).csv"

#first, setting up the data to create our umf
#we want to create detection bins, noting that each station has 3 visits
#we also want to distinguish between burned and unburned. 

df <- read.csv(data, stringsAsFactors = FALSE)
df <- df[, c("site_id","station_id","date","visit","julian_date","hour","species",
             "distance_m","how","time_remain","burn_status")]

df$station <- factor(paste(df$site_id, d$station_id, sep = "_"),
                     levels = sort(unique(paste(df$site_id, d$station_id, sep = "_"))))
df$visit_f <- factor(as.character(df$visit), levels = c("1","2","3"))

#assign levels for each visit.

deju <- df %>% filter(species == "DEJU")

#I detected many more species than DEJU, but this is my primary species of interest.

cutpoints <- c(0, 25, 50, 75, 100)   # distance bins, out to a max of 100m.
y <- formatDistData(deju, distCol = "distance_m", transectNameCol = "station",
                    dist.breaks = cutpoints, occasionCol = "visit_f")

station_covs <- df %>% distinct(site_id, station, burn_status) %>% arrange(station)

station_covs$burn_status_f <- factor(ifelse(station_covs$burn_status == 1, "burned", "unburned"),
                                     levels = c("unburned","burned"))

#arranging data before making the unmarked frame

umf <- unmarkedFrameGDS(y = y,siteCovs = data.frame(burn_status = station_covs$burn_status_f,
                                                    site_id = station_covs$site_id),
                        numPrimary = 3, dist.breaks = cutpoints,
                        survey = "point", unitsIn = "m")


#performing model selection. I relied heavily on Lionel Leston's work for this.
#see: https://rstudio-pubs-static.s3.amazonaws.com/221408_23c61679859e48e6bae0b9c5c2e48a92.html


#testing candidate models. It was unclear from visualization whether a half-normal function or hazard was better.
#also testing on how burn affects density (lambda) and detectability (p)
#lambda (first) — density covariates
#phi(second) — availability across visits (I kept this constant)
#p (third)— detection probability covariates

hn_null      <- gdistsamp(~1, ~1, ~1, umf, keyfun = "halfnorm", output = "density", unitsOut = "ha", K = 50)
hn_burndetect     <- gdistsamp(~1, ~1, ~burn_status, umf, keyfun = "halfnorm", output = "density", unitsOut = "ha", K = 50)
hn_burndensity     <- gdistsamp(~burn_status, ~1, ~1, umf, keyfun = "halfnorm", output = "density", unitsOut = "ha", K = 50)
hn_burndensitydetect <- gdistsamp(~burn_status, ~1, ~burn_status, umf, keyfun = "halfnorm", output = "density", unitsOut = "ha", K = 50)
haz_null      <- gdistsamp(~1, ~1, ~1, umf, keyfun = "hazard", output = "density", unitsOut = "ha", K = 50)
haz_burndetect     <- gdistsamp(~1, ~1, ~burn_status, umf, keyfun = "hazard", output = "density", unitsOut = "ha", K = 50)
haz_burndensity     <- gdistsamp(~burn_status, ~1, ~1, umf, keyfun = "hazard", output = "density", unitsOut = "ha", K = 50)
haz_burndensitydetect <- gdistsamp(~burn_status, ~1, ~burn_status, umf, keyfun = "hazard", output = "density", unitsOut = "ha", K = 50)


models <- fitList('Halfnorm, Null' = hn_null, 'Halfnorm, BurnDetect'= hn_burndetect,
                  'Halfnorm, BurnDensity' = hn_burndensity, 'Halfnorm, BurnDensity+Detect' = hn_burndensitydetect,
                  'Hazard, Null' = haz_null,'Hazard, BurnDensity' = haz_burndensity, 'Hazard, BurnDetect'= haz_burndetect,
                  'Hazard, BurnDensity+Detect' = haz_burndensitydetect)

ms <- modSel(models)
print(ms)

#using AIC, hazard function seems much better than half-normal.
#the top model suggests that detectability differs between burned and unburned,
#but is close with the second highest model suggesting density also differs given detectability.
#next step is to compare the models.

null_density <- haz_burndetect  #the simpler model here is nested within the more complex one.      
burn_density <- haz_burndensitydetect

null_density
burn_density

#note that I received a warning message about large SE values.
#This is from a boundary effect on "availability" (or phi) being near 1,
#But does not impact the rest of the results.

#comparing the two models using a chisq test, and getting a p value.

lrt <- 2 * (logLik(burn_density) - logLik(null_density))
p_lrt <- pchisq(as.numeric(lrt), df = 1, lower.tail = FALSE)

lrt
p_lrt

#this gives us a chisq of 0.41, rounded up, and a p value of 0.52. 

#finally, let's get the estimated density values between the two 

nd <- data.frame(burn_status = factor(c("unburned","burned"), levels = c("unburned","burned")))
dens <- predict(m_burn, type = "lambda", newdata = nd, appendData = TRUE)
dens

#predicted densities differ slightly between burned and unburned, but not significantly.
