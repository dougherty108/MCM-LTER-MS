########## Thermal Diffusion Model Result Interpretation #########
library(tidyverse)

setwd("/Users/charliedougherty/Documents/R-Repositories/MCM-LTER-MS")

# load file
GEE_corrected <- read_csv("data/thermal diffusion model data/model_outputs/GEE_output_corrected_20250402.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

summary(GEE_corrected$thickness)

# ice thickness data
ice_thick <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20250218_0.csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(location_name == "East Lake Bonney", 
         ) |> 
  filter(date_time > "2016-12-01" & date_time < "2025-02-01") |> 
  group_by(date_time) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

summary(ice_thick$mean_thickness)

# plot modeled ice thickness against the measured thickness
ggplot() + 
  geom_line(data = GEE_corrected, aes(x = time, y = thickness), linewidth = 1.25) + 
  geom_point(data = ice_thick, aes(x = date_time, y = mean_thickness), color = "red") +
  xlab("Time") + ylab("Ice Thickness (m)") + 
  ggtitle("East Lake Bonney Ice Thickness", 
          subtitle = "modeled vs. measured") +
  theme_linedraw(base_size = 20)

#ggsave("plots/manuscript/chapter 2/measured_vs_modeled.png", 
#       dpi = 300, height = 8, width = 12)

# sum model output to a daily average, to compare to measured ice thickness
# goal here is to see how large the gap is between modeled data and measured data 
modeled_daily <- GEE_corrected |> 
  mutate(time = ymd_hms(time), 
         date_time = date(time)) |> 
  group_by(date_time) |> 
  summarize(modeled_thickness = mean(thickness)) 


### join two datasets together to compare dates
comp <- ice_thick |> 
  left_join(modeled_daily, by = join_by(date_time)) |> 
  group_by(date_time) |> 
  mutate(difference = modeled_thickness - mean_thickness)

summary(comp$mean_thickness)
summary(comp$modeled_thickness)
summary(comp$difference)

#plot modeled and measured against each other
ggplot(comp, aes(mean_thickness, modeled_thickness)) + 
  geom_point() + 
  geom_abline() + 
  xlab("Measured Ice Thickness") + ylab("Modeled Ice Thickness") + 
  theme_linedraw(base_size = 20)

#ggsave(filename = "plots/manuscript/chapter 2/modeledice_vs_measuredice.png", 
#       width = 8, height = 6, dpi = 700)

linear_model = lm(modeled_thickness ~mean_thickness, data = comp)

summary(linear_model)

thickness_pivot <- comp |> 
  pivot_longer(cols = c(modeled_thickness, mean_thickness), 
               names_to = "measurement_type", values_to = "thickness") |> 
  select(date_time, measurement_type, thickness)

# plot output to confirm join worked
#ggplot(thickness_pivot, aes(date_time, thickness, color = measurement_type)) +
#  geom_()


# what if you remove data past 2022, when the longwave data gets particularly bad
comp2 <- comp |> 
  filter(date_time < "2019-05-01")

lin_model_filtered = lm(modeled_thickness ~mean_thickness, data = comp2)
summary(lin_model_filtered)

## if you ignore data past 2021, the r_squared value is 0.87. If you include 

ggplot(comp2, aes(modeled_thickness, mean_thickness)) + 
  geom_point() + 
  geom_abline()

