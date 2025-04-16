###### Model output scenarios Interrogation

library(tidyverse)
library(ggpubr)
library(MetBrewer)

# load ice thickness data
# load ice thickness data and manipulate for easier plotting
ice_thickness <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20250218_0_2025.csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(location_name == "East Lake Bonney") |> 
  filter(date_time > "2016-12-01" & date_time < "2024-04-01")


normal_output <- read_csv("data/thermal diffusion model data/model_outputs/GEE_output_corrected_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time), 
         type = "normal output") |> 
  filter(thickness > 0)

increase_5 <- read_csv("data/thermal diffusion model data/model_outputs/5%_increase_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time), 
         type = "increase-05%") |> 
  filter(thickness > 0)


increase_10 <- read_csv("data/thermal diffusion model data/model_outputs/10%_increase_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time), 
         type = "increase-10%") |> 
  filter(thickness > 0)

decrease_5 <- read_csv("data/thermal diffusion model data/model_outputs/5%_decrease_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time), 
         type = "decrease-05%") |> 
  filter(thickness > 0)

decrease_10 <- read_csv("data/thermal diffusion model data/model_outputs/10%_decrease_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time), 
         type = "decrease-10%") |> 
  filter(thickness > 0)

sw_10 <- read_csv("data/thermal diffusion model data/model_outputs/10%_increase_sw_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)


# what if i joined things together instead of doing separate plots. would look a lot cleaner. 
all_outputs = rbind(normal_output, increase_5, increase_10, decrease_5, decrease_10) |> 
  as.data.frame() |> 
  mutate(scenario = type)

# plot all data on one plot
all_outputs |> 
  #group_by(time) |> 
  #summarize(thickness = max(thickness)) |> 
  ggplot(aes(time, y = thickness)) + 
  geom_line(aes(color = scenario), size = 1.5) + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "Time", y = "Ice Thickness (m)") +
  geom_point(data = ice_thickness, aes(x = date_time, y = z_water_m), size = 2) + 
  theme_linedraw(base_size = 25)

ggsave("plots/manuscript/chapter 2/all_scenarios_one_plot.png", 
       dpi = 500, height = 8, width = 14)

#summarize to put some actual numbers to the
all_outputs |> 
  group_by(scenario) |> 
  summarize(min = min(thickness), 
            q25 = quantile(thickness, 0.25, na.rm = TRUE),
            median = median(thickness, na.rm = TRUE),
            mean = mean(thickness),
            q75 = quantile(thickness, 0.75, na.rm = TRUE),
            max = max(thickness)
            )


## Shortwave increase example
sw_increase <- read_csv("data/thermal diffusion model data/model_outputs/10%_increase_sw_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

sw_increase |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  ggplot(aes(x = time, y = thickness)) +
  geom_line(color = "red", size = 1) +
  labs(x = "Time", y = "Ice Thickness (m)",
       title = "10% increase in SW") +
  geom_point(data = ice_thickness, aes(x = date_time, y = z_water_m)) + 
  theme_linedraw(base_size = 20)

ggsave("plots/manuscript/chapter 2/sw_increase_plot.png", 
       dpi = 500, height = 8, width = 14)

