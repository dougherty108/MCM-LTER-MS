###### Model output scenarios Interrogation

library(tidyverse)


GEE_normal <- read_csv("data/thermal diffusion model data/model_outputs/GEE_output_corrected_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

increase_5 <- read_csv("data/thermal diffusion model data/model_outputs/5%_increase_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

increase_10 <- read_csv("data/thermal diffusion model data/model_outputs/10%_increase_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

decrease_5 <- read_csv("data/thermal diffusion model data/model_outputs/5%_decrease_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)

decrease_10 <- read_csv("data/thermal diffusion model data/model_outputs/10%_decrease_a_20250414.csv") |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  mutate(time = ymd_hms(time)) |> 
  filter(thickness > 0)




