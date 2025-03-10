####### SCRIPT TO INVESTIGATE THE INFLUENCE OF WIND, AIR TEMP, OR SOLAR RADIANCE 
# ON ICE THICKNESS. important because if there's a strong relation between these values, then
# that changes how we understand the sediment stuff. 

# libraries
library(tidyverse)
library(lubridate)

# add in data
ice_thickness <- read_csv("data/lake ice/mcmlter")