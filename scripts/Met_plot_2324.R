##### Met Station Plotting #######
library(tidyverse)
library(lubridate)
library(ggpubr)

#set working directory
setwd("~/Documents/R-Repositories/MCMLTER")

#Read File
file = read_delim("data/WLBBB/WLB_BB_CR1000X_WLB15min.dat", delim = ",", skip = 1) |> 
  select(2) |> pull(1)

# West Lobe
WL = read.csv("data/WLBBB/WLB_BB_CR1000X_WLB15min.dat", skip = 0, header = F, nrows = 1, 
                as.is = T) |> select(2) |> pull(1)
headers_WL = read.csv("data/WLBBB/WLB_BB_CR1000X_WLB15min.dat", skip = 1, header = F, nrows = 1, as.is = T)
WLBBB = read_csv("data/WLBBB/WLB_BB_CR1000X_WLB15min.dat", skip = 4)
colnames(WLBBB) = c(headers, 'sitename')

WL1min = read.csv("data/WLBBB/WLB_BB_CR1000X_WLB1min.dat", skip = 0, header = F, nrows = 1, 
              as.is = T) |> select(2) |> pull(1)
headers_WL1min = read.csv("data/WLBBB/WLB_BB_CR1000X_WLB1min.dat", skip = 1, header = F, nrows = 1, as.is = T)
WLBBB1min = read_csv("data/WLBBB/WLB_BB_CR1000X_WLB1min.dat", skip = 4)
colnames(WLBBB1min) = c(headers_WL1min, 'sitename')

# East Lobe
EL = read.csv("data/ELBBB/TOA5_40095.ELB15min.dat", skip = 0, header = F, nrows = 1, 
              as.is = T) |> select(2) |> pull(1)
headers_EL = read.csv("data/ELBBB/TOA5_40095.ELB15min.dat", skip = 1, header = F, nrows = 1, as.is = T)
ELBBB = read_csv("data/ELBBB/TOA5_40095.ELB15min.dat", skip = 4)
colnames(ELBBB) = c(headers_EL, 'sitename') 

ELBBB = ELBBB |> 
  filter(TIMESTAMP < "2098-08-23 06:28:16") |> 
  pivot_longer(cols = c(OSat_DOdeep_Avg, OSat_Dshallow_Avg),
              names_to = "DO Depth", 
              values_to = "DO_Sat")

WLBBB = WLBBB |> 
  pivot_longer(cols = c(OSat_DOdeep_Avg, OSat_Dshallow_Avg),
               names_to = "DO Depth", 
               values_to = "DO_Sat")

# DO Concentration Plot WL
W_plot <- ggplot(WLBBB, aes(x = TIMESTAMP, y = DO_Sat)) +
  geom_point(aes(color = `DO Depth`)) +
  ggtitle("DO Concentration (mg/L) West Lobe Bonney")

ggplot(WLBBB, aes(x = TIMESTAMP, y = ))


## EL DO Conc
E_plot <- ggplot(ELBBB, aes(x = TIMESTAMP, y = DO_Sat)) +
  geom_point(aes(color = `DO Depth`)) +
  ggtitle("DO Concentration (mg/L) East Lobe Bonney")

ggarrange(W_plot, E_plot)

# Stage and Ablation data

WLBBB_stage = WLBBB |> 
  pivot_longer(cols = c(ablation_Avg, stage_Avg),
               names_to = "Measurement Type", 
               values_to = "Stage/Ablation (m)")

ggplot(WLBBB, aes(x = TIMESTAMP, y = stage_Avg)) + 
  geom_path()

ggplot(WLBBB, aes(x = TIMESTAMP, y = ablation_Avg)) + 
  geom_path()

ggplot(WLBBB_stage, aes(x = TIMESTAMP, y = `Stage/Ablation (m)`)) + 
  geom_point(aes(color = `Measurement Type`)) +
  ggtitle("Stage and Ablation West Lobe (m)")

# Air temp by time of day
WLBBB_air <- WLBBB |> 
  group_by(MilitaryTime) |> 
  summarize(mean_air_C = mean(Air_Temp_Avg))

ggplot(WLBBB_air, aes(x = MilitaryTime, y = mean_air_C, color = MilitaryTime)) + 
  geom_point() + 
  scale_color_continuous()

loop = 8:26
for(i in loop) {
  
  var=names(WLBBB)[i]
  
  x <- WLBBB[,i]
  print(
    ggplot(WLBBB, aes(TIMESTAMP, x)) + 
      geom_point() + 
      labs(title = var) + 
      xlab("Date") + ylab(paste(i))
  )
} #
#|> 
  ggsave(filename = paste("plots/WLBBB_2023/WLBBB",i,".png", sep = "" ))







