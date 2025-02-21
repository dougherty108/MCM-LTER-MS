###### Data Check ######
# load packages

library("tidyverse")
library("scales")
library(lubridate)

# Load file
WLBBB <- read.delim(file = "data/WLBBB_15min.dat", header = TRUE, sep = ",", 
                    skip = 1)

site <- read.delim(file = "data/LFBB/Data/TOA5_40106.LF15min.dat", header = TRUE, sep = ",", 
                   skip = 1)

site <- site[-c(1,2),]

site <- site |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) |> 
  mutate(stage_Avg = as.numeric(stage_Avg), 
         Temp_stg_Avg = as.numeric(Temp_stg_Avg), 
         ablation_Avg = as.numeric(ablation_Avg), 
         UW_PAR_moored_Avg = as.numeric(UW_PAR_moored_Avg), 
         UW_PAR_hanging_Avg = as.numeric(UW_PAR_hanging_Avg), 
         Air_Temp_Avg = as.numeric(Air_Temp_Avg),
         BattV_Min = as.numeric(BattV_Min))
View(site)
loopy <- 8:23


for(i in loopy) {
  
  var=names(site)[i]
  
  h <- site[,i]
  print(
    ggplot(site, aes(TIMESTAMP, h)) + 
      geom_point() + 
      labs(title = var) + 
      xlab("Date") + ylab(paste(i))
  )
} |> 
  ggsave(filename = paste("plots/LFBB/LFBB",i,".png", sep = "" ))


#For DO Blue Boxes
site <- site |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) |> 
  mutate(stage_Avg = as.numeric(stage_Avg), 
         Temp_stg_Avg = as.numeric(Temp_stg_Avg), 
         ablation_Avg = as.numeric(ablation_Avg), 
         UW_PAR_moored_Avg = as.numeric(UW_PAR_moored_Avg), 
         UW_PAR_hanging_Avg = as.numeric(UW_PAR_hanging_Avg), 
         Air_Temp_Avg = as.numeric(Air_Temp_Avg), 
         Temp_DOdeep_Avg = as.numeric(Temp_DOdeep_Avg), 
         OSat_DOdeep_Avg = as.numeric(OSat_DOdeep_Avg), 
         Conc_mgL_DOdeep_Avg = as.numeric(Conc_mgL_DOdeep_Avg), 
         Conc_ppm_DOdeep_Avg = as.numeric(Conc_ppm_DOdeep_Avg),
         Temp_DOshallow_Avg = as.numeric(Temp_DOshallow_Avg), 
         OSat_Dshallow_Avg = as.numeric(OSat_Dshallow_Avg),
         Conc_mgL_DOshallow_Avg = as.numeric(Conc_mgL_DOshallow_Avg),
         Conc_ppm_DOshallow_Avg = as.numeric(Conc_ppm_DOshallow_Avg),
         BattV_Min = as.numeric(BattV_Min))


for(i in loopy) {
  
  var=names(site)[i]
  
  h <- site[,i]
  print(
    ggplot(site, aes(TIMESTAMP, h)) + 
      geom_point() + 
      labs(title = var) + 
      xlab("Date") + ylab(paste(i))
  )
} |> 
  ggsave(filename = paste("plots/WLBBB/WLBBB",i,".png", sep = "" ))



LFBB1min <- read.delim(file = "data/LFBB/Data/TOA5_40106.LF1min.dat", header = TRUE, sep = ",", 
                       skip = 1) |>
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) |> 
  mutate(stage_Avg = as.numeric(stage_Avg), 
         ablation_Avg = as.numeric(ablation_Avg))

ggplot(LFBB1min, aes(TIMESTAMP)) +
  geom_point(aes(y=stage_Avg, color = "red")) +
  geom_point(aes(y=ablation_Avg, color = "blue"))


LFBB <- site

ggplot(LFBB, aes(x = TIMESTAMP)) + 
  geom_point(aes(y = BattV_Min), fill = "red") + 
  geom_point(aes(y = stage_Avg), fill = "blue")

  








