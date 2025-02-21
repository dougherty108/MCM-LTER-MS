###### Data Check ######
# load packages

library("tidyverse")
library("scales")
library(lubridate)

# Load file

site <- read.delim(file = "data/FRLM/FRLM_CR1000X_FRLM15.dat", sep = ",",
                   skip = 1)

site <- site[-c(1,2),]

site <- site |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) |> 
  mutate(
    AirT3m = as.numeric(AirT3m),
    RH3m = as.numeric(RH3m),
    RH3m_AirT = as.numeric(RH3m_AirT),
    SwRadIn = as.numeric(),
  )
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
  ggsave(filename = paste("plots/FRLM",var,".png", sep = "" ))


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
