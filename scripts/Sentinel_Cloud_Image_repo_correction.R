##

library(tidyverse)

s_total <- read_csv("data/satellite image repos/Landsat 8 Image Repository.csv") |> 
  filter(`Cloud Free (Y/N/P)` == "Y" &
         `Coverage of lakes` == "All" | `Coverage of lakes` == "LF" &
           `Snow Cover (Y/N/P)` == "N")

s_filt <- read_csv("data/GEE/Sentinel Image Repo.csv") |> 
  drop_na(`Cloud Free (Y/N/P)`)

#filter join, only save values in s_filt that are found in s_total

output <- s_total |> 
  semi_join(s_filt, by = join_by(`File ID`))

# do another join this is sort of tortured but the process works
tosave <- s_filt |> 
  inner_join(output, by = join_by(`File ID`)) |> 
  filter(`Cloud Free (Y/N/P)` == "Y" &
           `Coverage of lakes` == "All" &
           `Snow Cover (Y/N/P)` == "N") |> 
  print(n = 272)

write_csv(tosave, "data/GEE/Sentinel_Image_repo_corrected.csv")
