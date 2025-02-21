####### Bibliography #######

library(tidyverse)

#read file
bib <- read_csv("data/Bibliography/Bibliography.csv")

titles <- bib |> 
  select(Title)

#Filter titles to only include terms "ice", glaciers, "lake"

lakes <- titles |> 
  filter(grepl("lake"))


