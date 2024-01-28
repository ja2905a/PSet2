library(tidyverse)

library(tidyverse)

gspace = read_csv('greenspace_data_share.csv') #added quotes around file name

table = 
  gspace |>
  group_by(Major_Geo_Region) |> #added pipe operator
summarise(
  obs = n(), #added comma
  avg = mean(annual_avg_2020), #added underscore in annual_avg_2020
  weighted_avg = mean(annual_weight_avg_2020) #added underscore in weighted_avg
)

knitr::kable(table) #performed function on table instead of gspace; pulled kable function from unloaded knitr package
