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


sum(table$obs)
  

#4. In a couple of sentences and with reference to a well-formatted tabulation,
# describe the greenspace classification scores for urban areas in 2021.

#WARNING: I know this code is bad. I am sorry.

AnnualAvg21 = summary(gspace$annual_avg_2021)
PeakNVDI21 = summary(gspace$peak_NDVI_2021)
PeakWeight21 = summary(gspace$peak_weight_2021)
WeightAvg21 = summary(gspace$annual_weight_avg_2021)

summary <- bind_rows(AnnualAvg21, PeakNVDI21, PeakWeight21, WeightAvg21)

summary21 <- summary[ -c(2, 5, 7)]

summary21$Var <- c("AnnualAvg21",
                        "PeakNVDI21",
                        "PeakWeight21",
                        "WeightAvg21")

knitr::kable(summary21)

