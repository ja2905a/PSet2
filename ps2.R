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

#5. Report the number of urban areas that satisfy the conditions below.
#Either write your code inline or echo the code that generated the answer.   

#a. Scored `High` or above for greenspace in 2015.  

#b. Scored `Exceptionally Low` at any point in the years covered.  

#c. Urban areas in arid climate that became greener
#(as measured by annual weighted average) from 2010 to 2020. 

a <- gspace |>
  filter(indicator_2015 == "High" |
           indicator_2015 == "Very High" |
           indicator_2015 == "Exceptionally High")
count(a)

b <- gspace |>
  filter(indicator_2010 == "Exceptionally Low" |
           indicator_2015=="Exceptionally Low" |
           indicator_2020 == "Exceptionally Low" |
           indicator_2021 == "Exceptionally Low")
count(b)

c <- gspace |>
  filter(Climate_region == "Arid",
         annual_weight_avg_2020 > annual_weight_avg_2010)
count(c)

conditional <- gspace |>
    filter(indicator_2015 == "High" |
             indicator_2015 == "Very High" |
             indicator_2015 == "Exceptionally High",
           indicator_2010 == "Exceptionally Low" |
             indicator_2020 == "Exceptionally Low" |
             indicator_2021 == "Exceptionally Low",
           annual_weight_avg_2020 > annual_weight_avg_2010,
           climate_region == "Arid")
count(conditional)  

#6.  How many urban areas became less green (measured by annual average)
#from 2010 to 2021? Were these changes concentrated in a particular
#geographic or climate region? Explain (with evidence, of course)

LessGreen <- gspace |>
  filter(annual_avg_2010 > annual_avg_2021)
count(LessGreen)


MajorGeoRegion <-
  LessGreen |>
  group_by(Major_Geo_Region) |>
  summarise(
  obs = n())
MajorGeoRegion |>
  mutate(MajorGeoRegionPer = obs / sum(obs) * 100)

ClimRegion <-
  LessGreen |>
  group_by(Climate_region) |>
  summarise(
    obs = n())
ClimRegion |>
  mutate(ClimRegionPer = obs / sum(obs) * 100)


#7.  Present a histogram showing the change in greenspace
#(annual average) from 2010 to 2021. Note that you will
#need to create a new variable equal to this difference.

Gspace7 <-
  gspace |>
   mutate(GspaceDiff = annual_avg_2021 - annual_avg_2010)

hist(Gspace7$GspaceDiff,
     main = "Histogram of Greenspace Change between 2010 and 2021",
     xlab = "Greenspace Change",
     breaks = 30)


ggplot(data = gspace,
       mapping= aes(x = annual_weight_avg_2021,
                    y = annual_weight_avg_2010,
                    xmin = 0)) +
  geom_point(color=ifelse(gspace$annual_avg_2021 < gspace$annual_avg_2010,
                          "red", "black")) +
  labs(x = "Population-Weighted Greenspace 2021",
       y = "Population-Weighted Greenspace 2010",
       title = "Greenspace Plot") +
  geom_abline(intercept = 0.0,
              slope = 1,
              color = "blue",
              size = 1)

  

