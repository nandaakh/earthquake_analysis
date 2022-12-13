library(tidyverse)
library(dplyr)
library(lubridate)
library(ggpubr)
library(leaflet)

# import dataset from local storage
df <- read_csv("katalog_gempa.csv")
# get to know our dataset
summary(df)
lapply(df, class)
sum(complete.cases(df)) # checking on missing values
sum(complete.cases(df))/nrow(df)

# rename column names
df2 <- df %>%
  rename(
    dates = tgl,
    times = ot,
    loc = remark
  )

# add months and year column into dataset
df3 <- df2 %>%
  mutate(
    years = year(dates),
    months= case_when(
                                              month(df2$dates)==01 ~ "January",
                                              month(df2$dates)==02 ~ "February",
                                              month(df2$dates)==03 ~ "March",
                                              month(df2$dates)==04 ~ "April",
                                              month(df2$dates)==05 ~ "May",
                                              month(df2$dates)==06 ~ "June",
                                              month(df2$dates)==07 ~ "July",
                                              month(df2$dates)==08 ~ "August",
                                              month(df2$dates)==09 ~ "September",
                                              month(df2$dates)==10 ~ "October",
                                              month(df2$dates)==11 ~ "November",
                                              month(df2$dates)==12 ~ "December"
                                            ))
colnames(df3)

# remove unused column
df4 <- select(df3, -c(strike1, dip1, rake1, strike2, dip2, rake2))
sum(is.na(df4)) # checking if still there are missing datas
summary(is.na(df4))

# taking a moment if there are duplicate datas
quakes_df <- df4 %>%
  distinct()

# plotting magnitude of each earthquake over the years, since 2008
qplot(mag, data = quakes_df, bins = 100)

# take a look earthquakes happened since 2008
year_df <- quakes_df %>%
  select(years) %>%
  group_by(years) %>%
  count() %>%
  arrange()
head(year_df)

ggplot(data = quakes_df) +
  geom_bar(mapping = aes(x=years))+
  labs(title = "Earthquakes Incident Since 2008")

# Does earthquake occur based on specific month? script below show the outcome
# over the years since 2008
month_df <- quakes_df %>%
  select(months) %>%
  group_by(months) %>%
  count() %>%
  arrange()
head(month_df)

ggplot(data = quakes_df) +
  geom_bar(mapping = aes(x=months)) +
  labs(title = "Earthquakes Incident by Month Since 2008")

# Define perseason analysis to see pattern on each season
seasEq <- quakes_df %>%
  mutate(
    seasons = case_when(
      month(quakes_df$dates) == 10|
        month(quakes_df$dates) == 11|
        month(quakes_df$dates) == 12|
        month(quakes_df$dates) == 01|
        month(quakes_df$dates) == 02|
        month(quakes_df$dates) == 03 ~ "rainy season",
      month(quakes_df$dates) == 04|
        month(quakes_df$dates) == 05|
        month(quakes_df$dates) == 06|
        month(quakes_df$dates) == 07|
        month(quakes_df$dates) == 08|
        month(quakes_df$dates) == 09 ~ "dry season"
      
    )
  ) 

season_df <- seasEq %>%
  select(seasons) %>%
  group_by(seasons) %>%
  count() %>%
  arrange()
head(season_df)
  
ggplot(data = seasEq) +
  geom_bar(mapping = aes(x=seasons, fill=seasons)) +
  facet_wrap(~years) +
  labs(title = "Earthquakes Incident by Seasons")

# looking for pattern if we categorize magnitude level into dataframe
# Create a new column called 'mag_category' that divides the values
# in the 'mag' column into bins and assigns each value to a corresponding category
# (minor = 0-3.9 SR), (moderate = 4-5 SR), (major = 5.1-7.9 SR)
magEq <- quakes_df %>%
  mutate(mag_category = cut(
    mag,
    breaks = c(0, 3.9, 4.9, 5, 7.9),
    labels = c("minor", "moderate", "strong", "major")
  ))
head(magEq)

# plotting the finding (shown visual from data since 2008)
ggplot(magEq, aes(x = mag_category, fill = mag_category)) +
  geom_bar() +
  labs(title = "Number of earthquakes by magnitude category",
       x = "Magnitude Category",
       y = "Number of earthquakes") +
  facet_wrap(~years)

# let's try to filter the data with a magnitude of more than 5 on the Richter scale
# which indicates that an earthquake with that strength caused damage
# from light to severe, and even caused fatalities
# first, take a look if we mapping out the finding using leaflet function
mag_maj <- magEq %>%
  filter(mag_category == "major")
leaflet() %>%
  addTiles() %>%
  addCircles(data = mag_mod,
             radius = mag_mod$mag,
             color = "red",
             fillOpacity = 0.1,
             weight = 1)

# identify the type of earthquakes based on depth (shown data from 2008)
# Create a new column called 'eq_type' that divides the values
# in the 'depth' column into bins and assigns each value to a corresponding category
# (shallow = 0-60 km), (medium = 61-300 km), (deep = >300 km)
depEq <- quakes_df %>%
  mutate(eq_type = cut(
    depth,
    breaks = c(0, 61, 300, 750),
    labels = c("shallow", "medium", "deep")
  ))
head(depEq)

# visualize the finding
ggplot(depEq, aes(x = eq_type, fill = eq_type)) +
  geom_bar() +
  labs(title = "Number of earthquakes by Depth",
       x = "Earthquake Type",
       y = "Number of earthquakes") +
  facet_wrap(~years)

# let's take a look if we mapping out the finding using leaflet function
depShl <- depEq %>%
  filter(eq_type == "shallow")
leaflet() %>%
  addTiles() %>%
  addCircles(data = depShl,
             radius = depShl$depth,
             color = "red",
             fillOpacity = 0.1,
             weight = 1)

# taking moment to know what correlation between depth and magnitude
# and to check the influence between these two variable,
# 'stat_cor()' function use for calculates the correlation
# coefficient and adds it as a text label to the plot.
ggplot(quakes_df, aes(x = depth, y = mag)) +
  geom_point() +
  stat_cor()

# Group the data by 'lat' and 'long' and summarize the number of earthquakes in each location
quake_locations <- quakes_df %>%
  group_by(lat, lon) %>%
  summarize(n = n())

# Plot the data using a scatter plot
ggplot(quakes_df, aes(x = lat, y = lon)) +
  geom_point() +
  labs(title = "Number of earthquakes by location",
       x = "Latitude",
       y = "Longitude",
       size = "Number of earthquakes") +
  facet_wrap(~years)

# Create a map of the region
map_df <- filter(quakes_df, dates >= as.Date("2017-01-01") & dates <= as.Date("2022-10-31"))
leaflet() %>%
  addTiles() %>%
  addCircles(data = map_df,
             radius = map_df$mag,
             color = "blue",
             fillOpacity = 0.1,
             weight = 1)




