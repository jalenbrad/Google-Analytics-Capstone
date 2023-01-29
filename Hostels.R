## Checking packages

library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

##Importing Hostel file 

Japan_Host <- read.csv("C:/Users/Public/Documents/archive/Hostel.csv")

## Looking at Hostel

str(Japan_Host)

## Confirming total number of rows

rowtotal <- sum(
  nrow(Japan_Host)
)
  
print(rowtotal)

## Removing NA data and duplicates

Japan_Host <- drop_na(Japan_Host)

Japan_Host_no_duplicates <- Japan_Host[!duplicated(Japan_Host$hostel.name),]
print(paste("Removed", nrow(Japan_Host) - nrow(Japan_Host_no_duplicates), "duplicated rows"))

## Getting favorite city

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

city_mode <- getmode(Japan_Host$City)

print(city_mode)

## Getting City with average ratings across the board

Japan_Host$City <- ordered(Japan_Host$City, levels = c("Tokyo", "Osaka", "Kyoto", "Fukuoka-City", "Hiroshima"))

Japan_Host %>% 
  group_by(City) %>% 
  summarise(num_of_hostel = n(), avg_price = mean(price.from), avg_sum = mean(summary.score), avg_atm = mean(atmosphere), avg_clean = mean(cleanliness), avg_fac = mean(facilities), avg_loc = mean(location.y), avg_sec = mean(security), avg_staff = mean(staff), avg_val = mean(valueformoney)) %>% 
  arrange(City)

##Visualizations - Number of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(hostel_count = n()) %>% 
  ggplot(aes(x = City, y = hostel_count, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = hostel_count, vjust = -0.25))

##Visualizations - Number of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(avg_price = round(mean(price.from)), digits = 4) %>% 
  ggplot(aes(x = City, y = avg_price, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = avg_price, vjust = -0.25))

##Visualizations - Average score of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(summary.score = signif(mean(summary.score), digits = 3)) %>% 
  ggplot(aes(x = City, y = summary.score, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = summary.score, vjust = -0.25))

##Visualizations - Average atmosphere of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(atmosphere = signif(mean(atmosphere), digits = 3)) %>% 
  ggplot(aes(x = City, y = atmosphere, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = atmosphere, vjust = -0.25))

##Visualizations - Average cleanliness of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(cleanliness = signif(mean(cleanliness), digits = 3)) %>% 
  ggplot(aes(x = City, y = cleanliness, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = cleanliness, vjust = -0.25))

##Visualizations - Average facilities rating of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(facilities = signif(mean(facilities), digits = 3)) %>% 
  ggplot(aes(x = City, y = facilities, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = facilities, vjust = -0.25))

##Visualizations - Average location rating of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(location.y = signif(mean(location.y), digits = 3)) %>% 
  ggplot(aes(x = City, y = location.y, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = location.y, vjust = -0.25))

##Visualizations - Average security rating of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(security = signif(mean(security), digits = 3)) %>% 
  ggplot(aes(x = City, y = security, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = security, vjust = -0.25))

##Visualizations - Average staff rating of hostels by city

Japan_Host %>% 
  group_by(City) %>% 
  summarise(staff = signif(mean(staff), digits = 3)) %>% 
  ggplot(aes(x = City, y = staff, fill = City)) + 
  geom_col(position = "dodge") + geom_text(aes(label = staff, vjust = -0.25))