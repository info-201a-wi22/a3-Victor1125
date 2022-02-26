## collection of R packages for D.S
library(tidyverse) 
## A new package which provides a set of tools for efficiently manipulating datasets in R
library(dplyr)
## A R package dedicated to Data Visualization
library(ggplot2)
## A package for creating interactive, publication-quality graphs
library(plotly)
## A package that contains a lot of outlines of continents, countries, states, and counties that have been with R
library(maps)
## A package that contains demographic data on the United States at the county and state levels
library(usdata)

## Data Loading
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Filtered Data for jail rates data
entire_country_jail_rate <- incarceration %>% mutate(total_jail_rate = round(((total_jail_pop / total_pop)*100),2)) %>% 
  mutate(black_jail_rate = round(((black_jail_pop / black_pop_15to64)*100),2)) %>% mutate(white_jail_rate = round(((white_jail_pop / white_pop_15to64)*100),2)) %>% 
  select (year, region, total_jail_rate, white_jail_rate, black_jail_rate)

## Data of jail rates in south region
south_states <- entire_country_jail_rate %>% filter(region == "South") %>% slice(31:49)
mean_south_total <- round(mean(south_states$total_jail_rate),2)
mean_south_white <- round(mean(south_states$white_jail_rate),2)
mean_south_black <- round(mean(south_states$black_jail_rate),2)

## Black jail rate over white jail rate data in south region
black_white_diff_south <- south_states %>% mutate(black_white_diff = round((black_jail_rate / white_jail_rate),2)) %>% 
                                                    select(year, region, black_white_diff)

## Data of jail rates in west region
west_states <- entire_country_jail_rate %>% filter(region == "West") %>% slice (31:49)
mean_west_total <- round(mean(west_states$total_jail_rate),2)
mean_west_white <- round(mean(west_states$white_jail_rate),2)
mean_west_black <- round(mean(west_states$black_jail_rate),2)

## Black Jail Rate / White Jail Rate (West)
black_white_diff_west <- west_states %>% mutate(black_white_diff = round((black_jail_rate / white_jail_rate),2)) %>% 
  select(year, region, black_white_diff)

## Data of jail rates in south region
midwest_states <- entire_country_jail_rate %>% filter(region == "Midwest") %>% slice(31:49)
mean_midwest_total <- round(mean(midwest_states$total_jail_rate),2)
mean_midwest_white <- round(mean(midwest_states$white_jail_rate),2)
mean_midwest_black <- round(mean(midwest_states$black_jail_rate),2)

## Black Jail Rate / White Jail Rate (Midwest)
black_white_diff_midwest <- midwest_states %>% mutate(black_white_diff = round((black_jail_rate / white_jail_rate),2)) %>% 
  select(year, region, black_white_diff)

## Mean value of black jail rate over white jail rate in different regions
mean_south <- round(mean(black_white_diff_south$black_white_diff),2)
mean_west <- round(mean(black_white_diff_west$black_white_diff),2)
mean_midwest <- round(mean(black_white_diff_midwest$black_white_diff),2)

## Combined data of black jail rate over white jail rate data in different regions
black_white_diff_combined <- rbind(black_white_diff_west, black_white_diff_south, black_white_diff_midwest)

## Comparison of black jail rate over white jail rate data in different regions (Trends over-time chart)
ggp_total <- ggplot(black_white_diff_combined, aes(x = year, y = black_white_diff, color = region)) + geom_line()
print(ggp_total + labs(title = "Black Over White Jail Rate By Regions (2000-2018)", y = "Black over White Jail Rate", x= "Year", color = "Region"))

## Change in jail rate of black people in different regions during 2000-2018 (Variable Comparison Chart)
black_south <- south_states %>% select(year, region, black_jail_rate)
black_west <- west_states %>% select(year, region, black_jail_rate)
black_midwest <- midwest_states %>% select(year, region, black_jail_rate)
black <- rbind(black_south, black_west, black_midwest)

bar_black <- ggplot(black, aes(x = year, y = black_jail_rate, fill = region)) + geom_col(position = "dodge")
print(bar_black + labs(title = "Change in Jail Rate of Black in Different Regions (2000-2018)", y = "Black Jail Rate (%)", x = "Year", color = "Region"))

## Black jail rate over total jail rate in 2018 in the United States (MAP)
data_map <- incarceration %>% mutate(white_jail_rate = round(((white_jail_pop / white_pop_15to64)*100),2)) %>%  
  mutate(black_jail_rate = round((black_jail_pop / black_pop_15to64)*100),2) %>%
  select (year, state, county_name, white_jail_rate, black_jail_rate, fips) %>% filter(year == 2018)

data_map <- data_map %>% mutate(black_over_white = round(((black_jail_rate / white_jail_rate)*100),2)) %>% 
  select(state, county_name, black_over_white, fips)

map_shapes <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>% left_join(county.fips, by="polyname")

map_data <- map_shapes %>% left_join(data_map, by="fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

final_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y =lat, group = group, fill = black_over_white),
    color = "gray", size = 0.3
  ) + 
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$black_over_white)), na.value = "white", low = "yellow", high = "red") + blank_theme +
  ggtitle("Black Jail Rate over White Jail Rate in the United States") +
  guides(fill=guide_legend(title="Black over white jail rate"))

final_map
