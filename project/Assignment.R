install.packages("tidyverse")
library(tidyverse)
install.packages("maps")
install.packages("plotly")
library(plotly)
map_world <- map_data("world")
library("maps")

#Datasets
metadata <- read_csv("unicef_metadata.csv")
indicator <- read_csv("unicef_indicator_2.csv")


#Join
data_join <- full_join(metadata, indicator)
data_join <- full_join(metadata, indicator, by =c("country" = "country", "year" = "time_period"))
top_10_gdp_final <- full_join(indicator, top_10_gdp_countries)

world_gdp <- full_join(country_gdp, map_world, by = c("country" = "region"))

#final data
data_join <- metadata %>%
  full_join(indicator)  

#Scatterplot
ggplot(metadata, aes(x = year, y = `Life expectancy at birth, total (years)`)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy at Birth Over Time",
       x = "Year",
       y = "Life Expectancy at Birth (years)")
       
#Bar Chart
# Group by country and calculate total GDP
country_gdp <- data_join %>%
  group_by(country) %>%
  summarise(total_gdp = sum(`GDP per capita (constant 2015 US$)`))

# Select the top 10 countries based on GDP
top_10_gdp_countries <- country_gdp %>%
  top_n(10, total_gdp)

# Filter data_join to include only the top 10 countries by GDP
filtered_data_join <- data_join %>%
  filter(country %in% top_10_gdp_countries$country)
  
ggplot(top_10_gdp_countries, aes(x = country, y = total_gdp)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "GDP per Capita by Country (Year: 2020)",
       x = "Country",
       y = "Total GDP")

#Timeseries
ggplot(metadata) +
  (aes(year, 'population, total', color = country)) +
  geom_line()


#Map
world_gdp <- full_join(country_gdp, map_world, by = c("country" = "region"))
ggplot(world_gdp) +
  aes(x = long, y = lat, group = group, fill = `total_gdp`) +
  geom_polygon()

options(scipen = 999)

