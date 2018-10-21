library(tidyverse)
library(mapproj)
library(maps)

raw <- read_csv("/Users/HM/Dropbox/DropDok/Uni/18:19WS/Data Science/Projekt Data/WGI_csv/WGIData.csv")

raw_gdp <- read_csv("/Users/HM/Dropbox/DropDok/Uni/18:19WS/Data Science/Projekt Data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10181085/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10181085.csv",
                    skip = 4)

raw_lit <- read_csv("/Users/HM/Dropbox/DropDok/Uni/18:19WS/Data Science/Projekt Data/API_SE.ADT.LITR.ZS_DS2_en_csv_v2_10181228/API_SE.ADT.LITR.ZS_DS2_en_csv_v2_10181228.csv",
                    skip = 4)

# Create Working Spreadsheets
data <- raw

data_gdp <- raw_gdp

data_lit <- raw_lit

# Governance Indicator Data
tidy <- data %>%
    filter(grepl("Estimate", `Indicator Name`)) %>%
    rename(country = `Country Name`, country_code = `Country Code`, indicator = `Indicator Name`) %>%
    select(-`Indicator Code`, -`X23`) %>%
    gather(year, value, -country, -country_code, -indicator)

tidy_2 <- tidy %>%
    spread(indicator, value) %>%
    rename(control_of_corruption = `Control of Corruption: Estimate`, 
           government_effectiveness = `Government Effectiveness: Estimate`,
           political_stability = `Political Stability and Absence of Violence/Terrorism: Estimate`,
           rule_of_law = `Rule of Law: Estimate`,
           regulatory_quality = `Regulatory Quality: Estimate`,
           voice_accountability = `Voice and Accountability: Estimate`)

# GDP Data

tidy_gdp <- data_gdp %>%
  rename(country_code = `Country Code`) %>%
  gather(year, gdp_value, -country_code, -`Country Name`, -`Indicator Name`, -`Indicator Code`) %>%
  select("country_code", "year", "gdp_value") %>%
  filter(year >= 1996, year <= 2016)

# Literacy Data

tidy_lit <- data_lit %>%
  rename(country_code = `Country Code`) %>%
  gather(year, lit_rate, -country_code, -`Country Name`, -`Indicator Name`, -`Indicator Code`) %>%
  select("country_code", "year", "lit_rate") %>%
  filter(year >= 1996, year <= 2016)

# tidy_gdp wird zu tidy 2 gejoined
joined_1 <- left_join(tidy_2, tidy_gdp, by = c("country_code", "year"))

joined_2 <- left_join(joined_1, tidy_lit, by = c("country_code", "year")) %>%
  mutate(date = as.Date(paste0(year,"-01-01")))


# Country Corruption Chart

joined_2 %>%
  filter(country_code == "ITA") %>% 
  ggplot() +
    aes(date, control_of_corruption) + 
    geom_line() +
    xlab("Year") +
    ylab("Impact of Corruption") +
    ylim(-2.5,2.5)

# Barplot mit 3 Ländern im Vergleich
joined_2 %>%
  filter(country_code %in% c("DEU", "ITA", "AFG"), date == "2014-01-01") %>%
  ggplot() +
  aes(country_code, control_of_corruption, fill = control_of_corruption) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylim(-2.5,2.5) +
  scale_fill_gradient(low = "red", high = "green")

# Versuch Spiderweb Plot    
joined_2 %>%
  filter(country_code == "AFG", date == 2014-01-01) %>%
  ggplot() +
    aes(control_of_corruption, government_effectiveness, political_stability, regulatory_quality, rule_of_law, voice_accountability) +
    geom_polygon()


#Map Plot
joined_2 %>%
  ggplot() +
    aes(control_of_corruption) +
    π + coord_map()

