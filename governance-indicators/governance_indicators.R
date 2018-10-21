library(tidyverse)
library(mapproj)
library(maps)
library(ggplot2)
library(readr)
library(rworldmap)
library(RColorBrewer)
library(plotly)


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

#control_of_corruption und country_code sind Variable (Auch in ggtitle!)!
joined_2 %>%
  filter(country_code == "ITA") %>% 
  ggplot() +
    aes(date, control_of_corruption) + 
    geom_line() +
    ggtitle("Control of Corruption in Italy") +
    xlab("Year") +
    ylab("Control of Corruption") +
    ylim(-3.5,2.5) +
    theme_minimal()

# Barplot mit 3 Ländern im Vergleich

# 3 Länder und Date sind Varable (auch in ggtitle!)
joined_2 %>%
  filter(country_code %in% c("DEU", "ITA", "DNK"), date == "2014-01-01") %>%
  ggplot() +
  aes(country_code, control_of_corruption, fill = control_of_corruption) +
  geom_bar(stat = "identity") +
  ggtitle("Comparison of Control of Corruption in 2014") +
  coord_flip() +
  #ylim(-3.5,3.5) +
  scale_y_continuous(limits = c(-3.5, 3.5)) +
  #scale_y_continuous(breaks = c(-3.5, -2, 0, 2, 3.5)) +
  # guides --> delete legend
  guides(fill = FALSE) +
  #Achsenbeschriftung entfernen
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_gradient(low = "darkred", high = "green") +
  theme_minimal()


# Spiderweb Plot
# year und country_code sind Variable!

spi <- joined_2 %>%
  filter(year == 2016, country_code == "DEU")
plot_ly(
    type = 'scatterpolar',
    r = c(spi$control_of_corruption, spi$government_effectiveness, spi$political_stability, spi$rule_of_law, spi$regulatory_quality, spi$voice_accountability),
    theta = c('Control of Corruption','Government Effectiveness','Political Stability', 'Rule of Law', 'Regulatory Quality', 'Voice and Accountability'),
    fill = 'toself'
    ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(-3.5,2.5)
        ) 
    ),
    showlegend = F
  )




#Map Plot
# year und control_of_corruption sind Variable!

map_joined(joined_2)
sPDF <- joined_2 %>%
  filter(year == 2009) %>%
  joinCountryData2Map(
    joinCode = "ISO3",
    nameJoinColumn = "country_code") %>%
  mapCountryData( 
               nameColumnToPlot = "control_of_corruption",
               # Je nach gewählter Variable!
               mapTitle = "Control of Corruption",
               catMethod = "diverging",
               numCats = 5,
               colourPalette = c("darkred", "red", "yellow", "chartreuse", "chartreuse4"),
               oceanCol = "aliceblue",
               missingCountryCol = "white")

