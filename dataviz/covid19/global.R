
# Print total confirmed cases in the top left of timeline.country facet plot
# Filter by clcking on map










library(tidyverse)
library(leaflet)
library(lubridate)


###########################################
## Loading data sets
Confirmed <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>% 
  rename('State'=`Province/State`,
         'Country'=`Country/Region`) %>% 
  mutate(status = 'Confirmed') 


Deaths <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>% 
  rename('State'=`Province/State`,
         'Country'=`Country/Region`) %>% 
  mutate(status = 'Deaths')


## US
#states coordinates
#states <- read_csv('https://raw.githubusercontent.com/gustavo-hideo/wandata/master/data/us_states.csv') %>% 
 # select(name, latitude, longitude)


Confirmed.us <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv') %>% 
  rename('State'=`Province_State`,
         'Country'=`Country_Region`,
         "County"=`Admin2`,
         "Long"=Long_) %>%
  mutate(status = 'Confirmed') %>% 
  select(-UID:-FIPS, -Country, -Combined_Key)# %>% 
  #left_join(states, by=c('State'='name'))


Deaths.us <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv') %>% 
  rename('State'=`Province_State`,
         'Country'=`Country_Region`,
         "County"=`Admin2`,
         "Long"=Long_) %>%
  mutate(status = 'Deaths') %>% 
  select(-UID:-FIPS, -Country, -Combined_Key)# %>% 
  #left_join(states, by=c('State'='name'))




#population is only in the Deaths table. Copying it to the COnfirmed table as well.
pop.us <- Deaths.us %>% 
  select(County, State, Population) 
  
Confirmed.us <- Confirmed.us %>% 
  left_join(pop.us, by=c("County"="County", "State"="State"))


  


###########################################







###########################################
# Merging the 2 datasets
covid <- rbind(Confirmed, Deaths)  # Merging datasets
n_col <- ncol(covid)  # Number of columns
Current <- as.data.frame(covid[, n_col-1])  # Creating the current number of cases using the last date
colnames(Current) <- "Current"
covid <- cbind(covid, Current) # Addig Current to the dataset covid
# Pivoting the dataset to have the dates as a column and #cases as another column
covid <- pivot_longer(covid,
             cols=names(covid[5]) : names(covid[n_col-1]),
             names_to='Date',
             values_to='Cases')%>% 
  mutate(Date = lubridate::mdy(Date),
         Country = ifelse(Country == 'US', 'United States', Country))

  

## US
covid.us <- rbind(Confirmed.us, Deaths.us)  # Merging datasets
n_col.us <- ncol(covid.us)  # Number of columns
Current.us <- as.data.frame(covid.us[, n_col.us - 2])  # Creating the current number of cases using the last date
colnames(Current.us) <- "Current"
covid.us <- cbind(covid.us, Current.us) # Addig Current to the dataset covid
# Pivoting the dataset to have the dates as a column and #cases as another column
covid.us <- pivot_longer(covid.us,
                      cols=names(covid.us[5]) : names(covid.us[n_col.us-3]),
                      names_to='Date',
                      values_to='Cases')%>% 
  mutate(Date = lubridate::mdy(Date))

###########################################




###########################################
# SHP data (for map | long, lat)
shp <- rgdal::readOGR("./shp/TM_WORLD_BORDERS_SIMPL-0.3.shp")
shp.states <- rgdal::readOGR("./shp/states/s_11au16.shp")


###########################################





###########################################
# MAP

covid.map <- covid %>% 
  filter(status=="Confirmed") %>% 
  select(Country, Current) %>% 
  unique() %>% 
  group_by(Country) %>% 
  summarise(Current = sum(Current, na.rm = T)) %>% 
  ungroup()


# ## US
# covid.map.us.county <- covid.us %>%
#   filter(!is.na(Lat)) %>% 
#   select(County, State, status, Current, Lat, Long) %>%
#   unique()

covid.map.us.state <- covid.us %>%
  select(State, Current, status) %>%
  unique() %>%
  group_by(State, status) %>%
  summarise(Current = sum(Current, na.rm = T)) %>%
  ungroup()

covid.map.us.state.conf <- covid.map.us.state %>% 
  filter(status=="Confirmed") %>% 
  mutate(Current.scale = BBmisc::normalize(Current, method='range', range=c(0,20)))

covid.map.us.state.death <- covid.map.us.state %>% 
  filter(status=="Deaths") %>% 
  mutate(Current.scale = BBmisc::normalize(Current, method='range', range=c(0,20)))

  

###########################################
## States datatable
covid.us.dt <- covid.map.us.state %>% 
  pivot_wider(names_from = status, values_from = Current) %>% 
  arrange(desc(Confirmed))



###########################################
# Countries to clean
clean.shp <- as.data.frame(shp$NAME)
colnames(clean.shp) <- 'Country'
clean.shp$y <- 0

countries <- covid.map %>% select(Country) %>% mutate(x=0)

to.clean <- countries %>% 
  left_join(clean.shp, by='Country') %>% 
  filter(is.na(y)) %>% 
  select(Country)


#Fixing
covid.map.c <- covid.map %>% 
  mutate(Country = case_when(Country == 'Bahamas, The' ~ 'Bahamas',
                   Country == 'Brunei' ~ 'Brunei Darussalam',
                   Country == 'Cabo Verde' ~ 'Cape verde',
                   Country == 'Channel Islands' ~ 'France',
                   Country %in% c('Congo (Brazzaville)', 'Congo (Kinshasa)') ~ 'Congo',
                   Country == 'Czechia' ~ 'Czech Republic',
                   Country == 'Holy See' ~ 'Holy See (Vatican City)',
                   Country == 'Hong Kong SAR' ~ 'Hong Kong',
                   Country == 'Iran' ~ 'Iran (Islamic Republic of)',
                   Country == 'Macao SAR' ~ 'Macau',
                   Country == 'Mainland China' ~ 'China',
                   Country == 'Moldova' ~ 'Republic of Moldova',
                   Country == 'North Macedonia' ~ 'The former Yugoslav Republic of Macedonia',
                   Country == 'occupied Palestinian territory' ~ 'Palestine',
                   Country %in% c('South Korea','Republic of Korea', 'Korea, South') ~ 'Korea, Republic of',
                   Country == 'Russian Federation' ~ 'Russia',
                   Country == 'St. Martin' ~ 'Saint Martin',
                   Country %in% c('Taipei and environs', 'Taiwan*') ~ 'Taiwan',
                   Country == 'East Timor' ~ 'Timor-Leste',
                   Country == 'UK' ~ 'United Kingdom',
                   Country == 'US' ~ 'United States',
                   Country == 'Vatican City' ~ 'Holy See (Vatican City)',
                   Country == 'Vietnam' ~ 'Viet Nam',
                   TRUE ~ Country)
         )


# Testing
countries <- covid.map.c %>% select(Country) %>% mutate(x=0)

to.clean <- countries %>% 
  left_join(clean.shp, by='Country') %>% 
  filter(is.na(y)) %>% 
  select(Country)

###########################################




###########################################
## Map colors
mypal.ref <- colorQuantile(c("#ffffcc", "#e60000"), na.color = "#FFFFEE", n = 40, domain = unique(covid.map.c$Current)) #the domain is "unique" so that the function creates unique breaks
mypal.ref.state.conf <- colorQuantile(c("#ffffcc", "#CD5C5C"), na.color = "#FFFFEE", n = 60, domain = unique(covid.map.us.state.conf$Current))
mypal.ref.state.death <- colorQuantile(c("#ffffcc", "#CD5C5C"), na.color = "#FFFFEE", n = 40, domain = unique(covid.map.us.state.death$Current))

## merging SHP and data
covid.map.sp <- tigris::geo_join(shp, covid.map.c, "NAME", "Country")
covid.map.sp.us.death <- tigris::geo_join(shp.states, covid.map.us.state.death, "NAME", "State")
covid.map.sp.us.conf <- tigris::geo_join(shp.states, covid.map.us.state.conf, "NAME", "State")


###########################################



# ############
# 
# covid.map.sp.us.conf %>%
#   leaflet() %>%
#   setView(lat=37, lng=-97.7129, zoom=3.3) %>% 
#   addProviderTiles("CartoDB.Positron"#,
#                    #options = providerTileOptions(
#                      #minZoom = 1, maxZoom = 12, noWrap = T)
#   ) %>%
#   addPolygons(
#     fillColor = ~mypal.ref.state.conf(covid.map.sp.us.conf$Current),
#     stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
#     weight = 1,
#     popup = paste0("State: ", as.character(covid.map.sp.us.conf$State), "<br>",
#                    "Total: ", as.character(covid.map.sp.us.conf$Current))
#   )









###########################################
## New cases/deaths

new <- covid %>% 
  select(Country, status, Date, Cases) %>% 
  group_by(Country, status, Date) %>% 
  mutate(Cases = sum(Cases, na.rm = T)) %>% 
  group_by(Country, status) %>% 
  mutate(new = abs(Cases - lag(Cases, order_by=Date)),
         row = row_number()) %>% 
  select(-Cases)%>% 
  ungroup() %>% 
  unique() %>% 
  pivot_wider(id_cols = c(row, Country, Date),
              names_from = status,
              values_from = new) %>% 
  select(-row)


###########################################









###########################################
## Scatterplot

covid.sum <- covid %>% 
  select(Country, Current, status) %>% 
  unique() %>% 
  group_by(Country, status) %>% 
  summarise(Current = sum(Current, na.rm = T)) %>% 
  pivot_wider(names_from = status, values_from = Current) %>% 
  ungroup() %>% 
  unique()


###########################################




## Summary US
covid.sum.us <- covid.sum %>% 
  filter(Country == "United States")













###########################################

## Summaries

## General tab

total.Confirmed <- covid %>%
  select(Country, status, State, Current) %>%
  filter(status=='Confirmed') %>%
  unique() %>%
  summarise(total = sum(Current, na.rm = T))


total.Deaths <- covid %>%
  select(Country, status, State, Current) %>%
  filter(status=='Deaths') %>%
  unique() %>%
  summarise(total = sum(Current, na.rm = T))


last.date <- max(covid$Date)
mon <- month(last.date, label=T, abbr=T)
day <- day(last.date)
year <- year(last.date)

title <- paste0("<span style=align:center;><font size=6><strong>", "COVID-19 ", "</strong></font size></span>",
               "<font size=3>as of: ", mon, " ", day, ", ", year, "</font size>")
conf <- paste("<font size=5>", "Confirmed:", "</font size>", "<font size=6><font color=#6495ed>", format(as.numeric(total.Confirmed$total), big.mark = ","), "</font color>", "|", "</font size>")
death <- paste("<font size=5>", "Deaths:", "</font size>", "<font size=6><font color=#CD5C5C>", format(as.numeric(total.Deaths$total), big.mark = ","), "</font color></font size>")


## US
title.us <- paste0("<span style=align:center;><font size=6><strong>", "United States ", "</strong></font size></span>",
                "<font size=3>as of: ", mon, " ", day, ", ", year, "</font size>")
conf.us <- paste("<font size=5>", "Confirmed:", "</font size>", "<font size=6><font color=#6495ed>", format(as.numeric(covid.sum.us$Confirmed), big.mark = ","), "</font color>", "|", "</font size>")
death.us <- paste("<font size=5>", "Deaths:", "</font size>", "<font size=6><font color=#CD5C5C>", format(as.numeric(covid.sum.us$Deaths), big.mark = ","), "</font color></font size>")



###########################################












##########################################
# ## Top 8
 top8 <- covid.map %>% 
   arrange(desc(Current)) %>% 
   select(Country) %>% 
   head(8)


# ## Top 8 states
top8.us <- covid.us.dt %>% 
  arrange(desc(Confirmed)) %>% 
  select(State) %>% 
  head(8)
# 
# covid.map.8 <- covid %>% 
#   group_by(Country, status, Date) %>% 
#   summarize(Cases = sum(Cases)) %>% 
#   ungroup() %>% 
#   select(Country, status, Cases, Date) %>% 
#   filter(Country %in% top8$Country)




##############
## Global variables
##############

my.theme <- theme(panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  legend.position = 'none',
                  axis.text.x = element_text(angle=90, hjust=1))


my.colors <- scale_color_manual(values=c('#6495ed','#CD5C5C')) #blue, red, green


countries <- covid %>%
  mutate(Country = ifelse(Country == 'US', 'United States', Country)) %>% 
  select(Country) %>% 
  unique() %>% 
  arrange(Country)
countries <- as.vector(countries$Country)


# US States
states <- covid.us %>% 
  select(State) %>% 
  unique()

states <- as.vector(states$State)
















