
# Print total confirmed cases in the top left of timeline.country facet plot
# Filter by clcking on map










library(tidyverse)
library(leaflet)


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


pop <- read_csv('https://raw.githubusercontent.com/gustavo-hideo/wandata/master/data/global_population_density.csv',
                skip=2,
                col_names = c('n', 'country', 'year', 'series', 'value', 'n2', 'n3')) %>% 
  select(country, year, series, value) %>% 
  filter(year == 2019)


###########################################







###########################################
# Merging the 3 datasets
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
  mutate(Date = lubridate::mdy(Date))

#covid <- covid.l %>% 
  

###########################################




###########################################
# SHP data (for map | long, lat)
shp <- rgdal::readOGR("./shp/TM_WORLD_BORDERS_SIMPL-0.3.shp")

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

###########################################




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
                   #Country == 'Republic of Korea' ~ 'Korea, Democratic People\'s Republic of',
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
mypal.ref <- colorQuantile(c("#ffffcc", "#e60000"), na.color = "#FFFFEE", n = 10, domain = unique(covid.map.c$Current)) #the domain is "unique" so that the function creates unique breaks

## merging SHP and data
covid.map.sp <- tigris::geo_join(shp, covid.map.c, "NAME", "Country")


###########################################
## Summaries
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


title <- paste("<span style=align:center;><font size=6><strong>", "COVID-19 ", "</strong></font size></span>")
conf <- paste("<font size=5>", "Confirmed:", "</font size>", "<font size=6><font color=#0074D9>", format(as.numeric(total.Confirmed$total), big.mark = ","), "</font color>", "|", "</font size>")
death <- paste("<font size=5>", "Deaths:", "</font size>", "<font size=6><font color=#FF4136>", format(as.numeric(total.Deaths$total), big.mark = ","), "</font color></font size>")


###########################################











##########################################
## Top 8
top8 <- covid.map %>% 
  arrange(desc(Current)) %>% 
  select(Country) %>% 
  head(8)

covid.map.8 <- covid %>% 
  group_by(Country, status, Date) %>% 
  summarize(Cases = sum(Cases)) %>% 
  ungroup() %>% 
  select(Country, status, Cases, Date) %>% 
  filter(Country %in% top8$Country)




##############
## Global variables
##############

my.theme <- theme(panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  legend.position = 'none',
                  axis.text.x = element_text(angle=90, hjust=1))


my.colors <- scale_color_manual(values=c('#0074D9','#FF4136')) #blue, red, green


countries <- covid %>% 
  select(Country) %>% 
  unique() %>% 
  arrange(Country)
countries <- as.vector(countries$Country)














