######################
# Phil Azar 
# Bourdain in Data
######################
library(tidyverse)
library(reshape2)
library(ggmap)
library(purrr)
library(ggplot2)
library(ggiraph)
library(readr)
library(stringr)
library(ggimage)
devtools::install_github('ellisp/ggflags')
library(ggflags)
install.packages('countrycode')
library(countrycode)
library(leaflet)

setwd('/Users/acazar/Desktop/blog/projects/bourdain/bourdain-travel-places')
######################
#  
# CLEAN DATA 
#
######################

data <- read_csv('./bourdain_travel_places.csv')
lat.long <- colsplit(data$coordinates, ",", names = c("lat", "long"))
data <- cbind(data,lat.long)


# impute NAs 
impute_na <- function(d=data,t, impute) {

location <- geocode(impute)
system('sleep 3') # can only make so many API requests
  
d[which(d$title==t) ,'long'] <- as.numeric(location[1])

d[which(d$title==t) ,'lat'] <- as.numeric(location[2])
data <<- d 
  
}
data %>% 
  dplyr::filter(is.na(lat)) %>% 
  dplyr::select(show,season,ep,title)

impute_na(t='Food Porn', impute='HANOI VIETNAM')
impute_na(t='Down on the Street', impute= 'NEW YORK CITY')
impute_na(t='Obsessed', impute='NEW YORK CITY') 
impute_na(t='Techniques Special', impute='NEW YORK CITY')
impute_na(t="Burning Questions", impute='NEW YORK CITY')
impute_na(t='Food Porn 2', impute='LOS ANGELES')
impute_na(t='Where It All Began', impute='NEW YORK CITY')
impute_na(t='Making of India', impute='KERALA INDIA')
impute_na(t='What Were We Thinking Special', impute='NEW YORK CITY')
impute_na(t='Holiday Special', impute='NEW YORK CITY')
impute_na(t='Holiday Special 2011', impute='NEW YORK CITY')
impute_na(t='Sex, Drugs And Rock & Roll', impute='PHNOM PENH CAMBODIA')
impute_na(t='Off The Charts', impute='CLUJ-NAPOCA ROMANIA')
impute_na(t='Seven Deadly Sins', impute='ROME ITALY')

impute_tt <- function(d=data, t, city,country) { 
  d[which(d$title==t), 'city_or_area'] <- city
  d[which(d$title==t), 'country'] <- country
  data <<- d
}

impute_tt(t='Food Porn', city='Hanoi', country='Vietnam')
impute_tt(t='Down on the Street', city='New York City', country = 'USA')
impute_tt(t='Obsessed', city='New York City', country = 'USA') 
impute_tt(t='Techniques Special', city='New York City', country = 'USA')
impute_tt(t="Burning Questions", city='New York City', country = 'USA')
impute_tt(t='Food Porn 2', city='Los Angeles', country='USA')
impute_tt(t='Where It All Began', city='New York City', country = 'USA')
impute_tt(t='Making of India', city='Kerala', country='India')
impute_tt(t='What Were We Thinking Special', city='New York City', country = 'USA')
impute_tt(t='Holiday Special', city='New York City', country = 'USA')
impute_tt(t='Holiday Special 2011', city='New York City', country = 'USA')
impute_tt(t='Sex, Drugs And Rock & Roll', city = 'Phnom Penh', country='Cambodia')
impute_tt(t='Off The Charts', city= 'Cluj-Napoca', country='Romania')
impute_tt(t='Seven Deadly Sins', city='Rome', country='Italy')

# Remove prime cuts and take on episode down for parts unknown

data <- data[-which(data$show=='Parts Unknown' & data$season >1 & data$season < 10 & data$ep == 1),]

data$tt <- paste0("Season ", 
                       data$season, " | Ep. ",
                       data$ep, " | ", 
                       data$city_or_area, ", ", data$country)

data <- dplyr::mutate(data,tt = stringr::str_replace_all(tt, "'", ""))

######################
#  
# WORLD MAP  
# 
######################


pal <- colorFactor(c("#C50006", "#400080", "deepskyblue3"), 
                   domain=c('No Reservations', 'The Layover','Parts Unknown'))
world_map <- leaflet(data=data) %>% 
  setView(lng = -17.46769, lat = 14.71668, zoom = 1.33) %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options=providerTileOptions(noWrap = TRUE)) %>% 
  addCircleMarkers(~long, ~lat, popup= ~tt ,label = ~tt, 
                   radius = .5, color = ~pal(show), opacity = .5) %>% 
  addLegend("bottomleft", pal = pal, values = ~show,
            title = "",
            opacity = .5)
######################
#  Histogram 
######################

# Clean Country names ex. USA vs. United States

View(unique(data$country))

usa_impute <- c('United States', 'Missouri', 'Oregon', 'Washington', 'California', 
                'Kansas', 'Massachusetts')
data <- data %>% 
        mutate(country_clean = ifelse(country %in% usa_impute, 'USA',
                                       ifelse(str_detect(country, 'England'),'England', 
                                              ifelse(str_detect(country, 'Scotland'), 
                                                     'Scotland', country))))
countries <- unique(data$country_clean)
codes <- vector()
for(i in 1:length(countries)){ 
  codes[i] <- countrycode(countries[i], origin='country.name', destination = 'iso2c')
}

cc_lkp <- dplyr::bind_cols('country'=countries, 'codes'=codes)

cc_lkp[which(cc_lkp$country %in% c('England', 'Scotland')), 'codes'] <- 'GB'

cc_lkp[which(cc_lkp$country %in% c('Sicily')), 'codes'] <- 'IT'


bourd_hist <- data %>% 
  group_by(country_clean) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  inner_join(cc_lkp, by=c('country_clean'='country')) %>% 
  dplyr::filter(count > 1) %>% 
  ggplot(aes(x=reorder(country_clean, count), y=count, 
             tooltip=paste0(country_clean, ": ", count, ' episodes'), 
             data_id = paste0(country_clean, ": ", count, ' episodes'))) + 
  geom_bar_interactive(stat='identity', alpha=.6) + 
  ggflags::geom_flag(y=-3,size=4, aes(country=tolower(codes))) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(family = ""), 
    panel.grid.major = element_blank()
  ) +
  coord_flip()
ggiraph(code={print(bourd_hist)}, 
        tooltip_extra_css = "background-color:transparent;font-weight:bold;", 
        hover_css="fill:blue;")

######################
#  NEW YORK
######################

ny_data <- read_delim('./nyc.csv', delim='|')
ny_data <- dplyr::mutate(ny_data, restaurant = str_trim(restaurant), 
                         address=str_trim(address))

ny_impute <- function(data=ny_data) { 
  output <- data.frame()
  for(i in 1:nrow(data)) { 
    out <- geocode(as.character(data[i,'address']))
    if(is.na(out)) {
      out <- 'CANT FIND'
    }
    output <- dplyr::bind_rows(output,out)
    
  }
  
  result <- dplyr::bind_cols(data,output)
  return(result)
  
  }
ny_latlong <- ny_impute()


######################
#  NY MAP
######################

# add shows

ny_latlong <- ny_latlong %>% 
            
              mutate(show=ifelse(row_number() <=42, 'No Reservations', 
                                 ifelse(row_number() <=52, 'Parts Unknown', 'The Layover'))) %>% 
              mutate(label = mapply(function(x,y) {HTML(sprintf("<b>%s</b><br>%s", x, 
                                                                y))}, 
                                    x=ny_latlong$restaurant, y=ny_latlong$address, SIMPLIFY = F))


pal <- colorFactor(c("#C50006", "#400080", "deepskyblue3"), 
                   domain=c('No Reservations', 'The Layover','Parts Unknown'))
nyc_map <- leaflet(data=ny_latlong) %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options=providerTileOptions(noWrap = TRUE)) %>% 
  addCircleMarkers(~lon, ~lat, radius = .5, opacity = .5, color=~pal(show),
                   label =~paste0(restaurant, " | ", address)) %>% 
  addLegend("bottomleft", pal = pal, values = ~show,
            title = "",
            opacity = .5)

######################
#  
# SAVE  
# 
######################
ny_latlong <- dplyr::select(ny_latlong,-label)
write_csv(data, path = './bourdain_episodes_clean.csv')
write_csv(ny_latlong, path='./bourdain_nyc.csv')

