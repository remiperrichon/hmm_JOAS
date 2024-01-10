################################################################################
#### The goal of this notebook is to vizualize a single NASA flight 
################################################################################

library(plotly)
library(ggplot2)
library(ggpubr)
library(leaflet)
library(dplyr)

col_vector = c("#e3342f", "#4dc0b5", "#f6993f", "#38c172", "#ffed4a", "#3490dc", 
               "#9561e2")

path_clean = "./clean_data"
path_to_icon_leaflet_1 = "./map_symbols/departure.png"
path_to_icon_leaflet_2 = "./map_symbols/arrival.png"

################################################################################
#### Step 0 - read data 
################################################################################

#List all available flight 
my_list = list.files(path_clean, full.names = TRUE)

#Randomly select a flight 
selected_file = sample(my_list, 1)

#Open the selected flight 
Flight = read.csv(selected_file)

Flight <- Flight %>%
  arrange(Flight_phase_text)

################################################################################
#### 3D view 
################################################################################

axx <- list(title = "Longitude")
axy <- list(title = "Latitude")
axz <- list(title ="Altitude (ft)")

fig3D2 <- plot_ly(Flight,
                  x = ~Longitude,
                  y = ~Latitude,
                  z = ~Altitude,
                  type = 'scatter3d', mode = 'markers',
                  color = ~Flight_phase_text,
                  colors =~col_vector, 
                  opacity = 1, marker = list(size = 5))%>%
  layout(scene = list(zaxis = axz, xaxis = axx, yaxis = axy, 
                      aspectmode="cube"))  %>% layout(showlegend = FALSE) 
fig3D2

################################################################################
#### Some profiles 
################################################################################

Altitude_plot = ggplot(Flight, aes(x = time01, y = Altitude, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Altitude (ft)") + xlab("Scaled time")+
  labs(color='Flight phase')+scale_colour_manual(values = col_vector)

Longitude_plot = ggplot(Flight, aes(x = time01, y = Longitude, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Longitude (deg)") + xlab("Scaled time")+
  labs(color='Flight phase')  +scale_colour_manual(values = col_vector)

Latitude_plot = ggplot(Flight, aes(x = time01, y = Latitude, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Latitude (deg)") + xlab("Scaled time")+
  labs(color='Flight phase')  +scale_colour_manual(values = col_vector)

ggarrange(Altitude_plot+ rremove("xlab"), Longitude_plot+ rremove("xlab"), Latitude_plot, nrow=3, common.legend = TRUE)

################################################################################
#### Flat view 
################################################################################

Flight <- Flight %>%
  arrange(time01)

AirportsIcons <- iconList(
  arrival = leaflet::makeIcon(iconUrl = path_to_icon_leaflet_2, iconHeight = 20, iconWidth = 20),
  departure = leaflet::makeIcon(iconUrl = path_to_icon_leaflet_1, iconHeight = 30, iconWidth = 30)
)
pal <- colorNumeric(
  palette = colorRampPalette(c('blue', 'blue'))(length(Flight$time01)), 
  domain = Flight$time01)

map <- leaflet(Flight) %>%
  addTiles() %>%
  addCircles(lng = ~Longitude, lat =~Latitude, color = ~pal(Flight$time01), weight = 0.1) %>%
  addMarkers(lng = ~last(Longitude), lat =~last(Latitude), icon = ~AirportsIcons["arrival"]) %>%
  addMarkers(lng = ~first(Longitude), lat =~first(Latitude), icon = ~AirportsIcons["departure"]) %>%
  addProviderTiles(providers$CartoDB.Positron)
map
