
library(plotly)
library(ggplot2)
library(lubridate)
library(depmixS4) #Hidden Markov
library(tidyverse)
library(ggpubr)
library(dplyr)
library(stringdist)
library(ggforce)
library(plotly)
library(ggplot2)
library(ggpubr)
library(leaflet)
library(dplyr)

################################################################################
# library(openSkies)
# 
# K <- getAircraftStateVectorsSeries(useImpalaShell = TRUE,
#                                    username = "XXX",
#                                    password = "XXX",
#                                    aircraft = "4aaa15",
#                                    startTime="2021-06-07 00:00:00",
#                                    endTime = "2021-06-08 00:00:00",
#                                    timeResolution = 10,
#                                    timeZone = "UTC")
# 
# K2 <- data.frame(icao = K$get_values("ICAO24"),
#                  call_sign = K$get_values("call_sign"),
#                  time = K$get_values("requested_time"),
#                  lat = K$get_values("latitude"),
#                  lon = K$get_values("longitude"),
#                  baro_alt = K$get_values("baro_altitude"),
#                  geo_alt = K$get_values("geo_altitude"),
#                  velocity = K$get_values("velocity"),
#                  heading = K$get_values("true_track"),
#                  vertical_rate = K$get_values("vertical_rate"),
#                  diff_time = c(10, diff(K$get_values("requested_time"))))
# 
# K3 = K2[329:625,]
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# K3  = K3 %>% mutate(Time = range01(time))
# 
# write.csv(K3, "/Volumes/T7/HMM/Helico/helico.csv", row.names=FALSE)

################################################################################

K3 = read.csv("/Volumes/T7/HMM/Helico/helico.csv")

axx <- list(title = "Longitude")
axy <- list(title = "Latitude")
axz <- list(title ="Altitude (m)")

fig3D2 <- plot_ly(K3,
                  x = ~lon,
                  y = ~lat,
                  z = ~baro_alt,
                  type = 'scatter3d', mode = 'markers',
                  color = ~Time,
                  opacity = 1, marker = list(size = 2))%>%
  layout(scene = list(zaxis = axz, xaxis = axx, yaxis = axy, 
                      aspectmode="cube"), 
         legend=list(title=list(text='Time')))  %>% layout(showlegend = TRUE) 
fig3D2

Altitude_plot = ggplot(K3, aes(x = Time, y = baro_alt)) +
  geom_point() + theme_bw() + ylab("Altitude (m)") + xlab("Scaled time")
Altitude_plot

Longitude_plot = ggplot(K3, aes(x = Time, y = lon)) +
  geom_point() + theme_bw() + ylab("Longitude (deg)") + xlab("Scaled time")

Latitude_plot = ggplot(K3, aes(x = Time, y = lat)) +
  geom_point() + theme_bw() + ylab("Latitude (deg)") + xlab("Scaled time")

Speed_plot = ggplot(K3, aes(x = Time, y = velocity)) +
  geom_point() + theme_bw() + ylab("Ground speed (m/sec)") + xlab("Scaled time")
Speed_plot

Vertical_plot = ggplot(K3, aes(x = Time, y = vertical_rate)) +
  geom_point() + theme_bw() + ylab("Vertical rate (m/sec)") + xlab("Scaled time")
Vertical_plot

Track_plot = ggplot(K3, aes(x = Time, y = heading)) +
  geom_point() + theme_bw() + ylab("Track angle (deg)") + xlab("Scaled time")
Track_plot

ggarrange(Altitude_plot+ rremove("xlab"), 
          Speed_plot + rremove("xlab"),
          Longitude_plot+ rremove("xlab"), 
          Vertical_plot +rremove("xlab"), 
          Latitude_plot, Track_plot,
          ncol=2, nrow=3, common.legend = TRUE, align="hv")

AirportsIcons <- iconList(
  arrival = leaflet::makeIcon(iconUrl = "/Volumes/T7/HMM/img/arrival.png", iconHeight = 20, iconWidth = 20),
  departure = leaflet::makeIcon(iconUrl = "/Volumes/T7/HMM/img/departure.png", iconHeight = 30, iconWidth = 30)
)
pal <- colorNumeric(
  palette = colorRampPalette(c('blue', 'blue'))(length(K3$Time)), 
  domain = K3$Time)

map <- leaflet(K3) %>%
  addTiles() %>%
  addCircles(lng = ~lon, lat =~lat, color = ~pal(K3$Time), weight = 3) %>%
  addMarkers(lng = ~last(lon), lat =~last(lat), icon = ~AirportsIcons["arrival"]) %>%
  addMarkers(lng = ~first(lon), lat =~first(lat), icon = ~AirportsIcons["departure"]) %>%
  addProviderTiles(providers$CartoDB.Positron)
map

############################################################################################################

K3$lon_first = c(0, diff(K3$lon))
K3$lat_first = c(0, diff(K3$lat))

set.seed = 10

K=15
Iterlikelihood = 100
Number_states_vector = c()
BIC_matrix = matrix(NA, ncol=K, nrow=Iterlikelihood)
for(j in 1:K){
  for(k in 1:Iterlikelihood){
    mod<- depmix(list(lon_first ~ 1, lat_first ~ 1, vertical_rate~1, velocity~1), data = K3, nstates = j,
                 family = list(gaussian(), gaussian(), gaussian(), gaussian()))
    tryCatch({
      fitted.mod.depmix <- fit(mod)
      BIC_matrix[k,j]=BIC(fitted.mod.depmix)
    }, error=function(e){BIC_matrix[k,j]= NA})
  }
  Number_states_vector = c(Number_states_vector, j)
}

BIC_boxplot = data.frame(Number_states = rep(Number_states_vector, each=Iterlikelihood), 
                         BIC = c(BIC_matrix))
library(ggplot2)
ggplot(BIC_boxplot, aes(x = Number_states, y = BIC, group=Number_states)) + 
  geom_boxplot(na.rm = TRUE)+ theme_bw() + ylab("BIC") + xlab("Number of states")

SummaryBIC = BIC_boxplot %>% group_by(Number_states) %>% summarise(summaryBIC = median(BIC, na.rm = TRUE))

K_star = which.min(SummaryBIC$summaryBIC)

best_BIC = 10^9
best_mod = NA

for(f in 1:Iterlikelihood){
  mod<- depmix(list(lon_first ~ 1, lat_first ~ 1, vertical_rate~1, velocity~1), data = K3, nstates = K_star,
               family = list(gaussian(), gaussian(), gaussian(),  gaussian()), instart = runif(K_star))
  
  fitted.mod.depmix = tryCatch({fit(mod)}, error=function(e) {NA})

  if(!is.na(fitted.mod.depmix)){
    if(BIC(fitted.mod.depmix) < best_BIC){
      forwardbackward(fitted.mod.depmix)$gamma
      K3$predict_local_state = posterior(fitted.mod.depmix, type = c("local"))
      K3$predict_local_state_uncertain = posterior(fitted.mod.depmix, type = c("smoothing"))
      K3$predict_local_state_global = posterior(fitted.mod.depmix, type = c("viterbi"))$state
      best_mod = fitted.mod.depmix
      best_BIC = BIC(fitted.mod.depmix)
    }}}

###True 
axx <- list(title = "Longitude")
axy <- list(title = "Latitude")
axz <- list(title ="Altitude (m)")

library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

fig3D <- plot_ly(K3,
                 x = ~lon,
                 y = ~lat,
                 z = ~baro_alt,
                 type = 'scatter3d', mode = 'markers',
                 color = ~as.factor(predict_local_state_global),
                 colors =~col_vector[1:K_star], 
                 opacity = 1, marker = list(size = 4))%>%
  layout(scene = list(zaxis = axz, xaxis = axx, yaxis = axy, 
                      aspectmode="cube"))  %>% layout(showlegend = FALSE) 
fig3D


Altitude_plot = ggplot(K3, aes(x = Time, y = baro_alt, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Altitude (m)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])
Altitude_plot

Longitude_plot = ggplot(K3, aes(x = Time, y = lon_first, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Longitude - first differences (deg)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])

Latitude_plot = ggplot(K3, aes(x = Time, y = lat_first, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Latitude - first differences (deg)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])

Speed_plot = ggplot(K3, aes(x = Time, y = velocity, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Ground speed (m/sec)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])
Speed_plot

Vertical_plot = ggplot(K3, aes(x = Time, y = vertical_rate, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Vertical rate (m/sec)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])
Vertical_plot

Track_plot = ggplot(K3, aes(x = Time, y = heading, color = as.factor(predict_local_state_global))) +
  geom_point() + theme_bw() + ylab("Track angle (deg)") + xlab("Scaled time")+
  labs(color='Segmentation')+ scale_color_manual(values=col_vector[1:K_star])
Track_plot

ggarrange(Altitude_plot+ rremove("xlab"), 
          Speed_plot + rremove("xlab"),
          Longitude_plot+ rremove("xlab"), 
          Vertical_plot +rremove("xlab"), 
          Latitude_plot, Track_plot,
          ncol=2, nrow=3, common.legend = TRUE, align="hv")
