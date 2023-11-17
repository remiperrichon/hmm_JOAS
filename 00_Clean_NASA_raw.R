################################################################################
#### The goal of this notebook is to clean and aggregate data for tail 687 
################################################################################
#Import packages 

library(dplyr) #handle data tables 
library(lubridate) #handle dates 
library(R.matlab) #handle Matlab files 

################################################################################
#### Step 0 - download data 
################################################################################

# Raw data source : https://c3.ndc.nasa.gov/dashlink/resources/664/
# Go to the website, download each zip file 
# Decompress the zip file and copy-paste everything in one folder 
# We have a big folder with all data in Matlab format 

################################################################################
#### Step 1 : for the global folder, list all the flights 
# individual flights are recorded in a Matlab file format
################################################################################

my_list_matlab = list.files("/Volumes/T7/HMM/NASA_raw", full.names = TRUE)

#How many flights in total ? 
length(my_list_matlab)

################################################################################
#### Step 2 : clean raw data with basic filters + regrid (1000 points here)
################################################################################

n_regrid = 1000
#Rescale time function (first point is t=0 and last point is t=1) 
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

for(k in 1:length(my_list_matlab)){
  #open matlab file 
  raw_flight <- readMat(my_list_matlab[k])
  
  #handle time correctly 
  text_date = paste0(raw_flight$DATE.YEAR[[1]], "-",
                     ifelse(raw_flight$DATE.MONTH[[1]] < 10, paste0("0", raw_flight$DATE.MONTH[[1]]), raw_flight$DATE.MONTH[[1]]), "-", 
                     ifelse(raw_flight$DATE.DAY[[1]] < 10, paste0("0", raw_flight$DATE.DAY[[1]]), raw_flight$DATE.DAY[[1]]), " ", 
                     ifelse(raw_flight$GMT.HOUR[[1]] < 10, paste0("0", raw_flight$GMT.HOUR[[1]]), raw_flight$GMT.HOUR[[1]]), ":",
                     ifelse(raw_flight$GMT.MINUTE[[1]] < 10, paste0("0", raw_flight$GMT.MINUTE[[1]]), raw_flight$GMT.MINUTE[[1]]), ":",
                     ifelse(raw_flight$GMT.SEC[[1]] < 10, paste0("0", raw_flight$GMT.SEC[[1]]), raw_flight$GMT.SEC[[1]]))
  
  time <- ymd_hms(text_date)
  
  if(all(is.na(time))){next}
  #Create posix time 
  posix_time = as.numeric(as.POSIXct(time))
  #Duration of the flight in seconds 
  duration_s = max(posix_time, na.rm = TRUE) - min(posix_time, na.rm = TRUE)
  nb_ori_points = length(posix_time)
  #keep flight > 30 mins and if there are at least 5 different phases. Linear interpolation of all the variables
  if(duration_s > 1800){
    if(length(unique(raw_flight$PH[[1]])) > 5){
      Latitude = approx(x = range01(seq(1, length(raw_flight$LATP[[1]]))), y= raw_flight$LATP[[1]], xout = seq(0, 1, length.out = n_regrid))
      Mach = approx(x = range01(seq(1, length(raw_flight$MACH[[1]]))), y= raw_flight$MACH[[1]], xout = seq(0, 1, length.out = n_regrid)) 
      Longitude = approx(x = range01(seq(1, length(raw_flight$LONP[[1]]))), y= raw_flight$LONP[[1]], xout = seq(0, 1, length.out = n_regrid)) 
      Altitude = approx(x = range01(seq(1, length(raw_flight$ALT[[1]]))), y= raw_flight$ALT[[1]], xout = seq(0, 1, length.out = n_regrid))
      Altitude_rate = approx(x = range01(seq(1, length(raw_flight$ALTR[[1]]))), y= raw_flight$ALTR[[1]], xout = seq(0, 1, length.out = n_regrid))
      Unix = approx(x = range01(seq(1, length(as.numeric(as.POSIXct(time))))), y= as.numeric(as.POSIXct(time)), xout = seq(0, 1, length.out = n_regrid))
      Pitch = approx(x = range01(seq(1, length(raw_flight$PTCH[[1]]))), y= raw_flight$PTCH[[1]], xout = seq(0, 1, length.out = n_regrid))
      Ground_speed = approx(x = range01(seq(1, length(raw_flight$GS[[1]]))), y= raw_flight$GS[[1]], xout = seq(0, 1, length.out = n_regrid))
      Air_speed = approx(x = range01(seq(1, length(raw_flight$CAS[[1]]))), y= raw_flight$CAS[[1]], xout = seq(0, 1, length.out = n_regrid))
      
      #outlier in longitude /lat are excluded 
      if(max(diff(Longitude$y)) > 10){next}
      if(max(diff(Latitude$y)) > 10){next}
      
      Flight_phase_data = data.frame(x = range01(seq(1, length(raw_flight$PH[[1]]))), 
                                     phase = raw_flight$PH[[1]])
      #Some kind of linear interpolation for the categorical value of the flight phase 
      seq_regrid = seq(0, 1, length.out = n_regrid)
      closest_phase = c()
      for(j in seq_regrid){
        closest_phase = c(closest_phase, Flight_phase_data[which.min(abs(Flight_phase_data$x - j)), ]$phase)}
      
      Flight = data.frame(time01 = seq(0, 1, length.out = n_regrid),
                          Duration_s = rep(duration_s, n_regrid), 
                          Nb_points = rep(nb_ori_points, n_regrid), 
                          Longitude = Longitude$y, 
                          Latitude = Latitude$y, 
                          Altitude = Altitude$y, 
                          Altitude_rate = Altitude_rate$y,
                          Mach = Mach$y,
                          Flight_phase = paste0("phase", closest_phase), 
                          Unix = Unix$y, 
                          Pitch = Pitch$y, 
                          Ground_speed = Ground_speed$y, 
                          Air_speed = Air_speed$y)
      
      Flight$Flight_phase_text <- recode_factor(Flight$Flight_phase,
                                                phase0 = "Unknown",
                                                phase1 = "Preflight", 
                                                phase2 = "Taxi", 
                                                phase3 = "Takeoff", 
                                                phase4 = "Climb", 
                                                phase5 = "Cruise", 
                                                phase6 = "Approach", 
                                                phase7 = "Rollout")
      
      
      write.csv(Flight, paste0("/Volumes/T7/HMM/NASA_clean_2/flight_", k, ".csv"), row.names=FALSE)
      print(k/length(my_list_matlab))
}
}
}











