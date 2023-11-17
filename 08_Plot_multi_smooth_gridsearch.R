################################################################################
#### Grid search for smoothing bandwidth, 3 states 
################################################################################

#Import packages 

library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggpubr)
library(dplyr)

################################################################################
#### Import data 
################################################################################

files <- list.files('./folder_res_5', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

myMergedData <- read_csv(files) %>% bind_rows(.id = 'id')

length(unique(myMergedData$flight_id))

################################################################################
#### Boxplot by group R
################################################################################

Accuracy = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=Accuracy)) + 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Accuracy")+xlab("Bandwidth for the RoC")
  ylim(quantile(myMergedData$Accuracy, 0.1, na.rm = TRUE), quantile(myMergedData$Accuracy, 0.9,  na.rm = TRUE))
Accuracy

F_1_climb = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Climb")+xlab("Bandwidth for the RoC")+
  ylim(0.3, 1)

F_1_cruise = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Cruise")+xlab("Bandwidth for the RoC")+
  ylim(0.3, 1)

F_1_approach = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Approach")+xlab("Bandwidth for the RoC")+
  ylim(0.3, 1)


F_1_taxi = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_taxi)) + 
  geom_boxplot(outlier.shape = NA, fill="#9561e2")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Taxi")+xlab("Bandwidth for the RoC")+
  ylim(0.3, 1)

F_1_takeoff = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_takeoff)) + 
  geom_boxplot(outlier.shape = NA, fill="#3490dc")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Takeoff")+xlab("Bandwidth for the RoC")+
  ylim(0.3, 1)

F_1_rollout = ggplot(myMergedData, aes(x=as.factor(h_altitude_rate), y=F_1_score_rollout))+
  geom_boxplot(outlier.shape = NA, fill="#ffed4a")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Rollout")+xlab("Bandwidth for the RoC") +
  ylim(0.3, 1)

ggarrange(F_1_taxi + rremove("xlab"),
          F_1_takeoff + rremove("xlab"),
          F_1_climb+ rremove("xlab"),
          F_1_cruise,
          F_1_approach, 
          F_1_rollout,
          nrow=2, ncol=3)

################################################################################
################################################################################
## Nb of transitions  
################################################################################
################################################################################

cols <- c("#9daf18", "#3ae144", "#218e4b", "#67f2f0", "#402ed3")

# Basic density plot in ggplot2
ggplot(myMergedData, aes(x = Nb_transitions_HMM, colour = as.factor(h_altitude_rate))) +
  geom_density(lwd = 0.8, linetype = 2, bw = 3) + xlim(0, 50)+
  geom_density(bw = 3, lwd = 1.2, data = myMergedData[myMergedData$h_altitude_rate == 0,], mapping = aes(Nb_transitions), col="red")+
  scale_color_manual(values = cols, name="RoC bandwidth value")+
  xlab("Number of transitions")+ylab("Density")+  theme_minimal() 




