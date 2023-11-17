################################################################################
#### Performance metrics for multivariate HMM - 6 flight phases 
################################################################################

#Import packages 
library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggpubr)

################################################################################
#### Import data 
################################################################################

files <- list.files('./folder_res_4', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

myMergedData <- read_csv(files) %>% bind_rows(.id = 'id')

################################################################################
## Global accuracy 
################################################################################

Accuracy = ggplot(myMergedData, aes(x= factor(0), y=Accuracy)) + 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  ylab("Accuracy")+xlab(NULL)+
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(c(0.9, 1))

################################################################################
#### Recall per state 
################################################################################

Recall_climb = ggplot(myMergedData, aes(x=factor(0), y=Recall_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Climb")+
  ylim(quantile(myMergedData$Recall_climb, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_climb, 0.9, na.rm = TRUE))

Recall_cruise = ggplot(myMergedData, aes(x=factor(0), y=Recall_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Cruise")+
  ylim(quantile(myMergedData$Recall_cruise, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_cruise, 0.9, na.rm = TRUE))

Recall_approach = ggplot(myMergedData, aes(x=factor(0), y=Recall_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Approach")+
  ylim(quantile(myMergedData$Recall_approach, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_approach, 0.9, na.rm = TRUE))

Recall_taxi = ggplot(myMergedData, aes(x=factor(0), y=Recall_taxi)) + 
  geom_boxplot(outlier.shape = NA, fill="#9561e2")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Taxi")+
  ylim(quantile(myMergedData$Recall_taxi, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_taxi, 0.9, na.rm = TRUE))

Recall_takeoff = ggplot(myMergedData, aes(x=factor(0), y=Recall_takeoff)) + 
  geom_boxplot(outlier.shape = NA, fill="#3490dc")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Takeoff")+
  ylim(quantile(myMergedData$Recall_takeoff, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_takeoff, 0.9, na.rm = TRUE))

Recall_rollout = ggplot(myMergedData, aes(x=factor(0), y=Recall_rollout)) + 
  geom_boxplot(outlier.shape = NA, fill="#ffed4a")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Rollout")+
  ylim(quantile(myMergedData$Recall_rollout, 0.1, na.rm = TRUE), quantile(myMergedData$Recall_rollout, 0.9, na.rm = TRUE))

ggarrange(Recall_taxi + rremove("xlab"),
          Recall_takeoff + rremove("xlab"), Recall_climb+ rremove("xlab"),
          Recall_cruise+ rremove("xlab"),  Recall_approach+ rremove("xlab"), 
          Recall_rollout+ rremove("xlab"),
          nrow=1)

################################################################################
#### Precision per state 
################################################################################

Precision_climb = ggplot(myMergedData, aes(x=factor(0), y=Precision_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Climb")+
  ylim(quantile(myMergedData$Precision_climb, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_climb, 0.9, na.rm = TRUE))

Precision_cruise = ggplot(myMergedData, aes(x=factor(0), y=Precision_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Cruise")+
  ylim(quantile(myMergedData$Precision_cruise, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_cruise, 0.9, na.rm = TRUE))

Precision_approach = ggplot(myMergedData, aes(x=factor(0), y=Precision_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Approach")+
  ylim(quantile(myMergedData$Precision_approach, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_approach, 0.9, na.rm = TRUE))

Precision_taxi = ggplot(myMergedData, aes(x=factor(0), y=Precision_taxi)) + 
  geom_boxplot(outlier.shape = NA, fill="#9561e2")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Taxi")+
  ylim(quantile(myMergedData$Precision_taxi, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_taxi, 0.9, na.rm = TRUE))

Precision_takeoff = ggplot(myMergedData, aes(x=factor(0), y=Precision_takeoff)) + 
  geom_boxplot(outlier.shape = NA, fill="#3490dc")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Takeoff")+
  ylim(quantile(myMergedData$Precision_takeoff, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_takeoff, 0.9, na.rm = TRUE))

Precision_rollout = ggplot(myMergedData, aes(x=factor(0), y=Precision_rollout)) + 
  geom_boxplot(outlier.shape = NA, fill="#ffed4a")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Rollout")+
  ylim(quantile(myMergedData$Precision_rollout, 0.1, na.rm = TRUE), quantile(myMergedData$Precision_rollout, 0.9, na.rm = TRUE))

ggarrange(Precision_taxi + rremove("xlab"),
          Precision_takeoff + rremove("xlab"), Precision_climb+ rremove("xlab"),
          Precision_cruise+ rremove("xlab"),  Precision_approach+ rremove("xlab"), 
          Precision_rollout+ rremove("xlab"),
          nrow=1)

################################################################################
#### F-1 score per state 
################################################################################

F_1_climb = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Climb")+
  ylim(0.5, 1)

F_1_cruise = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Cruise")+
  ylim(0.5, 1)

F_1_approach = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Approach")+
  ylim(0.5, 1)

F_1_taxi = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_taxi)) + 
  geom_boxplot(outlier.shape = NA, fill="#9561e2")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Taxi")+
  ylim(0.5, 1)

F_1_takeoff = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_takeoff)) + 
  geom_boxplot(outlier.shape = NA, fill="#3490dc")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Takeoff")+
  ylim(0.5, 1)

F_1_rollout = ggplot(myMergedData, aes(x=factor(0), y=F_1_score_rollout)) + 
  geom_boxplot(outlier.shape = NA, fill="#ffed4a")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ ylab("F1 score - Rollout")+
  ylim(0.5, 1)

ggarrange(F_1_taxi + rremove("xlab"),
          F_1_takeoff + rremove("xlab"), F_1_climb+ rremove("xlab"),
          F_1_cruise+ rremove("xlab"),  F_1_approach+ rremove("xlab"), 
          F_1_rollout+ rremove("xlab"),
          nrow=1, 
          align = "hv")

################################################################################
################################################################################
## Nb of transitions  
################################################################################
################################################################################

ggplot(myMergedData, aes(Nb_transitions)) +
  geom_density(col="red", bw = 0.9)+
  geom_density(mapping = aes(Nb_transitions_HMM), col="blue", bw = 0.9)+
  theme_minimal()+
  xlab("Number of transitions")+ylab("Density")

################################################################################
################################################################################
## Nb of points for takeoff and rollout
################################################################################
################################################################################

ggplot(myMergedData, aes(nb_points_takeoff)) +
  geom_density(col="#3490dc", linewidth=2)+
  geom_density(mapping = aes(nb_points_rollout), col="#ffed4a", linewidth=2)+
  theme_minimal()+
  xlab("Kernel density estimate of the number of takeoff points [blue] and rollout points [yellow] in the original trajectories")+ylab("Density")

mean(myMergedData$nb_points_takeoff)
mean(myMergedData$nb_points_rollout)
