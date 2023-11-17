################################################################################
#### Performance metrics with smoothing, univariate model 
################################################################################

#Import packages 
library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggpubr)

################################################################################
#### Files from ENAC servers  
################################################################################

files <- list.files('./folder_res_2', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

myMergedData <- read_csv(files) %>% bind_rows(.id = 'id')

################################################################################
#### Accuracy - global
################################################################################

Accuracy = ggplot(myMergedData, aes(x=Type, y=Accuracy)) + 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Accuracy")+
  ylim(quantile(myMergedData$Accuracy, 0.1), quantile(myMergedData$Accuracy, 0.9))

################################################################################
#### Accuracy - Precision per state 
################################################################################

Recall_climb = ggplot(myMergedData, aes(x=Type, y=Recall_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Climb")+
  ylim(quantile(myMergedData$Recall_climb, 0.1), quantile(myMergedData$Recall_climb, 0.9))

Recall_cruise = ggplot(myMergedData, aes(x=Type, y=Recall_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Cruise")+
  ylim(quantile(myMergedData$Recall_cruise, 0.1), quantile(myMergedData$Recall_cruise, 0.9))

Recall_approach = ggplot(myMergedData, aes(x=Type, y=Recall_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Recall - Approach")+
  ylim(quantile(myMergedData$Recall_approach, 0.1), quantile(myMergedData$Recall_approach, 0.9))

Precision_climb = ggplot(myMergedData, aes(x=Type, y=Precision_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Climb")+
  ylim(quantile(myMergedData$Precision_climb, 0.1, na.rm=TRUE), quantile(myMergedData$Precision_climb, 0.9, na.rm=TRUE))

Precision_cruise = ggplot(myMergedData, aes(x=Type, y=Precision_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Cruise")+
  ylim(quantile(myMergedData$Precision_cruise, 0.1, na.rm=TRUE), quantile(myMergedData$Precision_cruise, 0.9, na.rm=TRUE))

Precision_approach = ggplot(myMergedData, aes(x=Type, y=Precision_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("Precision - Approach")+
  ylim(quantile(myMergedData$Precision_approach, 0.1, na.rm=TRUE), quantile(myMergedData$Precision_approach, 0.9, na.rm=TRUE))

ggarrange(Recall_climb + rremove("xlab"),
          Recall_cruise + rremove("xlab"),
          Recall_approach+ rremove("xlab"),
          Precision_climb + rremove("xlab"), 
          Precision_cruise + rremove("xlab"), 
          Precision_approach +rremove("xlab"), 
          nrow=1)

################################################################################
#### F1 score 
################################################################################

F1_climb = ggplot(myMergedData, aes(x=Type, y=F_1_climb)) + 
  geom_boxplot(outlier.shape = NA, fill="#4dc0b5")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Climb")+
  ylim(quantile(myMergedData$F_1_climb, 0.1, na.rm=TRUE), quantile(myMergedData$F_1_climb, 0.9, na.rm=TRUE))

F1_cruise = ggplot(myMergedData, aes(x=Type, y=F_1_cruise)) + 
  geom_boxplot(outlier.shape = NA, fill="#f6993f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Cruise")+
  ylim(quantile(myMergedData$F_1_cruise, 0.1, na.rm=TRUE), quantile(myMergedData$F_1_cruise, 0.9, na.rm=TRUE))

F1_approach = ggplot(myMergedData, aes(x=Type, y=F_1_approach)) + 
  geom_boxplot(outlier.shape = NA, fill="#e3342f")+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+ ylab("F1 score - Approach")+
  ylim(quantile(myMergedData$F_1_approach, 0.1, na.rm=TRUE), quantile(myMergedData$F_1_approach, 0.9, na.rm=TRUE))


ggarrange(Accuracy + rremove("xlab"),
          F1_climb + rremove("xlab"),
          F1_cruise+ rremove("xlab"),
          F1_approach + rremove("xlab"), 
          nrow=1)

################################################################################
#### Phase transitions 
################################################################################

CC = ggplot(myMergedData, aes(x=Type, y=climb_to_approach)) + 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+
  ylab("Number of transitions - Cruise to Approach")+
  ylim(quantile(myMergedData$climb_to_approach, 0.1), quantile(myMergedData$climb_to_approach, 0.9))

AC = ggplot(myMergedData, aes(x=Type, y=approach_to_climb)) + 
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun=mean, geom="point", shape=4, size=2, stroke = 2, color="black", fill="black") +
  theme_minimal() +
  theme(legend.position = "none")+
  ylab("Number of transitions - Approach to Climb")+
  ylim(quantile(myMergedData$approach_to_climb, 0.1), quantile(myMergedData$approach_to_climb, 0.9))

ggarrange(CC+ rremove("xlab"), AC+ rremove("xlab"), nrow=1)

################################################################################
#### How many invalid transition in original data ? 
################################################################################

myMergedData$sum_invalid = myMergedData$climb_to_approach + myMergedData$approach_to_climb
summary1 = myMergedData %>% group_by(Type) %>% summarise(count_invalid = sum(sum_invalid == 0) / n())
summary1
subset = myMergedData[myMergedData$Type == "HMM",]
subset$total_invalid_truth = subset$climb_to_approach_truth + subset$approach_to_climb_truth
summary2 = subset %>% summarise(count_invalid_truth = sum(total_invalid_truth == 0) / n())
summary2

################################################################################
#### Number of transitions for each method 
################################################################################

subset = myMergedData[myMergedData$Type == "HMM",]
subset2 = myMergedData[myMergedData$Type == "Naive",]
subset3 = myMergedData[myMergedData$Type == "Fuzzy",]

median(subset$Nb_transitions)
median(subset3$Nb_transitions_model)
median(subset2$Nb_transitions_model)
median(subset$Nb_transitions_model)

median(subset$climb_to_approach + subset$approach_to_climb)
median(subset$climb_to_approach_truth + subset$climb_to_approach_truth)
median(subset2$climb_to_approach + subset2$approach_to_climb)
median(subset3$climb_to_approach + subset3$climb_to_approach)
mean(subset3$climb_to_approach + subset3$climb_to_approach)





Data_frame_transitions = data.frame(nb = c(subset$Nb_transitions_model, 
                                           subset$Nb_transitions, 
                                           subset2$Nb_transitions_model, 
                                           subset3$Nb_transitions_model), 
                                    type = rep(c("HMM", "Truth", "Naive", "Fuzzy"), each=nrow(subset)))

ggplot(Data_frame_transitions, aes(nb, colour = type)) +
  geom_density() +
  theme_minimal()+
  xlab("Number of transitions")+ylab("Density")+
  scale_colour_discrete(name = NULL)




