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

files <- list.files('./folder_res_3', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

myMergedData <- read_csv(files) %>% bind_rows(.id = 'id')

length(unique(myMergedData$flight_id))

################################################################################
#### Compute median global accuracy per method and plot it 
################################################################################

summary_1 = myMergedData %>% group_by(Type, bw_ar, bw_gs) %>% summarize(median_acc = median(Accuracy), 
                                                                                           median_acc_climb = median(F_1_climb), 
                                                                                           median_acc_cruise = median(F_1_cruise), 
                                                                                           median_acc_approach = median(F_1_approach))

HMM_plot = ggplot(data= summary_1[summary_1$Type == "HMM" & summary_1$bw_gs == min(summary_1$bw_gs),],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median accuracy", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("HMM")+
  geom_text(aes(label =  round(median_acc, 3)), color = "white", size = 4, fontface='bold')

Naive_plot = ggplot(data= summary_1[summary_1$Type == "Naive" & summary_1$bw_gs == min(summary_1$bw_gs),],
                  aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median accuracy", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Naive")+
  geom_text(aes(label =  round(median_acc, 3)), color = "white", size = 4, fontface='bold') 


Fuzzy_plot = ggplot(data= summary_1[summary_1$Type == "Fuzzy",],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median accuracy", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Fuzzy")+
  geom_text(aes(label =  round(median_acc, 3)), color = "white", size = 4, fontface='bold')

global = ggarrange(HMM_plot, Naive_plot, Fuzzy_plot, nrow=1, common.legend = TRUE, 
          legend="right")
global


################################################################################
#### Climb 
################################################################################

HMM_plot_2 = ggplot(data= summary_1[summary_1$Type == "HMM",],
                  aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_climb)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Climb", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("HMM")+
  geom_text(aes(label =  round(median_acc_climb, 3)), color = "white", size = 4, fontface='bold') 

Naive_plot_2 = ggplot(data= summary_1[summary_1$Type == "Naive",],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_climb)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Climb", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Naive")+
  geom_text(aes(label =  round(median_acc_climb, 3)), color = "white", size = 4, fontface='bold') 


Fuzzy_plot_2 = ggplot(data= summary_1[summary_1$Type == "Fuzzy",],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_climb)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Climb", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Fuzzy")+
  geom_text(aes(label =  round(median_acc_climb, 3)), color = "white", size = 4, fontface='bold') 

climb = ggarrange(HMM_plot_2, Naive_plot_2, Fuzzy_plot_2, nrow=1, common.legend = TRUE, 
          legend="right")

################################################################################
#### Cruise 
################################################################################

HMM_plot_3 = ggplot(data= summary_1[summary_1$Type == "HMM",],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_cruise)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Cruise", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("HMM")+
  geom_text(aes(label =  round(median_acc_cruise, 3)), color = "white", size = 4, fontface='bold') 

Naive_plot_3 = ggplot(data= summary_1[summary_1$Type == "Naive",],
                      aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_cruise)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Cruise", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Naive")+
  geom_text(aes(label =  round(median_acc_cruise, 3)), color = "white", size = 4, fontface='bold') 


Fuzzy_plot_3 = ggplot(data= summary_1[summary_1$Type == "Fuzzy",],
                      aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_cruise)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Cruise", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Fuzzy")+
  geom_text(aes(label =  round(median_acc_cruise, 3)), color = "white", size = 4, fontface='bold') 

cruise = ggarrange(HMM_plot_3, Naive_plot_3, Fuzzy_plot_3, nrow=1, common.legend = TRUE, 
          legend="right")

################################################################################
#### Approach 
################################################################################

HMM_plot_4 = ggplot(data= summary_1[summary_1$Type == "HMM",],
                    aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_approach)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Approach", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("HMM")+
  geom_text(aes(label =  round(median_acc_approach, 3)), color = "white", size = 4, fontface='bold') 

Naive_plot_4 = ggplot(data= summary_1[summary_1$Type == "Naive",],
                      aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_approach)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Approach", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Naive")+
  geom_text(aes(label =  round(median_acc_approach, 3)), color = "white", size = 4, fontface='bold') 


Fuzzy_plot_4 = ggplot(data= summary_1[summary_1$Type == "Fuzzy",],
                      aes(as.factor(bw_ar), as.factor(bw_gs), fill= median_acc_approach)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +theme_minimal() +ylab("Bandwidth value - Ground speed") + xlab("Bandwidth value - RoC")+
  theme(aspect.ratio=1, legend.direction = "vertical", plot.title = element_text(face = "bold"))+
  scale_fill_gradientn("Median F1 score - Approach", colors = hcl.colors(5, "RdYlGn"))  +
  ggtitle("Fuzzy")+
  geom_text(aes(label =  round(median_acc_approach, 3)), color = "white", size = 4, fontface='bold') 

approach = ggarrange(HMM_plot_4, Naive_plot_4, Fuzzy_plot_4, nrow=1, common.legend = TRUE, 
          legend="right")





