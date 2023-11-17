###################################################################################
#### The goal of this notebook is to perform segmention with RoC - 3 flight phases 
###################################################################################

#Import packages 
library(depmixS4) #HMM package 
library(ggplot2) #for plot 
library(ggpubr) #for combining plot 
library(dplyr) #handle data tables 
library(stringr) #package to handle string 

################################################################################
# Step 1 : import file of interest 
################################################################################

#Import file 
#File for the article : flight_133.csv
selected_file = "/Volumes/T7/HMM/NASA_clean_2/flight_133.csv"
Flight <- read.csv(selected_file)

#Select only 3 phases here 
Flight2 = Flight[Flight$Flight_phase_text %in% c('Climb', "Cruise", 'Approach'), ]

# Missing values 
missing_rows = sample(seq(1, nrow(Flight2)), 500)
Altitude_missing = c()
Altitude_rate_missing = c()
for(h in 1:nrow(Flight2)){
  if(h %in% missing_rows){
    Altitude_missing = c(Altitude_missing, NA)
    Altitude_rate_missing = c(Altitude_rate_missing, NA)
  }
  else{
    Altitude_missing = c(Altitude_missing, Flight2$Altitude[h])
    Altitude_rate_missing = c(Altitude_rate_missing, Flight2$Altitude_rate[h])
  }
}
Flight2$Altitude_missing = Altitude_missing
Flight2$Altitude_rate_missing = Altitude_rate_missing

#Recode order of the flight phases to make a beautiful plot 
Flight2$Flight_phase_text <- factor(Flight2$Flight_phase_text, levels = c("Climb", "Cruise", "Approach"))

#Plot of the flight phases, reference flight 
Truth = ggplot(Flight2, aes(x = time01, y = Altitude_missing, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Altitude (ft)") + xlab("Scaled time")+
  labs(color='Flight phase') + scale_color_manual(values=c("#4dc0b5", "#f6993f", "#e3342f"))+
  ggtitle("Original flight")+theme(plot.title = element_text(face = "bold"))

#What is the sequence of flight phases ? 
#Get unique transitions 
Scan_Flight_phase_text <- scan(what = character(), text = as.character(Flight2$Flight_phase_text))
Unik_Flight_phase_text = rle(Scan_Flight_phase_text)$values

################################################################################
#### First, we fit an unconstrained model to have an idea of the parameters values 
#### We comment this because it's necessary to do it once 
################################################################################

# K_impose = 3 #3 states here 
# Iterlikelihood = 1  
# best_BIC = 10^9
# best_mod = NA
# for(f in 1:Iterlikelihood){
#     mod<- depmix(list(Altitude_rate~1), data = Flight2, nstates = K_impose,
#                  family = list(gaussian()), instart=c(1, 0, 0))
# 
#     #parameters of the model 
#     pars <- c(unlist(getpars(mod)))
#     pars[1] = 1
#     pars[2] = 0
#     pars[3] = 0
#     
#     pars[4] = runif(1, min = 0.8, max = 0.95)
#     pars[5] = 1 - pars[4]
#     pars[6] = 0
#     
#     pars[7] = 0
#     pars[8] = runif(1, min = 0.8, max = 0.95)
#     pars[9] = 1 - pars[8]
#     
#     pars[10] = 0
#     pars[11] = runif(1, min = 0.01, max = 0.05)
#     pars[12] = 1 - pars[11]
#     
#     mod_fix <- setpars(mod, pars)
#     
#     fitted.mod.depmix = tryCatch({fit(mod_fix)}, error=function(e) {NA})
#     if(any(is.na(fitted.mod.depmix))){next}
#     if(BIC(fitted.mod.depmix) < best_BIC){
#       forwardbackward(fitted.mod.depmix)$gamma
#       Flight2$predict_local_state = posterior(fitted.mod.depmix, type = c("local"))
#       Flight2$predict_local_state_uncertain = posterior(fitted.mod.depmix, type = c("smoothing"))
#       Flight2$predict_local_state_global = posterior(fitted.mod.depmix, type = c("viterbi"))$state
#       Flight2$predict_local_state_global_uncertain = apply(posterior(fitted.mod.depmix, type = c("viterbi"))[,2:4], 1, max)
#       best_mod = fitted.mod.depmix
#       best_BIC = BIC(fitted.mod.depmix)
#     }
#   }

################################################################################
#### The model is here 
################################################################################

mllk<-function(theta.star,x){
  theta <- c(plogis(theta.star[1]),plogis(theta.star[2]), plogis(theta.star[3]), 
             plogis(theta.star[4]),
             theta.star[5], theta.star[6], theta.star[7],
             exp(theta.star[8]), exp(theta.star[9]), exp(theta.star[10]))
  
  Gamma <- matrix(NA, nrow=3, ncol=3)
  Gamma[1,]= c(theta[1], 1 - theta[1], 0)
  Gamma[2,]= c(theta[2], theta[3], 1-(theta[2]+theta[3]))
  Gamma[3,]= c(0, theta[4],  1-theta[4])
  
  delta <- c(1, 0, 0)
  mu = theta[5:7]
  sigma = theta[8:10]
  allprobs <- matrix(1,dim(x)[1],3)
  ind = which(!is.na(x$Altitude_rate_missing))
  for (j in 1:3){
    allprobs[ind,j] <- dnorm(x$Altitude_rate_missing[ind], mean = mu[j], sd = sigma[j]) 
  }
  foo <- delta%*%diag(allprobs[1,])
  l <- log(sum(foo))
  phi <- foo/sum(foo)
  for (t in 2:dim(x)[1]){
    foo <- phi%*%Gamma%*%diag(allprobs[t,]) 
    l <- l+log(sum(foo))
    phi <- foo/sum(foo)
  }
  return(-l) }

best_min = 10000
#Number of interations, likelihood
Iterlikelihood_hand = 20

for(h in 1:Iterlikelihood_hand){
  theta.star<-c(qlogis(c(runif(1, 0.9, 0.95), runif(1, 0.01, 0.04),
                         runif(1, 0.9, 0.95), runif(1, 0.01, 0.04))),
                #These parameters come from unconstrained 
                c(runif(1, 1500, 1700), runif(1, -50, -10), runif(1, -1500, -500)),
                log(c(runif(1, 500, 1000), runif(1, 50, 150),runif(1, 500, 800))))
  mod_hand = tryCatch({nlm(mllk,theta.star,x=Flight2,print.level=2)}, error=function(e) {NA})
  
  if(any(is.na(mod_hand))){next}
  if(mod_hand$minimum < best_min){
    best_mod_hand = mod_hand
    best_min = mod_hand$minimum
  }
} 

best_mod = best_mod_hand$estimate

Gamma_star <- matrix(NA, nrow=3, ncol=3)
Gamma_star[1,]= c(plogis(best_mod[1]), 1-plogis(best_mod[1]), 0)
Gamma_star[2,]=  c(plogis(best_mod[2]), plogis(best_mod[3]), 1-(plogis(best_mod[2]) + plogis(best_mod[3])))
Gamma_star[3,]=  c(0, plogis(best_mod[4]), 1-plogis(best_mod[4]))
Gamma_star

mu = best_mod[5:7]
mu
sigma = exp(best_mod[8:10])
sigma

################################################################################
#### Decoding 
################################################################################

lforward <- function(x,mu,sigma,Gamma,delta){
  n <- dim(x)[1]
  lalpha <- matrix(NA,3,n)
  allprobs <- matrix(1,dim(x)[1],3)
  ind = which(!is.na(x$Altitude_rate_missing))
  for (j in 1:3){
    allprobs[ind,j] <- dnorm(x$Altitude_rate_missing[ind], mean = mu[j], sd = sigma[j]) 
  }
  foo <- delta*allprobs[1,]
  sumfoo <- sum(foo)
  lscale <- log(sumfoo)
  foo <- foo/sumfoo
  lalpha[,1] <- lscale+log(foo)
  for (t in 2:n){
    foo <- foo %*% Gamma * allprobs[t,] 
    sumfoo <- sum(foo) 
    lscale <- lscale+log(sumfoo)
    foo <- foo/sumfoo
    lalpha[,t] <- log(foo)+lscale
  }
  return(lalpha)
}

lbackward <- function(x,mu,sigma,Gamma,delta){
  n <- dim(x)[1] 
  m = 3
  allprobs <- matrix(1,dim(x)[1],3)
  ind = which(!is.na(x$Altitude_rate_missing))
  for (j in 1:3){
    allprobs[ind,j] <- dnorm(x$Altitude_rate_missing[ind], mean = mu[j], sd = sigma[j]) 
  }
  lbeta <- matrix(NA,3,n)
  lbeta[,n] <- rep(0,m)
  foo <- rep(1/m,m)
  lscale <- log(m)
  for (i in (n-1):1){
    foo <-  Gamma %*%(allprobs[i+1,]*foo)
    lbeta[,i] <- log(foo)+lscale
    sumfoo <- sum(foo)
    foo <- foo/sumfoo
    lscale <- lscale+log(sumfoo)
  }
  return(lbeta)
}

state_probs <- function(x,mu,sigma,Gamma,delta){
  n <- dim(x)[1]
  la <- lforward(x,mu,sigma,Gamma,delta)
  lb <- lbackward(x,mu,sigma,Gamma,delta)
  c <- max(la[,n])
  llk <- c+log(sum(exp(la[,n]-c)))
  stateprobs <- matrix(NA,ncol=n,nrow=3)
  for (i in 1:n){
    stateprobs[,i]<-exp(la[,i]+lb[,i]-llk)
  }
  return(stateprobs)
}

Proba_per_state = state_probs(x=Flight2, mu=mu, sigma=sigma, Gamma = Gamma_star, delta = c(1, 0, 0))
Flight2$proba_state_1 = Proba_per_state[1,]
Flight2$proba_state_2 = Proba_per_state[2,]
Flight2$proba_state_3 = Proba_per_state[3,]

#Uncertainty with local decoding 
Uncertain = ggplot(data=Flight2, aes(x=time01, y=proba_state_1)) +
  geom_line(color="#4dc0b5")+
  geom_line(mapping = aes( y=proba_state_2), color="#f6993f")+
  geom_line(mapping = aes( y=proba_state_3), color="#e3342f")+
  ylab("Probability") + xlab("Scaled time")+
  theme_bw()+
  ggtitle("Uncertainty")+theme(plot.title = element_text(face = "bold"))

#Viterbi algo for global decoding 
viterbi<-function(x,mu,sigma,Gamma,delta){
  n <- dim(x)[1]
  allprobs <- matrix(1,dim(x)[1],3)
  ind = which(!is.na(x$Altitude_rate_missing))
  for (j in 1:3){
    allprobs[ind,j] <- dnorm(x$Altitude_rate_missing[ind], mean = mu[j], sd = sigma[j]) 
  }
  xi <- matrix(0,n,3)
  foo <- delta*allprobs[1,]
  xi[1,] <- foo/sum(foo)
  for (t in 2:n){
    foo <- apply(xi[t-1,]*Gamma,2,max)*allprobs[t,]
    xi[t,] <- foo/sum(foo)
  }
  iv <- numeric(n)
  iv[n] <- which.max(xi[n,]) 
  for (t in (n -1):1){
    iv[t] <- which.max(Gamma[,iv[t+1]]*xi[t,]) }
  iv }

Flight2$viterbi = viterbi(x=Flight2, mu=mu, sigma=sigma, Gamma = Gamma_star, delta = c(1, 0, 0))

Flight2$viterbi = ifelse(Flight2$viterbi == 1, "Climb", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 2, "Cruise", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 3, "Approach", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == "Cruise" & Flight2$Altitude <10000 & Flight2$time01 > 0.5, "Approach", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == "Cruise" & Flight2$Altitude <10000 & Flight2$time01 < 0.5, "Climb", Flight2$viterbi)
#Recode factors for beautiful plot 
Flight2$viterbi <- factor(Flight2$viterbi, levels = c("Climb", "Cruise", "Approach"))

#Global decoding 
Predict_viterbi = ggplot(Flight2, aes(x = time01, y = Altitude_missing, color = as.factor(viterbi))) +
  geom_point() + theme_bw() + ylab("Altitude (ft)") + xlab("Scaled time")+
  labs(color='Segmentation') + scale_color_manual(values=c("#4dc0b5", "#f6993f", "#e3342f"))+
  ggtitle("HMM segmentation")+theme(plot.title = element_text(face = "bold"))

Scan_viterbi <- scan(what = character(), text = as.character(Flight2$viterbi))
Unik_viterbi = rle(Scan_viterbi)$values

################################################################################
#### Result of all segmentations 
################################################################################

ggarrange(Truth, 
          Predict_viterbi,  ncol=2)

