
library(plotly)
library(ggplot2)
library(lubridate)
library(depmixS4) #Hidden Markov
library(tidyverse)
library(ggpubr)
library(dplyr)
library(stringdist)
library(ggforce)
n_regrid = 1000

selected_file = "/Volumes/T7/HMM/NASA_clean_2/flight_133.csv"


myplots_truth <- vector('list', 1)
myplots_predict <- vector('list', 1)

Flight <- read.csv(selected_file)
Flight2 = Flight[Flight$Flight_phase_text %in% c('Taxi', 'Climb', "Cruise", 'Approach', 'Takeoff', "Rollout"), ]
  
Flight2$Flight_phase_text <- factor(Flight2$Flight_phase_text, levels = c("Taxi", "Takeoff", "Climb", "Cruise", "Approach", "Rollout"))
  
Truth_1 = ggplot(Flight2, aes(x = time01, y = Altitude, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Altitude (ft)") + xlab("Scaled time")+
  labs(color='Flight phase') + scale_color_manual(values=c("#9561e2", "#3490dc", "#4dc0b5", "#f6993f", "#e3342f", 
                                                           "#ffed4a"))+
  ggtitle("Original flight")+theme(plot.title = element_text(face = "bold"))
Truth_1

Truth_2 = ggplot(Flight2, aes(x = time01, y = Ground_speed, color = as.factor(Flight_phase_text))) +
  geom_point() + theme_bw() + ylab("Ground speed (kt)") + xlab("Scaled time")+
  labs(color='Flight phase') + scale_color_manual(values=c("#9561e2", "#3490dc", "#4dc0b5", "#f6993f", "#e3342f", 
                                                           "#ffed4a"))
Truth_2

Scan_Flight_phase_text <- scan(what = character(), text = as.character(Flight2$Flight_phase_text))
Unik_Flight_phase_text = rle(Scan_Flight_phase_text)$values

Flight2$Binary_GS =ifelse(Flight2$Ground_speed < 0.05 , 1, 0)
Flight2$GS_deriv = (c(mean(diff(Flight2$Ground_speed)), diff(Flight2$Ground_speed)))
Flight2$Binary_GS_deriv =ifelse(Flight2$GS_deriv > quantile(Flight2$GS_deriv, 0.995), 1,
                            ifelse(Flight2$GS_deriv < quantile(Flight2$GS_deriv, 0.005), -1, 0))

#Flight2$smoothed_Altitude_rate = ksmooth(x=Flight2$time01, y=Flight2$Altitude_rate, "normal", bandwidth = 0.01)$y
#plot(Flight2$time01, Flight2$smoothed_Altitude_rate, type = "l")
#lines(Flight2$time01, Flight2$Altitude_rate, col="red")

K_impose = 6
Iterlikelihood = 1
best_BIC = 10^9
best_mod = NA

for(f in 1:Iterlikelihood){
  mod<- depmix(list(Altitude_rate~1, Binary_GS~1, Binary_GS_deriv~1), data = Flight2, nstates = K_impose,
               family = list(gaussian(),  multinomial("identity"), multinomial("identity")), instart=c(1, 0, 0, 0, 0, 0))
  npar(mod)
  freepars(mod)
  getpars(mod,  which = "fixed")
  #parameters of the model 
  pars <- c(unlist(getpars(mod)))
  pars[1] = 1
  pars[2] = 0
  pars[3] = 0
  pars[4] = 0
  pars[5] = 0
  pars[6] = 0
  
  #Taxi to ... 
  pars[7] = runif(1, min = 0.9, max = 0.95) #taxi
  pars[8] = 1-pars[7] #takeoff
  pars[9] = 0
  pars[10] = 0
  pars[11] = 0
  pars[12] = 0
  
  #Takeoff to ...
  pars[13] = 0
  pars[14] = runif(1, min = 0.1, max = 0.5) #takeoff
  pars[15] = 1-pars[14] #climb
  pars[16] = 0
  pars[17] = 0
  pars[18] = 0
  
  #Climb to 
  pars[19] = 0
  pars[20] = 0
  pars[21] = runif(1, min = 0.9, max = 0.95) #Climb 
  pars[22] = 1 - pars[21] #cruise 
  pars[23] = 0
  pars[24] = 0
  
  #Cruise to 
  pars[25] = 0
  pars[26] = 0
  pars[27] = runif(1, min = 0.01, max = 0.04) #climb  
  pars[28] = runif(1, min = 0.8, max = 0.9) #Cruise 
  pars[29] = 1 - pars[27] +pars[28]  #approach 
  pars[30] = 0
  
  #Approach to 
  pars[31] = 0
  pars[32] = 0
  pars[33] = 0
  pars[34] = runif(1, min = 0.01, max = 0.02) #Cruise 
  pars[35] = runif(1, min = 0.8, max = 0.9) #Approach 
  pars[36] = 1 - pars[34] + pars[35]
  
  #Rollout to 
  pars[37] = runif(1, min = 0.01, max = 0.04)
  pars[38] = 0
  pars[39] = 0
  pars[40] = 0
  pars[41] = 0
  pars[42] = 1 - pars[37]
  
  mod_fix <- setpars(mod, pars)
  fitted.mod.depmix = tryCatch({fit(mod_fix)}, error=function(e) {NA})
  
  if(!is.na(fitted.mod.depmix)){
  if(BIC(fitted.mod.depmix) < best_BIC){
    forwardbackward(fitted.mod.depmix)$gamma
    Flight2$predict_local_state = posterior(fitted.mod.depmix, type = c("local"))
    Flight2$predict_local_state_uncertain = posterior(fitted.mod.depmix, type = c("smoothing"))
    Flight2$predict_local_state_global = posterior(fitted.mod.depmix, type = c("viterbi"))$state
    Flight2$predict_local_state_global_uncertain = apply(posterior(fitted.mod.depmix, type = c("viterbi"))[,2:K_impose+1], 1, max)
    best_mod = fitted.mod.depmix
    best_BIC = BIC(fitted.mod.depmix)
  }}}

summary(best_mod)

mllk<-function(theta.star,x){
  theta <- c(plogis(theta.star[1]), plogis(theta.star[2]), plogis(theta.star[3]), 
             plogis(theta.star[4]), plogis(theta.star[5]), plogis(theta.star[6]), 
             plogis(theta.star[7]), plogis(theta.star[8]),
             #mu - 6 states 
             theta.star[9], theta.star[10], theta.star[11], theta.star[12], 
             theta.star[13],  theta.star[14],
             #sigma - 6 states 
             exp(theta.star[15]), exp(theta.star[16]), exp(theta.star[17]), 
             exp(theta.star[18]), exp(theta.star[19]), exp(theta.star[20]),
             #pi = 0 (pas au sol)
             plogis(theta.star[21]), plogis(theta.star[22]), plogis(theta.star[23]), 
             plogis(theta.star[24]), plogis(theta.star[25]),plogis(theta.star[26]), 
             #phi_1 = -1 (rollout)
             plogis(theta.star[27]), plogis(theta.star[28]), plogis(theta.star[29]), 
             plogis(theta.star[30]), plogis(theta.star[31]), plogis(theta.star[32]), 
             #phi_2 = 1 (takeoff)
             plogis(theta.star[33]), plogis(theta.star[34]), plogis(theta.star[35]), 
             plogis(theta.star[36]), plogis(theta.star[37]), plogis(theta.star[38]))

  Gamma <- matrix(NA, nrow=6, ncol=6)
  Gamma[1,]= c(theta[1], 1 - theta[1], 0, 0, 0, 0)
  Gamma[2,]= c(0, theta[2], 1-theta[2], 0, 0, 0)
  Gamma[3,]= c(0, 0, theta[3],  1-theta[3], 0, 0)
  Gamma[4,]= c(0, 0, theta[4],  theta[5], 1 - theta[4]+ theta[5], 0)
  Gamma[5,]= c(0, 0, 0, theta[6], theta[7], 1-theta[6]+theta[7])
  Gamma[6,]= c(theta[8], 0, 0, 0, 0, 1-theta[8])
  
  delta <- c(1, 0, 0, 0, 0, 0)
  mu = theta[9:14]
  sigma = theta[15:20]
  pi = theta[21:26]
  phi_1 = theta[27:32]
  phi_2 = theta[33:38]
  
  allprobs <- matrix(1,dim(x)[1],6)
  ind = which(!is.na(x$Altitude_rate)& !is.na(x$Binary_GS) & !is.na(x$Binary_GS_deriv))
  
  list_binary_mach = ifelse(x$Binary_GS_deriv == -1, list(c(1, 0, 0)), 
                            ifelse(x$Binary_GS_deriv == 0, list(c(0, 1, 0)), 
                                   list(c(0, 0, 1))))
  
  for(k in 1:length(ind)){
    for (j in 1:6){
      allprobs[k,j] <- dnorm(x$Altitude_rate[k], mean = mu[j], sd = sigma[j]) * 
      dbinom(x$Binary_GS[k], size = 1, p = pi[j]) *
      dmultinom(unlist(list_binary_mach[k]),
                size = 1,
                prob = c(phi_1[j], 1-phi_1[j]+phi_2[j], phi_2[j])) 
  }}
  foo <- delta%*%diag(allprobs[1,])
  l <- log(sum(foo))
  phi <- foo/sum(foo)
  for (t in 2:dim(x)[1]){
    foo <- phi%*%Gamma%*%diag(allprobs[t,]) 
    l <- l+log(sum(foo))
    phi <- foo/sum(foo)
  }
  return(-l) }

Summary1 = Flight2 %>% group_by(Flight_phase_text) %>% summarize(Altitude_rate_mean = mean(Altitude_rate), 
                                                                 Altitude_rate_sd = sd(Altitude_rate))



best_min = 10^9
Iterlikelihood_hand = 50
for(h in 1:Iterlikelihood_hand){
  theta.star<-c(qlogis(c(runif(3, 0.9, 0.95), runif(1, 0.01, 0.05), runif(1, 0.8, 0.9), 
                         runif(1, 0.01, 0.05), runif(1, 0.8, 0.9), runif(1, 0.01, 0.05))),
                #mu
                c(runif(1, -10, 10), runif(1, 100, 200), runif(1, 1300, 1400), 
                  runif(1, -1, 1), runif(1, -1200, -1000), runif(1, 20, 30)),
                #sigma
                log(c(runif(1, 40, 60), runif(1, 600, 700), runif(1, 600, 700), 
                      runif(1, 200, 300), runif(1, 600, 700), runif(1, 50, 200))),
                #pi
                qlogis(c(runif(1, 0.9, 0.95), runif(5, 0, 0.01))), 
                #phi_1 rollout 
                qlogis(c(runif(5, 0, 0.01), runif(1, 0.9, 0.95))), 
                #phi_1 takeoff 
                qlogis(c(runif(1, 0, 0.01), runif(1, 0.9, 0.95), runif(4, 0, 0.01)))) 
  
  mod_hand = tryCatch({nlm(mllk,theta.star,x=Flight2,print.level=2, 
                           gradtol = 0.05, iterlim = 100)}, error=function(e) {NA})
  if(any(is.na(mod_hand))){next}
  if(mod_hand$minimum < best_min){
    best_mod_hand = mod_hand
    best_min = mod_hand$minimum
  }
} 

best_mod = best_mod_hand$estimate

Gamma_star <- matrix(NA, nrow=6, ncol=6)
Gamma_star[1,]= c(plogis(best_mod[1]), 1-plogis(best_mod[1]), 0, 0, 0, 0)
Gamma_star[2,]=  c(0, plogis(best_mod[2]), 1-plogis(best_mod[2]), 0, 0, 0)
Gamma_star[3,]=  c(0, 0, plogis(best_mod[3]), 1-plogis(best_mod[3]), 0, 0)
Gamma_star[4,]=  c(0, 0, plogis(best_mod[4]), plogis(best_mod[5]), 1-plogis(best_mod[4])+plogis(best_mod[5]), 0)
Gamma_star[5,]=  c(0, 0, 0, plogis(best_mod[6]), plogis(best_mod[7]), 1 - plogis(best_mod[6]) + plogis(best_mod[7]))
Gamma_star[6,]= c(plogis(best_mod[8]), 0, 0, 0, 0, 1-plogis(best_mod[8]))

mu = best_mod[9:14]
sigma = exp(best_mod[15:20])
pi_1 = plogis(best_mod[21:26])
phi_1 = plogis(best_mod[27:32])
phi_2 = plogis(best_mod[33:38])

lforward <- function(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta){
  n <- dim(x)[1]
  lalpha <- matrix(NA,6,n)
  allprobs <- matrix(1,dim(x)[1],6)
  
  ind = which(!is.na(x$Altitude_rate)& !is.na(x$Binary_GS) & !is.na(x$Binary_GS_deriv))
  
  list_binary_mach = ifelse(x$Binary_GS_deriv == -1, list(c(1, 0, 0)), 
                            ifelse(x$Binary_GS_deriv == 0, list(c(0, 1, 0)), 
                                   list(c(0, 0, 1))))
  
  for(k in 1:length(ind)){
    for (j in 1:6){
      allprobs[k,j] <- dnorm(x$Altitude_rate[k], mean = mu[j], sd = sigma[j]) * 
        dbinom(x$Binary_GS[k], size = 1, p = pi[j]) *
        dmultinom(unlist(list_binary_mach[k]),
                  size = 1,
                  prob = c(phi_1[j], 1-phi_1[j]+phi_2[j], phi_2[j])) 
    }}
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

lbackward <- function(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta){
  n <- dim(x)[1] 
  m = 6
  allprobs <- matrix(1,dim(x)[1],6)
  
  ind = which(!is.na(x$Altitude_rate)& !is.na(x$Binary_GS) & !is.na(x$Binary_GS_deriv))
  
  list_binary_mach = ifelse(x$Binary_GS_deriv == -1, list(c(1, 0, 0)), 
                            ifelse(x$Binary_GS_deriv == 0, list(c(0, 1, 0)), 
                                   list(c(0, 0, 1))))
  
  for(k in 1:length(ind)){
    for (j in 1:6){
      allprobs[k,j] <- dnorm(x$Altitude_rate[k], mean = mu[j], sd = sigma[j]) * 
        dbinom(x$Binary_GS[k], size = 1, p = pi[j]) *
        dmultinom(unlist(list_binary_mach[k]),
                  size = 1,
                  prob = c(phi_1[j], 1-phi_1[j]+phi_2[j], phi_2[j])) 
    }}
  lbeta <- matrix(NA,m,n)
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

state_probs <- function(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta){
  n <- dim(x)[1]
  la <- lforward(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta)
  lb <- lbackward(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta)
  c <- max(la[,n])
  llk <- c+log(sum(exp(la[,n]-c)))
  stateprobs <- matrix(NA,ncol=n,nrow=6)
  for (i in 1:n){
    stateprobs[,i]<-exp(la[,i]+lb[,i]-llk)
  }
  return(stateprobs)
}

Proba_per_state = state_probs(x=Flight2, mu=mu, sigma=sigma, pi=pi_1, phi_1=phi_1,
                               phi_2=phi_2, Gamma = Gamma_star, delta = c(1, 0, 0, 0, 0, 0))
Flight2$proba_state_1 = Proba_per_state[1,]
Flight2$proba_state_2 = Proba_per_state[2,]
Flight2$proba_state_3 = Proba_per_state[3,]
Flight2$proba_state_4 = Proba_per_state[4,]
Flight2$proba_state_5 = Proba_per_state[5,]
Flight2$proba_state_6 = Proba_per_state[6,]

Uncertain = ggplot(data=Flight2, aes(x=time01, y=proba_state_1)) +
  geom_line(color="#9561e2")+
  geom_line(mapping = aes( y=proba_state_2), color="#3490dc")+
  geom_line(mapping = aes( y=proba_state_3), color="#4dc0b5")+
  geom_line(mapping = aes( y=proba_state_4), color="#f6993f")+
  geom_line(mapping = aes( y=proba_state_5), color="#e3342f")+
  geom_line(mapping = aes( y=proba_state_6), color="#ffed4a")+
  ylab("Probability") + xlab("Scaled time")+
  theme_bw()+
  ggtitle("Uncertainty")+theme(plot.title = element_text(face = "bold"))





viterbi<-function(x,mu,sigma,pi,phi_1, phi_2,Gamma,delta){
  n <- dim(x)[1]
  allprobs <- matrix(1,dim(x)[1],6)
  
  ind = which(!is.na(x$Altitude_rate)& !is.na(x$Binary_GS) & !is.na(x$Binary_GS_deriv))

  list_binary_mach = ifelse(x$Binary_GS_deriv == -1, list(c(1, 0, 0)), 
                            ifelse(x$Binary_GS_deriv == 0, list(c(0, 1, 0)), 
                                   list(c(0, 0, 1))))
  
  for(k in 1:length(ind)){
    for (j in 1:6){
      allprobs[k,j] <- dnorm(x$Altitude_rate[k], mean = mu[j], sd = sigma[j]) * 
        dbinom(x$Binary_GS[k], size = 1, p = pi[j]) *
        dmultinom(unlist(list_binary_mach[k]),
                  size = 1,
                  prob = c(phi_1[j], 1-phi_1[j]+phi_2[j], phi_2[j])) 
    }}
  xi <- matrix(0,n,6)
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

Flight2$viterbi = viterbi(x=Flight2, mu=mu, sigma=sigma, pi=pi_1, phi_1=phi_1,
                          phi_2=phi_2, Gamma = Gamma_star, delta = c(1, 0, 0, 0, 0, 0))


Flight2$viterbi = ifelse(Flight2$viterbi == 1, "Taxi", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 2, "Takeoff", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 3, "Climb", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 4, "Cruise", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 5, "Approach", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == 6, "Rollout", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == "Cruise" & Flight2$Altitude <10000 & Flight2$time01 > 0.5, "Approach", Flight2$viterbi)
Flight2$viterbi = ifelse(Flight2$viterbi == "Cruise" & Flight2$Altitude <10000 & Flight2$time01 < 0.5, "Climb", Flight2$viterbi)

Flight2$viterbi <- factor(Flight2$viterbi, levels = c("Taxi", "Takeoff", "Climb", 
                                                      "Cruise", "Approach", "Rollout"))


Predict_viterbi_1 = ggplot(Flight2, aes(x = time01, y = Altitude, color = as.factor(viterbi))) +
  geom_point() + theme_bw() + ylab("Altitude (ft)") + xlab("Scaled time")+
  labs(color='Segmentation') + scale_color_manual(values=c("#9561e2", "#3490dc", "#4dc0b5", "#f6993f", "#e3342f", 
                                                           "#ffed4a"))+
  ggtitle("HMM segmentation")+theme(plot.title = element_text(face = "bold"))
Predict_viterbi_1

Predict_viterbi_2 = ggplot(Flight2, aes(x = time01, y = Ground_speed, color = as.factor(viterbi))) +
  geom_point() + theme_bw() + ylab("Ground speed (kt)") + xlab("Scaled time")+
  labs(color='Segmentation') + scale_color_manual(values=c("#9561e2", "#3490dc", "#4dc0b5", "#f6993f", "#e3342f", 
                                                           "#ffed4a"))
Predict_viterbi_2

Scan_viterbi <- scan(what = character(), text = as.character(Flight2$viterbi))
Unik_viterbi = rle(Scan_viterbi)$values

ggarrange(Truth_1, Truth_2, Predict_viterbi_1, Predict_viterbi_2, ncol=2, nrow=2, align="hv")

ggarrange(Predict_viterbi_1 +rremove("legend"), Uncertain, nrow=2)

################################################################################
#### Performance metrics 
################################################################################

#Invalid transitions 

taxi_to_climb_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Taxi-Climb")
taxi_to_climb_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Taxi-Climb")
taxi_to_cruise_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Taxi-Cruise")
taxi_to_cruise_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Taxi-Cruise")
taxi_to_approach_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Taxi-Approach")
taxi_to_approach_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Taxi-Approach")
taxi_to_rollout_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Taxi-Rollout")
taxi_to_rollout_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Taxi-Rollout")

takeoff_to_taxi_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Takeoff-Taxi")
takeoff_to_taxi_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Takeoff-Taxi")
takeoff_to_cruise_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Takeoff-Cruise")
takeoff_to_cruise_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Takeoff-Cruise")
takeoff_to_approach_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Takeoff-Approach")
takeoff_to_approach_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Takeoff-Approach")
takeoff_to_rollout_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Takeoff-Rollout")
takeoff_to_rollout_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Takeoff-Rollout")

climb_to_takeoff_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Climb-Takeoff")
climb_to_takeoff_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Climb-Takeoff")
climb_to_taxi_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Climb-Taxi")
climb_to_taxi_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Climb-Taxi")
climb_to_approach_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Climb-Approach")
climb_to_approach_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Climb-Approach")
climb_to_rollout_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Climb-Rollout")
climb_to_rollout_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Climb-Rollout")

cruise_to_takeoff_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Cruise-Takeoff")
cruise_to_takeoff_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Cruise-Takeoff")
cruise_to_taxi_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Cruise-Taxi")
cruise_to_taxi_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Cruise-Taxi")
cruise_to_rollout_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Cruise-Rollout")
cruise_to_rollout_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Cruise-Rollout")

approach_to_climb_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Approach-Climb")
approach_to_climb_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Approach-Climb")
approach_to_takeoff_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Approach-Takeoff")
approach_to_takeoff_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Approach-Takeoff")
approach_to_taxi_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Approach-Taxi")
approach_to_taxi_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Approach-Taxi")

rollout_to_approach_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Rollout-Approach")
rollout_to_approach_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Rollout-Approach")
rollout_to_cruise_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Rollout-Cruise")
rollout_to_cruise_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Rollout-Cruise")
rollout_to_climb_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Rollout-Climb")
rollout_to_climb_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Rollout-Climb")
rollout_to_takeoff_HMM = str_count(paste(Unik_viterbi, collapse = "-"), "Rollout-Takeoff")
rollout_to_takeoff_truth = str_count(paste(Unik_Flight_phase_text, collapse = "-"), "Rollout-Takeoff")

#Global accuracy 
dist_truth_HMM = sum(Flight2$viterbi == Flight2$Flight_phase_text)/length(Flight2$Flight_phase_text)

################################################ Climb
#Precision 
Precision_climb_HMM = sum(Flight2$viterbi == "Climb" & Flight2$Flight_phase_text == "Climb") / sum(Flight2$viterbi == "Climb")
#Recall
Recall_climb_HMM = sum(Flight2$viterbi == "Climb" & Flight2$Flight_phase_text == "Climb") / sum(Flight2$Flight_phase_text == "Climb")
#F-1 
F_1_score_climb_HMM = 2*(Precision_climb_HMM * Recall_climb_HMM)/(Precision_climb_HMM+Recall_climb_HMM)

################################################ Cruise
#Precision 
Precision_cruise_HMM = sum(Flight2$viterbi == "Cruise" & Flight2$Flight_phase_text == "Cruise") / sum(Flight2$viterbi == "Cruise")
#Recall
Recall_cruise_HMM = sum(Flight2$viterbi == "Cruise" & Flight2$Flight_phase_text == "Cruise") / sum(Flight2$Flight_phase_text == "Cruise")
#F-1 
F_1_score_cruise_HMM = 2*(Precision_cruise_HMM * Recall_cruise_HMM)/(Precision_cruise_HMM+Recall_cruise_HMM)


################################################ Approach 
#Precision 
Precision_approach_HMM = sum(Flight2$viterbi == "Approach" & Flight2$Flight_phase_text == "Approach") / sum(Flight2$viterbi == "Cruise")

#Recall
Recall_approach_HMM = sum(Flight2$viterbi == "Approach" & Flight2$Flight_phase_text == "Approach") / sum(Flight2$Flight_phase_text == "Approach")

#F-1 
F_1_score_approach_HMM = 2*(Precision_approach_HMM * Recall_approach_HMM)/(Precision_approach_HMM+Recall_approach_HMM)


################################################ Taxi 
#Precision 
Precision_taxi_HMM = sum(Flight2$viterbi == "Taxi" & Flight2$Flight_phase_text == "Taxi") / sum(Flight2$viterbi == "Taxi")

#Recall
Recall_taxi_HMM = sum(Flight2$viterbi == "Taxi" & Flight2$Flight_phase_text == "Taxi") / sum(Flight2$Flight_phase_text == "Taxi")

#F-1 
F_1_score_taxi_HMM = 2*(Precision_taxi_HMM * Recall_taxi_HMM)/(Precision_taxi_HMM+Recall_taxi_HMM)


################################################ Takeoff 
#Precision 
Precision_takeoff_HMM = sum(Flight2$viterbi == "Takeoff" & Flight2$Flight_phase_text == "Takeoff") / sum(Flight2$viterbi == "Takeoff")

#Recall
Recall_takeoff_HMM = sum(Flight2$viterbi == "Takeoff" & Flight2$Flight_phase_text == "Takeoff") / sum(Flight2$Flight_phase_text == "Takeoff")

#F-1 
F_1_score_takeoff_HMM = 2*(Precision_takeoff_HMM * Recall_takeoff_HMM)/(Precision_takeoff_HMM+Recall_takeoff_HMM)

################################################ Rollout 
#Precision 
Precision_rollout_HMM = sum(Flight2$viterbi == "Rollout" & Flight2$Flight_phase_text == "Rollout") / sum(Flight2$viterbi == "Rollout")
#Recall
Recall_rollout_HMM = sum(Flight2$viterbi == "Rollout" & Flight2$Flight_phase_text == "Rollout") / sum(Flight2$Flight_phase_text == "Rollout")
#F-1 
F_1_score_rollout_HMM = 2*(Precision_rollout_HMM * Recall_rollout_HMM)/(Precision_rollout_HMM+Recall_rollout_HMM)

Flight_climb = Flight2[Flight2$Flight_phase_text %in% c('Climb'), ]
Flight_cruise = Flight2[Flight2$Flight_phase_text %in% c('Cruise'), ]
Flight_approach = Flight2[Flight2$Flight_phase_text %in% c('Approach'), ]
Flight_taxi = Flight2[Flight2$Flight_phase_text %in% c('Taxi'), ]
Flight_takeoff = Flight2[Flight2$Flight_phase_text %in% c('Takeoff'), ]
Flight_rollout = Flight2[Flight2$Flight_phase_text %in% c('Rollout'), ]

df_0 = data.frame(Accuracy = dist_truth_HMM, 
                  Recall_climb = Recall_climb_HMM,
                  Recall_cruise = Recall_cruise_HMM,
                  Recall_approach = Recall_approach_HMM,
                  Recall_taxi = Recall_taxi_HMM,
                  Recall_takeoff = Recall_takeoff_HMM,
                  Recall_rollout = Recall_rollout_HMM,
                  
                  Precision_climb = Precision_climb_HMM,
                  Precision_cruise = Precision_cruise_HMM,
                  Precision_approach = Precision_approach_HMM,
                  Precision_taxi = Precision_taxi_HMM,
                  Precision_takeoff = Precision_takeoff_HMM,
                  Precision_rollout = Precision_rollout_HMM,         
                  
                  F_1_score_climb = F_1_score_climb_HMM,
                  F_1_score_cruise = F_1_score_cruise_HMM,
                  F_1_score_approach = F_1_score_approach_HMM,
                  F_1_score_taxi = F_1_score_taxi_HMM,
                  F_1_score_takeoff = F_1_score_takeoff_HMM,
                  F_1_score_rollout = F_1_score_rollout_HMM, 
                  
                  taxi_to_climb_HMM = taxi_to_climb_HMM, 
                  taxi_to_climb_truth = taxi_to_climb_truth,
                  taxi_to_cruise_HMM = taxi_to_cruise_HMM,
                  taxi_to_cruise_truth = taxi_to_cruise_truth,
                  taxi_to_approach_HMM = taxi_to_approach_HMM,
                  taxi_to_approach_truth = taxi_to_approach_truth,
                  taxi_to_rollout_HMM = taxi_to_rollout_HMM,
                  taxi_to_rollout_truth = taxi_to_rollout_truth,
                  
                  takeoff_to_taxi_HMM = takeoff_to_taxi_HMM,
                  takeoff_to_taxi_truth = takeoff_to_taxi_truth,
                  takeoff_to_cruise_HMM = takeoff_to_cruise_HMM,
                  takeoff_to_cruise_truth = takeoff_to_cruise_truth,
                  takeoff_to_approach_HMM = takeoff_to_approach_HMM,
                  takeoff_to_approach_truth = takeoff_to_approach_truth,
                  takeoff_to_rollout_HMM = takeoff_to_rollout_HMM,
                  takeoff_to_rollout_truth = takeoff_to_rollout_truth,
                  
                  climb_to_takeoff_HMM = climb_to_takeoff_HMM,
                  climb_to_takeoff_truth = climb_to_takeoff_truth,
                  climb_to_taxi_HMM = climb_to_taxi_HMM,
                  climb_to_taxi_truth = climb_to_taxi_truth,
                  climb_to_approach_HMM = climb_to_approach_HMM,
                  climb_to_approach_truth = climb_to_approach_truth,
                  climb_to_rollout_HMM = climb_to_rollout_HMM,
                  climb_to_rollout_truth = climb_to_rollout_truth,
                  
                  cruise_to_takeoff_HMM = cruise_to_takeoff_HMM,
                  cruise_to_takeoff_truth = cruise_to_takeoff_truth,
                  cruise_to_taxi_HMM = cruise_to_taxi_HMM,
                  cruise_to_taxi_truth = cruise_to_taxi_truth,
                  cruise_to_rollout_HMM = cruise_to_rollout_HMM,
                  cruise_to_rollout_truth = cruise_to_rollout_truth,
                  
                  approach_to_climb_HMM = approach_to_climb_HMM,
                  approach_to_climb_truth = approach_to_climb_truth,
                  approach_to_takeoff_HMM = approach_to_takeoff_HMM,
                  approach_to_takeoff_truth = approach_to_takeoff_truth,
                  approach_to_taxi_HMM = approach_to_taxi_HMM,
                  approach_to_taxi_truth = approach_to_taxi_truth,
                  
                  rollout_to_approach_HMM = rollout_to_approach_HMM,
                  rollout_to_approach_truth = rollout_to_approach_truth,
                  rollout_to_cruise_HMM = rollout_to_cruise_HMM,
                  rollout_to_cruise_truth = rollout_to_cruise_truth,
                  rollout_to_climb_HMM = rollout_to_climb_HMM,
                  rollout_to_climb_truth = rollout_to_climb_truth,
                  rollout_to_takeoff_HMM = rollout_to_takeoff_HMM,
                  rollout_to_takeoff_truth = rollout_to_takeoff_truth,
                   
                  Nb_transitions = length(Unik_Flight_phase_text)-1, 
                  Nb_transitions_HMM = length(Unik_viterbi)-1, 
                  nb_points_takeoff = nrow(Flight_takeoff), 
                  nb_points_rollout = nrow(Flight_rollout), 
                  nb_points_taxi = nrow(Flight_taxi), 
                  nb_points_cruise = nrow(Flight_cruise), 
                  nb_points_climb = nrow(Flight_climb), 
                  nb_points_approach = nrow(Flight_approach))

