Gfunc = function(x, mu, sigma){
  return(exp((- (x-mu)^2) / (2*sigma^2)))
}

Zfunc = function(x, a, b){
  if(x <= a){
    return(1)
  }
  if(x >= a & x<=( (a+b) / 2)){
    return(1 - 2* ( (x-a)/(b-a))^2 ) 
  }  
  if(x >= ( (a+b) / 2) & x<=b){
    return(2* ( (x-b)/(b-a))^2 ) 
  }   
  if(x >= b){
    return(0) 
  }  
}

Sfunc = function(x, a, b){
  if(x <= a){
    return(0)
  }
  if(x >= a & x<=( (a+b) / 2)){
    return(2* ( (x-a)/(b-a))^2 ) 
  }  
  if(x >= ( (a+b) / 2) & x<=b){
    return(1 - 2* ( (x-b)/(b-a))^2  ) 
  }   
  if(x >= b){
    return(1) 
  }  
}

Hgnd = function(eta){Zfunc(x = eta, a = 0, b=200)}
Hlo = function(eta){Gfunc(x = eta, mu = 10000, sigma=10000)}
Hhi = function(eta){Gfunc(x = eta, mu = 35000, sigma=20000)}

RoC0 = function(tau){Gfunc(x = tau, mu = 0, sigma=100)}
RoCplus = function(tau){Sfunc(x = tau, a = 10, b=1000)}
RoCminus = function(tau){Zfunc(x = tau, a = -1000, b = -10)}

Vlo = function(v){Gfunc(x = v, mu = 0, sigma = 50)}
Vmid = function(v){Gfunc(x = v, mu = 300, sigma = 100)}
Vhi = function(v){Gfunc(x = v, mu = 600, sigma = 100)}

Pgnd = function(p){Gfunc(x = p, mu = 1, sigma = 0.2)}
Pclb = function(p){Gfunc(x = p, mu = 2, sigma = 0.2)}
Pcru = function(p){Gfunc(x = p, mu = 3, sigma = 0.2)}
Pdes = function(p){Gfunc(x = p, mu = 4, sigma = 0.2)}
Plvl = function(p){Gfunc(x = p, mu = 5, sigma = 0.2)}

# alt_seq = seq(0, 40000, length.out = 100)
# roc_seq = seq(-4000, 4000, length.out = 100)
# speed_seq = seq(0, 700, length.out = 100)
# 
# plot(alt_seq, sapply(alt_seq, Hgnd), type="l")
# lines(alt_seq, sapply(alt_seq, Hlo), col="red")
# lines(alt_seq, sapply(alt_seq, Hhi), col="blue")
# 
# plot(roc_seq, sapply(roc_seq, RoC0), type="l")
# lines(roc_seq, sapply(roc_seq, RoCplus), col="green")
# lines(roc_seq, sapply(roc_seq, RoCminus), col="orange")
# 
# plot(speed_seq, sapply(speed_seq, Vlo), type="l")
# lines(speed_seq, sapply(speed_seq, Vmid), col="violet")
# lines(speed_seq, sapply(speed_seq, Vhi), col="yellow")
