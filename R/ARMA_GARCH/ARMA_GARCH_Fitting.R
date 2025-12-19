#----------- Fitting in sample-------------------------------------
#_________________________________SSMI________________________________________

  price <- px_list$SSMI$SSMI.Adjusted
  ret <- na.omit((suppressWarnings(diff(log(price)))))

  # In and out of sample definieren
  in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)
  
  ret_out<-ret[paste(in_sample,"/",sep="")]
  ret_in  <- ret[!index(ret) %in% index(ret_out)]
  

  #------In Sample------
  par(mfrow = c(2,1))
  plot(price[paste("/",in_sample,sep="")], main = "In Sample SMI Price")
  plot(ret_in, main = "In Sample SMI Log_Returns")

  acf(ret_in)
  pacf(ret_in)
  
  fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
                include.delta=F,include.mean=T, cond.dist = "norm")
  s<-summary(fit)
  s$stat_tests #ALLE TESTS OK, except distribution assumptions
  s$ics #BIC = -6.653617
  
  fit2<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
                include.delta=F,include.mean=T, cond.dist = "sstd")
  s2<-summary(fit2)
  s2$stat_tests #ALLE TESTS OK, except distribution assumptions
  s2$ics #BIC = -6.700768 

  fit3<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
                 include.delta=F,include.mean=T,  cond.dist = "sstd")
  s3<-summary(fit3)
  s3$stat_tests #ALL TESTS OK, except distribution assumption
  s3$ics #BIC =  -6.703647

  fit4<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
                 include.delta=F,include.mean=T,  cond.dist = "sstd")
  s4<-summary(fit4)
  s4$stat_tests #ALL TESTS OK, except distribution assumptions
  s4$ics #BIC = -6.703622



  #Based on BIC and Diagnostics fit4 is chosen. Although AR(1) was prefered over
  #of MA(1) due to simplicity of prediction and mu was included even though 
  #it isnt necessary due to expierences that it leads to less trades

  # Compute model residuals u_t
  u<-fit4@residuals/fit4@sigma.t
  par(mfrow = c(2,1))
  acf(u) #Mean level Model is sufficient
  acf(u^2) #No volatility clusters in residuals U_t

  #----SSMI Model----
  ARMA_GARCH_SSMI <- fit4

  
#_________________________________GSPC________________________________________

price <- px_list$GSPC$GSPC.Adjusted
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T)

s<-summary(fit)
s$stat_tests #ALL TESTS OK, except distribution assumptions
s$ics #BIC = -6.653617


fit1<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #ALL TESTS OK, except distribution assumptions
s1$ics #BIC = -6.700768

fit2<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #ALL TESTS OK, except distribution assumptions
s2$ics #BIC = -6.703622


fit3<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s3<-summary(fit3)
s3$stat_tests #ALL TESTS OK, except distribution assumptions
s3$ics #BIC = -6.703647


u<-fit2@residuals/fit2@sigma.t
par(mfrow = c(2,1))
acf(u) #Mean level Model is sufficient
acf(u^2)



#_________________________________IXIC________________________________________

price <- px_list$IXIC$IXIC.Adjusted
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T)
s<-summary(fit)
s$stat_tests #ALL TESTS OK, except distribution assumptions
s$ics #BIC = -6.428746


fit1<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Mean level Residuals analysis a little close to significant, except distribution assumptions
s1$ics #BIC = -6.491242



fit2<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #ALL TESTS OK, except distribution assumptions 
s2$ics #BIC = -6.484157

fit3<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s3<-summary(fit3)
s3$stat_tests #ALL TESTS OK, except distribution assumptions 
s3$ics #BIC = -6.485869


#_________________________________GDAXI________________________________________

price <- px_list$GDAXI$GDAXI.Adjusted
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T)
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -6.168788


fit1<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -6.197873

fit2<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests
s2$ics #BIC = -6.200273

fit3<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s3<-summary(fit3)
s3$stat_tests
s3$ics #BIC = -6.200273

#Model fit2 is chosen based on ACF and interpretability of model


#_________________________________BTC-USD________________________________________

price <- px_list$`BTC-USD`$`BTC-USD.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
    #later date chosen beacause of BTC change in more recent years
ret_in  <- ret[paste("2017-01-03", in_sample, sep="::")] 

#------In Sample------
par(mfrow = c(2,1))
plot(price[index(ret_in)], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T)
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -3.412399


fit1<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -3.552815

fit2<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Model diagnostics insuficient


#Model fit1 is chosen based on ACF and interpretability of model



#_________________________________CL=F________________________________________

price <- px_list$`CL=F`$`CL=F.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -5.169296


fit1<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -5.172341

fit2<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Diagnostics Okay except distribution
s2$ics #BIC = -5.172293

#_________________________________NG=F________________________________________

price <- px_list$`NG=F`$`NG=F.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T, cond.dist = "sstd")
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -4.464894


fit1<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -4.466539

fit2<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Diagnostics Okay except distribution
s2$ics #BIC = -4.466434

#_________________________________GC=F________________________________________

price <- px_list$`GC=F`$`GC=F.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T, cond.dist = "sstd")
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -6.529370


fit1<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -6.532753

fit2<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Diagnostics Okay except distribution
s2$ics #BIC = -6.532699


#_________________________________SI=F________________________________________

price <- px_list$`SI=F`$`SI=F.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T, cond.dist = "sstd")
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -5.370043


fit1<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -5.373408

fit2<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Diagnostics Okay except distribution
s2$ics #BIC = -5.373311 


#_________________________________SI=F________________________________________

price <- px_list$`HG=F`$`HG=F.Adjusted`
ret <- na.omit((suppressWarnings(diff(log(price)))))

# In and out of sample definieren
in_sample<-min(common_idx) #we want to then be out of sample at min(common_idx)

ret_out<-ret[paste(in_sample,"/",sep="")]
ret_in  <- ret[!index(ret) %in% index(ret_out)]

#------In Sample------
par(mfrow = c(2,1))
plot(price[paste("/",in_sample,sep="")], main = "In Sample GSPC Price")
plot(ret_in, main = "In Sample GSPC Log_Returns")

acf(ret_in)
pacf(ret_in)

fit<-garchFit(~arma(1,1)+garch(1,1),data=ret_in,delta=2,
              include.delta=F,include.mean=T, cond.dist = "sstd")
s<-summary(fit)
s$stat_tests #Diagnostics Okay except distribution
s$ics #BIC = -5.796220


fit1<-garchFit(~arma(1,0)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s1<-summary(fit1)
s1$stat_tests #Diagnostics Okay except distribution
s1$ics #BIC = -5.799502

fit2<-garchFit(~arma(0,1)+garch(1,1),data=ret_in,delta=2,
               include.delta=F,include.mean=T, cond.dist = "sstd")
s2<-summary(fit2)
s2$stat_tests #Diagnostics Okay except distribution
s2$ics #BIC = -5.799451 

