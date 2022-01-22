x=c('tidyr','dplyr',"ggplot2","tidyverse",'spc','readxl','mvdalab','reshape2'
    ,"hash","smoothmest")
lapply(x, require, character.only = TRUE)


arl.C12=function(muX, sdX, nX, num.sim=1000, dist.type = "normal", upper=TRUE){
  
  seX=sdX/sqrt(nX)  #standard deviation of X-bar
  #set up control chart
  UCL=muX+3*seX
  LCL=muX - 3*seX
  UWL=muX+2*seX
  LWL=muX - 2*seX
  #number sample standard deviations the process has shifted from the target mean
  # ifelse(upper, shifts=seq(from=0, to=3, by=0.2), shifts=seq(from=0, to=-3, by=-0.2))
  if(upper){
    shifts=seq(from=0, to=3, by=0.2)
  }
  else{
    shifts=seq(from=0, to=-3, by=-0.2)
  }
  #set up variables
  run.length=seq(1:num.sim)
  C12.arl=seq(1:length(shifts))
  for (d in shifts) {
    for (i in 1:num.sim) {
      run=0
      signal=0
      count.C2=0
      while (signal==0) {
        if(dist.type=="normal"){samp.X=rnorm(nX, muX+d*seX, sdX)}
        else if(dist.type=="uniform"){
          unif.a=muX+d*seX-sqrt(3*sdX)
          unif.b=muX+d*seX+sqrt(3*sdX)
          samp.X=runif(nX, unif.a, unif.b)}
        
        else if(dist.type=="lognorm"){
            lnorm.mean=log(muX+d*seX)-0.5*log((sdX/(muX+d*seX))^2 + 1)
          lnorm.sd=sqrt(log((sdX/(muX+d*seX))^2 + 1))
          samp.X=rlnorm(nX, lnorm.mean, lnorm.sd)}
        
        else {samp.X=rdoublex(nX, muX+d*seX, sqrt(sdX/2))}
        
        mean.X=mean(samp.X)
        run=run+1
        if(mean.X>UCL|mean.X<LCL) {signal=1}
        if(run==1){mean.x1=mean.X}
        else{
          if(between(mean.X, LCL, LWL))
          {
            if(between(mean.x1, LCL, LWL)|between(mean.x2, LCL, LWL)) {signal=1}
          }
          if(between(mean.X, UWL, UCL))
          {
            if(between(mean.x1, UWL, UCL)|between(mean.x2, UWL, UCL))  {signal=1}
          }
          mean.x2=mean.x1
          mean.x1=mean.X
        }  
        
      }
      run.length[i]=run
    }
    C12.arl[abs(d)*5+1]=mean(run.length)
  }
  C12=cbind(shifts, C12.arl)
  return(C12)
}


#Mean and standard deviation and sample size
muX=5  #mean
sdX=1  #standard deviation
nX=5   #sample size

C12arl.norm=arl.C12(muX, sdX, nX)
C12arl.norm


muX=5  #mean
sdX=1  #standard deviation
nX=5   #sample size
C12arl.unif=arl.C12(muX, sdX, nX, dist.type = "uniform")
C12arl.unif


muX=5  #mean
sdX=1  #standard deviation
nX=5   #sample size
C12arl.logNUpper5=arl.C12(muX, sdX, nX, dist.type = "lognorm", upper = TRUE)
C12arl.logNUpper5


muX=5  #mean
sdX=1  #standard deviation
nX=5   #sample size
C12arl.Dexp=arl.C12(muX, sdX, nX, dist.type = "Dexp")

# write.csv(C12arl.Dexp, file = "C12arlDexp.csv", row.names = FALSE)
# write.csv(C12arl.norm, file = "C12arlNorm.csv", row.names = FALSE)
# write.csv(C12arl.unif, file = "C12arlUnif.csv", row.names = FALSE)
# write.csv(C12arl.logNLower5, file = "C12arllogNLower5.csv", row.names = FALSE)
# write.csv(C12arl.logNUpper5, file = "C12arllogNUpper5.csv", row.names = FALSE)