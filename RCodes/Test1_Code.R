correlated_cols=c('AIR101','AIR121pH','AIR131UV','PIR111','TIR101')
final_mewma_df=mewma_df[,correlated_cols]

final_melt_graph_df <- melt(final_mewma_df, measure.vars=colnames(final_mewma_df))
ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
  geom_histogram()+
  facet_wrap(variable~.,ncol = 3,scales = "free")

Xbar=rowMeans(final_mewma_df)
Xbar_bar=mean(Xbar)
Xbar_bar

# arl.C12=function(muX, sdX, nX, num.sim=1000, dist.type = "normal", upper=TRUE){
  
#number sample standard deviations the process has shifted from the target mean

#set up variables
for (sample_mean in Xbar){
  
}
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
# }
#------------------------
n <- 150
p0 <- .1
z0 <- n*p0
lambda <- c(1, .51, .165)
hu <- c(27, 22, 18)
p.value <- .1 + (0:20)/200
p.EWMA.arl <- Vectorize(p.ewma.arl, "p")
p.EWMA.arl


ucl <- 0.649169922 ## in-control ARL 370.4 (determined with d.res = 2^14 = 16384)
lambda <- c(0.05,0.1)

n <- 5
p0 <- 0.02
z0 <- n*p0
lambda <- 0.05
ucl=22
res.list <- 2^(1:11)
arl.list <- NULL
for ( res in res.list ) {
  arl <- p.ewma.arl(lambda, ucl, n, p0, z0, d.res=res)
  arl.list <- c(arl.list, arl)
}
cbind(res.list, arl.list)

# CUSUM test
data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)

q <- cusum(diameter[1:25,], decision.interval = 4, se.shift = 1)
summary(q)

q <- cusum(diameter[1:25,], newdata=diameter[26:40,])
summary(q)
plot(q, chart.all=FALSE)

#---------
# Lets consider 10 highly correlated batch's with 6 highly correlated variables.
# & study batchwise analysis
# highly_correlated_cols=c('Batch','CurrentBlock','AIR101','AIR121pH','AIR131UV','PIR111','Pdiff','TIR101')
# highly_correlated_phases=c('Clean_Airtrap_Pre_Use',
#                             'Clean_Column_Tubing_Pre_Use',
#                             'Clean_Inlet_preuse',
#                             'Clean_Outlet_Pre_Use',
#                             'Main',
#                             'Recirculation_NaOH_Pre_Use',
#                             'Recirculation_Prep_NaOH_Preuse',
#                             'SODIUM_CITRATE_PH_5_WASH',
#                             'Skid_Clean_Pre_Use')



# Unique phases:
unique_phases=unique(dpc_df[,"CurrentBlock"])
unique_phases

batch_phase_dict <- hash()
batch_phase_dict <- list()
for(phases in unique_phases){
  batch_phase=length(unique(dpc_df[dpc_df$CurrentBlock==phases,"Batch"]))
  if(phases!=""){
    batch_phase_dict[phases]=batch_phase
  }
  else{
    batch_phase_dict["Empty"]=batch_phase
  }
  
}
batch_phase_dict[order(unlist(batch_phase_dict), decreasing=TRUE)]


data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)
diameter
q <- cusum(diameter[1:25,], decision.interval = 4, se.shift = 1)
summary(q)

data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
model.matrix(mod1)

data(Penta)
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
loadingsplot2D(mod2, comp = c(1, 2))
data(College)
MVcis(College, Vars2Plot = c(1, 2), include.zero = TRUE)

data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
T2(mod1, ncomp = 2)
