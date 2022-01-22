x=c('tidyr','dplyr',"ggplot2","tidyverse",'spc','readxl','mvdalab','reshape2'
    ,"hash","qcc","bestNormalize","mvnormtest","ConsRank","ICSNP","Hotelling",
    "PerformanceAnalytics")
lapply(x, require, character.only = TRUE)

setwd('D:\\OneDrive\\OneDrive\\MSc_Project\\Data')

# importing in_out dataframe
in_out_df=read_excel('Input vs Output.xlsx')
# View(in_out_df)
in_out_drops <- c('...4','Batch ID...5')
in_out_df=in_out_df[ , !(names(in_out_df) %in% in_out_drops)]
colnames(in_out_df)[1]="Batch"

in_out_df['difference_in']=in_out_df['Input']-in_out_df['Output']
in_out_df['difference_out']=in_out_df['Output']-in_out_df['Input']

Weighted_mean_df=read.csv('WeightedMean.csv')
Weighted_mean_df=Weighted_mean_df[-1]

#--merge with input_output_df
Weighted_merged_df=merge(x = Weighted_mean_df, y = in_out_df, by = "Batch")
merged_columns=c('AIR101','AIR121pH','PIR112','TIR101','Output','Input',
                 'Yeild','difference_in','difference_out')
Weighted_merged_df1=Weighted_merged_df[merged_columns]

#--- pcaFit---
PCA_columns=c('AIR101','AIR121pH','PIR112','TIR101')
pc1 <- pcaFit(Weighted_merged_df1[PCA_columns], scale = TRUE, ncomp = NULL)
pc1
print(pc1) #Model summary
plot(pc1) #MSEP
PE(pc1) #X-explained variance
t2_ts=T2(pc1, ncomp = 2,verbose = T) #T2 plot # c(17,43,49)
Xresids(pc1, ncomp = 2) #X-residuals plot
scoresplot(pc1) #scoresplot variable importance
(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11)) #score contribution # c(17,43,49,87)
plot(SC) #score contribution plot

ewma_q=ewma(Weighted_merged_df1$TIR101, sizes = 1,plot = TRUE)
summary(ewma_q)
cusum_q=cusum(Weighted_merged_df1$TIR101, sizes = 1,plot = TRUE)
summary(cusum_q)

ewma_q=ewma(Normalized_df$AIR101, sizes = 1,plot = FALSE)
plot(ewma_q, add.stats = TRUE, chart.all = TRUE, 
     label.limits = c("LCL", "UCL"), title="EWMA Chart for variable AIR101", xlab="Batch Sample",
     ylab="Group Summary Statistics", 
     axes.las = 0, digits = getOption("digits"),
     restore.par = TRUE)
summary(ewma_q)
cusum_q=cusum(Normalized_df$AIR121pH, sizes = 1,plot = TRUE)
plot(cusum_q, add.stats = TRUE, chart.all = TRUE, 
     label.limits = c("LCL", "UCL"), title="CUSUM Chart for variable AIR121pH", xlab="Batch Sample",
     ylab="Group Summary Statistics", 
     axes.las = 0, digits = getOption("digits"),
     restore.par = TRUE)
summary(cusum_q)
#--- T2---
#'Output','Input','Yeild','difference_in','difference_out'
T2_columns=c('AIR101','AIR121pH','PIR112','TIR101','Output')
T2_model <- plsFit(Output ~., scale = TRUE, data = Weighted_merged_df1[T2_columns],
                   ncomp = 2, validation = "oob")
summary(T2_model)
T2(T2_model, ncomp = 2)

col.labs <- rep(c("Green", "Blue", "Red"), 20)
t2_pca_resid=Xresids(T2_model, ncomp = 2)
plot(t2_pca_resid, col="blue", pch=4, xlab = "Scores on PC1", 
     ylab="Scores on PC2")

summary(T2_model)
# polyplot(X = c(10,13,10,15,16,9,5,8), L = NULL, Wk = NULL, nobj = 3)
#--- T2---

#--- MEWMA---


MEWMA_columns=c('AIR101','PIR112','TIR101')
pca_mewma <- pcaFit(Weighted_merged_df1[MEWMA_columns], scale = TRUE, ncomp = NULL)
pca_mewma
plot(pca_mewma) # ncomp=2

pca_mewma1 <- pcaFit(Weighted_merged_df1[MEWMA_columns], scale = TRUE, ncomp = 2)
pca_mewma1

mewma_model=mewma(Weighted_merged_df1[MEWMA_columns], phase = 1, lambda = 0.1, conf = c(0.95, 0.99), asymptotic.form = FALSE)
table(mewma_model$Results$Result.99)

mewma_PCA_model=mewma(pca_mewma1$scores, phase = 1, lambda = 0.1, conf = c(0.95, 0.99), asymptotic.form = FALSE)
table(mewma_PCA_model$Results$Result.99)

out_greater_in=c(900889, 902493, 902676, 902780)
mewma_df[mewma_df$Batch %in% out_greater_in,]
#--ANAlysing MEWMA
memwa_ooc=c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,52,53,54,55,56,57,60,75,76,77,78,79,80,81,82,
            83,84,85,86,87,88,89,90,91,92,93,94,95,96)

#--- MEWMA---

#---test mewma: lowry--
X1=c(-1.19,0.12,-1.69,0.30,0.89,0.82,-0.30,0.63,1.56,1.46)
X2=c(0.59,0.90,0.40,0.46,-0.75,0.98,2.28,1.75,1.58,3.05) 
test_df=data.frame("X1"=X1,"X2"=X2,"mean"=(X1+X2)/2)
mewma(test_df, phase = 4, lambda = 0.1, conf = c(0.95, 0.99), asymptotic.form = FALSE)
#---test mewma: lowry--
#----Hotelling--
hote=HotellingsT2(Weighted_merged_df1[MEWMA_columns], Y = NULL, mu = NULL, test = "f",na.action = na.fail)
fit = hotelling.test(.~Number, bottle.df, perm = TRUE)
#---- Hotelling--


#--- Finding the correlation between Yield and 'AIR101','AIR121pH','TIR101'
library(PerformanceAnalytics)

test_cols=c('AIR101','AIR121pH','PIR112','TIR101','Yeild','Input','Output','difference_in')
View(Weighted_merged_df1[test_cols])
Corr_columns=c('AIR101','AIR121pH','PIR112','TIR101','Yeild')

chart.Correlation(Weighted_merged_df1[Corr_columns],
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

#-------------Final Concluding Test-----------------
Set1_columns=c('AIR101','AIR121pH','PIR112','TIR101')
OOC_pts=c(8,9,13,14,15,17,43,42,25,23)
OOC_pts=c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,52,53,54,55,56,57,60,75,76,77,78,79,80,
          81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)
OOC_df=Weighted_merged_df1[OOC_pts,Set1_columns]
IC_df=Weighted_merged_df1[-OOC_pts,Set1_columns]
fit = hotelling.test(OOC_df,IC_df,perm = TRUE)
plot(fit)
plot(fit, col = "lightblue")

df=hotelling.stat(OOC_df,IC_df, shrinkage = TRUE)

#
# final_melt_graph_df <- melt(OOC_df, measure.vars=colnames(OOC_df))
# ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
#   geom_histogram()+
#   facet_wrap(variable~.,ncol = 3,scales = "free")
# 
# 
# final_melt_graph_df <- melt(IC_df, measure.vars=colnames(IC_df))
# ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
#   geom_histogram()+
#   facet_wrap(variable~.,ncol = 3,scales = "free")



data(bottle.df)
bottle.df = subset(bottle.df, Number == 1)
bottle.df$Number = rep(1:2,c(10,10))
fit = hotelling.test(.~Number, bottle.df, perm = TRUE)

plot(fit)
plot(fit, col = "lightblue")

#Batch wise phase wise
OOC_sample=c(9,40,34,19,71,1,12,44,47,43,49,50,3,6,8,26,32,13,15,27,30,37,5,17,42,85,18,20,36,41,46,
             60,64,84,87,91)

#mewma
OOC_sample=c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,52,53,54,55,56,57,60,75,76,77,78,79,80,81,82,83,
             84,85,86,87,88,89,90,91,92,93,94,95,96)

#T2
OOC_sample=c(8,17,43)

#EWMA
OOC_sample=c(9, 17, 23, 24, 25, 26, 27, 28, 29, 30, 40, 41, 42, 43, 44, 58, 59, 60, 61)

#CUSUM
OOC_sample=c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 39, 40, 41, 42, 43, 44, 
             45, 46, 47, 48, 86, 87, 88, 89)

#mewma python with batch and phase
OOC_sample=c(1, 3, 5, 6, 8, 9, 11, 12, 15, 16, 17, 22, 26, 30, 36, 38, 40, 42, 43, 46, 47, 50, 
             66, 69, 71, 79, 85)
batch_sample=c(4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25, 26, 27, 28, 35, 41, 42, 43, 
               46, 49, 50, 53, 54, 67, 68, 69, 71, 73, 74, 75, 76, 77, 78, 89, 90, 91, 95)
intersect(OOC_sample,batch_sample)

OOC_sample=c(6,8, 9, 11, 12, 15, 16, 17, 22, 26, 42, 43, 46, 50, 69, 71)
OOC_sample=c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 22, 26, 28, 30, 33, 34, 36, 37, 38, 
             39, 40, 42, 43, 44, 46, 47, 48, 50, 51, 55, 61, 66, 69, 71, 79, 85)

OOC_sample=c(5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 
             32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 
             57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77)

Set1_columns=c('AIR101','AIR121pH','PIR112','TIR101')
# Set1_columns=c('AIR101','AIR121pH','PIR112')
OOC_df=Weighted_merged_df1[OOC_sample,Set1_columns]
IC_df=Weighted_merged_df1[-OOC_sample,Set1_columns]
fit <- hotelling.test(OOC_df,IC_df,perm = TRUE)
print(fit)

# mewma.arl(l, cE, p, delta=0, hs=0, r=20, ntype=NULL, qm0=20, qm1=qm0)

par(mar=c(1,1,1,1))
plot(fit, col = "lightblue")

final_melt_graph_df <- melt(OOC_df, measure.vars=colnames(OOC_df))
ggplot(final_melt_graph_df, aes(y=value, fill=variable)) +
  geom_boxplot()+
  facet_wrap(variable~.,ncol = 3,scales = "free")

final_melt_graph_df <- melt(IC_df, measure.vars=colnames(IC_df))
ggplot(final_melt_graph_df, aes(y=value, fill=variable)) +
  geom_boxplot()+
  facet_wrap(variable~.,ncol = 3,scales = "free")

mshapiro.test(t(OOC_df))
mshapiro.test(t(IC_df))
det(cov(OOC_df))
det(cov(IC_df))

hote_stat=hotelling.stat(OOC_df,IC_df, TRUE)
hote_stat
HotellingsT2(OOC_df,IC_df)

#
fit <- hotelling.test(OOC_df,IC_df,perm = TRUE)
print(fit)
par(mar=c(1,1,1,1))
plot(fit, col = "lightblue")

#Since p-value < alpha (or F < Fcrit), we reject the null hypothesis, and conclude there is 
#significant difference between the mean vectors for the OOC sample and IC sample, 
#providing evidence that the choosen OOC sample pts or Batch is different than the normal
#performing IC batch.

OOC_Yeild=Weighted_merged_df[OOC_sample,'Yeild']
summary(OOC_Yeild)
IIC_Yeild=Weighted_merged_df[-OOC_sample,'Yeild']
summary(IIC_Yeild)
# With a p value less than 0.05 we can reject the null hypothesis and state that there is 
# multivariate difference between the two samples.


# Correlation between same type of columns from two dataset
a=IC_df$AIR121pH
b=OOC_df$AIR121pH
n <- min(length(a),length(b))
cor(a[seq(n)],b[seq(n)])

Two_column_comparison=data.frame("IC"=a[seq(n)],"OOC"=b[seq(n)])
chart.Correlation(Two_column_comparison,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)


#----- MEWMA_Final_Simulation------
MEWMA_WM_df=Weighted_merged_df1[,c(1,3,4,2,7,8,9,5,6)]
MEWMA_columns=c('AIR101','PIR112','AIR121pH','TIR101')
process=c("normal","pca")
final_mewma_list=list()
for(cols_index in length(MEWMA_columns):2){
  mewma_data=MEWMA_WM_df[1:cols_index]
  mewma_cols=colnames(mewma_data)
  mewma_list=list()
  for(current_process in process){
    if(current_process=="normal"){
      MEWMA_df=mewma_data
    }
    else{
      pca_mewma <- pcaFit(mewma_data, scale = TRUE, ncomp = 2)
      MEWMA_df=pca_mewma$scores
    }
    mewma_cc=mewma(MEWMA_df, phase = 4, lambda = 0.1, 
                      conf = c(0.95, 0.99), asymptotic.form = FALSE)
    mewma_per_99=paste0(round(prop.table(table(mewma_cc$Results$Result.99))*100),'%')
    mewma_per_95=paste0(round(prop.table(table(mewma_cc$Results$Result.95))*100),'%')
    mewma_cc_df=mewma_cc$Results
    OOC_sample=which(mewma_cc_df$Result.99=="Out")
    OOC_df=MEWMA_df[OOC_sample,]
    IC_df=MEWMA_df[-OOC_sample,]
    
    if(length(OOC_sample)<3){
      mshapiro_ooc="Null"
      mshapiro_ic="Null"
      det_ooc="Null"
      det_ic="Null"
      mewma_hotelling_test <- "Null"
    }
    else{
      mshapiro_ooc=mshapiro.test(t(OOC_df))
      mshapiro_ic=mshapiro.test(t(IC_df))
      det_ooc=det(cov(OOC_df))
      det_ic=det(cov(IC_df))
      mewma_hotelling_test <- hotelling.test(OOC_df,IC_df,perm = TRUE)
    }
    
    mewma_list[[current_process]]=list("MEWMA_df"=MEWMA_df,"mewma_cols"=mewma_cols,
                                       "mewma_cc"=mewma_cc,"mewma_per_99"=mewma_per_99,
                                       "mewma_per_95"=mewma_per_95,"mewma_cc_df"=mewma_cc_df,
                                       "OOC_sample"=OOC_sample,"mshapiro_ooc"=mshapiro_ooc,
                                       "mshapiro_ic"=mshapiro_ic,"mewma_hotelling_test"=mewma_hotelling_test)
  }
  final_mewma_list[[cols_index]]=mewma_list
  
}

final_mewma_list[[4]]$pca$mewma_hotelling_test
final_mewma_list[[4]]$pca$mshapiro_ooc
final_mewma_list[[4]]$pca$mshapiro_ic
final_mewma_list[[4]]$pca$mewma_cc # h4=14.73285, and lambd=0.1
final_mewma_list[[4]]$pca$mewma_per_95
final_mewma_list[[4]]$pca$mewma_per_99
final_mewma_list[[4]]$pca$OOC_sample
mewma_scores=final_mewma_list[[4]]$pca$MEWMA_df
mewma_cc_df=as.data.frame(final_mewma_list[[4]]$pca$mewma_cc)
Final_Mewma_df=cbind("PCA_1"=round(mewma_scores[,1],2),"PCA_2"=round(mewma_scores[,2],2),round(mewma_cc_df[1:6],2),mewma_cc_df[7:8])

library(openxlsx)
write.xlsx(Final_Mewma_df, 'Final_Mewma_df.xlsx')
View(Final_Mewma_df)
OOC_sample_mewma=final_mewma_list[[4]]$pca$OOC_sample

Weighted_merged_df1[OOC_sample_mewma,'Batch']

OOC_Yeild=Weighted_merged_df[final_mewma_list[[4]]$pca$OOC_sample,'Input']
summary(OOC_Yeild)
IIC_Yeild=Weighted_merged_df[-final_mewma_list[[4]]$pca$OOC_sample,'Input']
summary(IIC_Yeild)

OOC_sample=OOC_sample_mewma
OOC_sample_T2
# OOC_sample=union(OOC_sample_T2,OOC_sample_mewma)

OOC_sample_mewma

final_test(OOC_sample)
OOC_Yeild=Weighted_merged_df[OOC_sample,'Yeild']
summary(OOC_Yeild)
IIC_Yeild=Weighted_merged_df[-OOC_sample,'Yeild']
summary(IIC_Yeild)

Set1_columns=c('AIR101','AIR121pH','PIR112','TIR101')
OOC_df=Weighted_merged_df1[OOC_sample,Set1_columns]
IC_df=Weighted_merged_df1[-OOC_sample,Set1_columns]

a=IC_df$AIR121pH
b=OOC_df$AIR121pH
n <- min(length(a),length(b))
cor(a[seq(n)],b[seq(n)])

Two_column_comparison=data.frame("IC"=a[seq(n)],"OOC"=b[seq(n)])
chart.Correlation(Two_column_comparison,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)
# c(14 15 16 17 18 19 20 21 22 29 72 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 90 91 92 93 94 95 96)

final_mewma_list[[4]]$pca$mewma_hotelling_test
final_mewma_list[[4]]$pca$MEWMA_df
final_mewma_list[[4]]$pca$mshapiro_ooc
final_mewma_list[[4]]$pca$mshapiro_ic
final_mewma_list[[4]]$pca$mewma_cc
final_mewma_list[[4]]$pca$mewma_per_95
final_mewma_list[[4]]$pca$mewma_per_99
final_mewma_list[[4]]$pca$OOC_sample


mewma_hotelling_test_99=final_mewma_list[[4]]$pca$mewma_hotelling_test
MEWMA_df_99=final_mewma_list[[4]]$pca$MEWMA_df
mshapiro_ooc_99=final_mewma_list[[4]]$pca$mshapiro_ooc
mshapiro_ic_99=final_mewma_list[[4]]$pca$mshapiro_ic
mewma_cc_99=final_mewma_list[[4]]$pca$mewma_cc
mewma_per_95=final_mewma_list[[4]]$pca$mewma_per_95
mewma_per_99=final_mewma_list[[4]]$pca$mewma_per_99
OOC_sample_99=final_mewma_list[[4]]$pca$OOC_sample
# c(14 15 16 17 18 19 20 21 22 43 44 90 92 94 95 96)

mewma_hotelling_test_99
MEWMA_df_99
mshapiro_ooc_99
mshapiro_ic_99
mewma_cc_99
mewma_per_95
mewma_per_99
OOC_sample_99


#----- MEWMA_Final_Simulation------

#----- T2_Final_Simulation------
T2_WM_df=Weighted_merged_df1[,c(1,3,4,2,7,8,9,5,6)]
T2_columns=c('AIR101','PIR112','AIR121pH','TIR101')
process=c("pca","pls_model")
final_T2_list=list()
for(cols_index in length(T2_columns):2){
  T2_data=T2_WM_df[1:cols_index]
  T2_cols=colnames(T2_data)
  T2_list=list()
  for(current_process in process){
    if(current_process=="pls_model"){
      new_T2_data=T2_data
      new_T2_data['Yeild']=T2_WM_df[5]
      T2_df <- plsFit(Yeild ~., scale = TRUE, data = new_T2_data,
                         ncomp = 2, validation = "oob") 
    }
    else{
      pca_T2 <- pcaFit(T2_data, scale = TRUE, ncomp = 2)
      T2_df=pca_T2
    }
    T2_cc=T2(T2_df, ncomp = 2,verbose = T)
    T2_per_99=paste0(round(prop.table(table(T2_cc['Out of Upper CI']))*100),'%')
    T2_per_95=paste0(round(prop.table(table(T2_cc['Out of Lower CI']))*100),'%')
    T2_cc_df=T2_cc
    OOC_sample=which(T2_cc_df['Out of Lower CI']=="Out")
    OOC_df=T2_data[OOC_sample,]
    IC_df=T2_data[-OOC_sample,]
    
    if(length(OOC_sample)<3){
      mshapiro_ooc="Null"
      mshapiro_ic="Null"
      det_ooc="Null"
      det_ic="Null"
      T2_hotelling_test <- "Null"
    }
    else{
      mshapiro_ooc=mshapiro.test(t(OOC_df))
      mshapiro_ic=mshapiro.test(t(IC_df))
      det_ooc=det(cov(OOC_df))
      det_ic=det(cov(IC_df))
      T2_hotelling_test <- hotelling.test(OOC_df,IC_df,perm = TRUE)
    }
    T2_list[[current_process]]=list("T2_df"=T2_df,"T2_cols"=T2_cols,
                                       "T2_cc"=T2_cc,"T2_per_99"=T2_per_99,
                                       "T2_per_95"=T2_per_95,"T2_cc_df"=T2_cc_df,
                                       "OOC_sample"=OOC_sample,"mshapiro_ooc"=mshapiro_ooc,
                                       "mshapiro_ic"=mshapiro_ic,"T2_hotelling_test"=T2_hotelling_test)
  }
  final_T2_list[[cols_index]]=T2_list
}

final_T2_list[[4]]$pca$T2_hotelling_test
final_T2_list[[4]]$pca$mshapiro_ooc
final_T2_list[[4]]$pca$mshapiro_ic
final_T2_list[[2]]$pca$T2_per_95
final_T2_list[[3]]$pca$T2_per_99
final_T2_list[[4]]$pca$OOC_sample

final_T2_list[[4]]$pca$T2_hotelling_test
final_T2_list[[4]]$pca$mshapiro_ooc
final_T2_list[[4]]$pca$mshapiro_ic
final_T2_list[[4]]$pca$T2_cc
final_T2_list[[4]]$pca$T2_per_95
final_T2_list[[4]]$pca$T2_per_99
final_T2_list[[4]]$pca$OOC_sample
OOC_sample_T2=final_T2_list[[4]]$pca$OOC_sample
OOC_sample_T2
#----- T2_Final_Simulation------
