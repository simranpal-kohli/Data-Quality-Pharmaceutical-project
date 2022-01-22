# x=c('forecast','tidyr','dplyr',"ggplot2","tidyverse",'ggcorrplot','GGally','corrplot','VIM','mice',
#     'ggpubr','spc','readxl','mvdalab')
x=c('tidyr','dplyr',"ggplot2","tidyverse",'spc','readxl','mvdalab','reshape2'
    ,"hash","qcc","bestNormalize","mvnormtest","PerformanceAnalytics")
lapply(x, require, character.only = TRUE)

setwd('D:\\OneDrive\\OneDrive\\MSc_Project\\Data')

# Reading the Data without columnms 

dpc_df=read.csv('Product A DPC data.csv')
# View(dpc_df)

dpc_df['Date'] = as.Date(dpc_df$Date,format="%m/%d/%Y")

drops <- c('Date','UnitID','Batch','BatchID_Calc','CurrentBlock')
# dpc_df=dpc_df[ , !(names(dpc_df) %in% drops)]
# Object to int64/float conversion:

# Print the changed datatypes:
# sapply(dpc_df, class)
dpc_df[ , !(names(dpc_df) %in% drops)]=lapply(dpc_df[ , !(names(dpc_df) %in% drops)], as.character)
dpc_df[ , !(names(dpc_df) %in% drops)]=lapply(dpc_df[ , !(names(dpc_df) %in% drops)], as.numeric)
# sapply(dpc_df, class)

# importing in_out dataframe
in_out_df=read_excel('Input vs Output.xlsx')
# View(in_out_df)
in_out_drops <- c('...4','Batch ID...5')
in_out_df=in_out_df[ , !(names(in_out_df) %in% in_out_drops)]
colnames(in_out_df)[1]="Batch"

in_out_df['difference_in']=in_out_df['Input']-in_out_df['Output']
in_out_df['difference_out']=in_out_df['Output']-in_out_df['Input']

# Filtering columns
dpc_df= dpc_df %>% select(-Date,-UnitID,-BatchID_Calc,-PU313)
# colnames(dpc_df)

# Form Batch & Currentblock vector 
exclude_batchid=c(893635, 891813, 893063, 893256, 893737, 923465, 894377, 892524, 955308, 893457, 
                 892914, 893971, 892374, 892151, 894649, 892762)
dpc_df=dpc_df[!(dpc_df$Batch %in% exclude_batchid),]
Xbar_S_Batch_ls=unique(dpc_df['Batch'])
CurrentBlock_unique_ls=unique(dpc_df['CurrentBlock'])

# Reducing the dataframe size with required columns & batchs
highly_correlated_cols=c('Batch','CurrentBlock','AIR101')
highly_correlated_phases=c('EQUILIBRATION_WASH', 'GUANIDINE_RECIRCULATION', 
                           'Pre_use_Rinse_Filter', 'RECIRCULATION_GUANIDINE')
# Batch Id's: 900889, 902676, 902780
highly_correlated_df=dpc_df[dpc_df$CurrentBlock %in% highly_correlated_phases, (names(dpc_df) %in% highly_correlated_cols)]
dim(highly_correlated_df) # 405743      6
unique(highly_correlated_df[highly_correlated_df$CurrentBlock==
                              "RECIRCULATION_GUANIDINE","Batch"])

unique(dpc_df[dpc_df$CurrentBlock=="STORAGE_FILTER","Batch"])

# Preparing data for graphs:
mewma_remove_cols=c('AccTime','AccVolume','BlockTime','BlockVolume','CurrentBlock')
mewma_df=dpc_df[ , !(names(dpc_df) %in% mewma_remove_cols)]

# mewma_df['Batch']=as.factor(mewma_df$Batch)
# mewma_df=mewma_df %>% group_by('Batch') %>% summarise_all("mean")
mewma_df=aggregate(. ~ Batch, data = mewma_df, FUN = mean)
# mewma using mvdalab package

final_graph_df=mewma_df[,-1]
# View(final_graph_df)

final_melt_graph_df <- melt(final_graph_df, measure.vars=colnames(final_graph_df))
ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
  geom_histogram()+
  facet_wrap(variable~.,ncol = 3,scales = "free")



#--- points above UCL:
above_ucl_pts=c(9,10,17,18,19,21,26,27,28,29,30,43,44,45,46,60,61,62,63,64,87,88,89,90,91,92,93,94,95)
# View(mewma_df[above_ucl_pts,])
# View(mewma_df[-above_ucl_pts,])

summary(mewma_df[above_ucl_pts,])
summary(mewma_df[-above_ucl_pts,])

#--- Deal with only 5 columns
correlated_cols=c('AIR101','AIR121pH','AIR131UV','PIR111','TIR101')
final_qcc_df=mewma_df[,correlated_cols]


#--- Transforming data to Normal form
final_melt_graph_df <- melt(final_qcc_df, measure.vars=colnames(final_qcc_df))
ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
  geom_histogram()+
  facet_wrap(variable~.,ncol = 3,scales = "free")

# Normalized_df=data.frame()
# for(cols in colnames(final_qcc_df)){
#   BNobject=bestNormalize(final_qcc_df[cols])
#   Normalized_df=cbind(Normalized_df,cols=BNobject$x.t)
# }
# colnames(final_qcc_df) # "AIR101"   "AIR121pH" "AIR131UV" "PIR111"   "TIR101"
Normalized_df=data.frame("AIR101"=BNobject$x.t)
BNobject=bestNormalize(final_qcc_df$TIR101, allow_orderNorm = FALSE, out_of_sample = FALSE)
Normalized_df=cbind(Normalized_df,"TIR101"=BNobject$x.t)

final_melt_graph_df <- melt(Normalized_df, measure.vars=colnames(Normalized_df))
ggplot(final_melt_graph_df, aes(x=value, fill=variable)) +
  geom_histogram()+
  facet_wrap(variable~.,ncol = 3,scales = "free")

par(mfrow = c(3, 2))
MASS::truehist(Normalized_df$AIR101)
MASS::truehist(Normalized_df$AIR121pH)
MASS::truehist(Normalized_df$AIR131UV)
MASS::truehist(Normalized_df$PIR111)
MASS::truehist(Normalized_df$TIR101)

#-- Merging two dataframes 
plsFit_df=cbind(Normalized_df,"Batch"=mewma_df$Batch)
Merged_normalized_df=merge(x = plsFit_df, y = in_out_df, by = "Batch")
merged_columns=c('AIR101','AIR121pH','PIR111','TIR101','Output','Input',
                 'Yeild','difference_in','difference_out')
Merged_normalized_df=Merged_normalized_df[merged_columns]
#-- Merging two dataframes 


#--- Transforming data to Normal form

#--- MEWMA---
mewma(Normalized_df, phase = 1, lambda = 0.05, conf = c(0.95, 0.99), asymptotic.form = FALSE)
out_greater_in=c(900889, 902493, 902676, 902780)
mewma_df[mewma_df$Batch %in% out_greater_in,]
#--- MEWMA---

#---Adding input & output in Normalized df in order to fit a model of class plsFit
T2_columns=c('AIR101','AIR121pH','PIR111','TIR101','Yeild')
plsFit_model <- plsFit(Yeild ~., scale = TRUE, data = Merged_normalized_df[T2_columns], method = "bidiagpls",
               ncomp = 2, validation = "oob")
summary(plsFit_model)
predict.mvdareg(plsFit_model)
residuals(plsFit_model)

#--- T2---
#'Output','Input','Yeild','difference_in','difference_out'
T2_columns=c('AIR101','AIR121pH','PIR111','TIR101','Yeild')
T2_model <- plsFit(Yeild ~., scale = TRUE, data = Merged_normalized_df[T2_columns],
               ncomp = 2, validation = "oob")
T2(T2_model, ncomp = 2)
summary(T2_model)
#--- T2---

#--- PCA---
PCA_columns=c('AIR101','AIR121pH','PIR111','TIR101')
summary(Merged_normalized_df[PCA_columns])
targets2 <- c(0,6.43,0,0,0)
lsls2 <- c(-2.562, 4.88,-2.2192,-1.977,-2.4444)
usLs2 <- c(2.562, 9.71,2.9515,4.341,2.9004)

lsls2 <- c(-0.666, 5.96,-0.7235,-0.647,-0.5579)
usLs2 <- c(0.666, 6.82,0.6564,0.498,0.6811)

MultCapability(Merged_normalized_df[PCA_columns], lsls = lsls2, usls = usLs2,
               targets = targets2, ncomps = 3)
#--- PCA---

#--- PCA with the NIPALS algorithm---
# Giving normalized data
nipals_normalized <- pca.nipals(Merged_normalized_df[PCA_columns], ncomps = 3, tol = 1e-08)
nipals_normalized_df <- data.frame(nipals_normalized$Scores)
names(nipals_normalized_df) <- paste("Nipals_PCA", 1:3)
# Giving non normalized data
nipals_nonnormalized <- pca.nipals(mewma_df[PCA_columns], ncomps = 3, tol = 1e-08)
nipals_nonnormalized_df <- data.frame(nipals_nonnormalized$Scores)
names(nipals_nonnormalized_df) <- paste("Nipals_PCA", 1:3)
#--- PCA with the NIPALS algorithm---

#--- pcaFit---
PCA_columns=c('AIR101','AIR121pH','AIR131UV','PIR111','TIR101')
PCA_columns=c('AIR101','AIR121pH','PIR111','TIR101')
pc1 <- pcaFit(mewma_df[PCA_columns], scale = TRUE, ncomp = NULL)
pc1
print(pc1) #Model summary
plot(pc1) #MSEP
PE(pc1) #X-explained variance
T2(pc1, ncomp = 2) #T2 plot # c(17,43,49)
Xresids(pc1, ncomp = 2) #X-residuals plot
scoresplot(pc1) #scoresplot variable importance
(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11)) #score contribution # c(17,43,49,87)
plot(SC) #score contribution plot

# on normal data
pc1 <- pcaFit(Merged_normalized_df[PCA_columns], scale = TRUE, ncomp = NULL)
pc1
print(pc1) #Model summary
plot(pc1) #MSEP
PE(pc1) #X-explained variance
T2(pc1, ncomp = 1) #T2 plot # c(17,43,87)
Xresids(pc1, ncomp = 1) #X-residuals plot
scoresplot(pc1) #scoresplot variable importance c(4,17,43)
(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11)) #score contribution # c(17,43,49,87)
plot(SC) #score contribution plot
#--- pcaFit---

#---  Plot of Auto-correlation Funcion
T2_columns=c('AIR101','AIR121pH','AIR131UV','PIR111','TIR101','Yeild')
plsFit_model <- plsFit(Yeild ~., scale = TRUE, data = Merged_normalized_df[T2_columns], method = "bidiagpls",
                       ncomp = 2, validation = "oob")
acfplot(plsFit_model, parm = NULL)
#---  Plot of Auto-correlation Funcion

#--- # Plotting function for Significant Multivariate Correlation
PCA_columns=c('AIR101','AIR121pH','AIR131UV','PIR111','TIR101','Yeild')
mod1 <- plsFit(Yeild ~., scale = TRUE, data = Merged_normalized_df[PCA_columns],
               ncomp = 1, validation = "loo")
smc(mod1)
plot(smc(mod1))
#--- # Plotting function for Significant Multivariate Correlation

#--- EWMA---
par(mfrow=c(1,1))
ewma_q <-  ewma(Normalized_df$TIR101, lambda=0.05)
summary(ewma_q)
#--- EWMA---

#---CUSUM---
cusum_q=cusum(Normalized_df$AIR121pH, sizes = 1,plot = TRUE)
# q <- cusum(Normalized_df[1:10,],sizes = 5)
summary(cusum_q)

q <- cusum(Normalized_df[1:48,], newdata=Normalized_df[49:96,],sizes = 5)
summary(q)
plot(cusum_q, chart.all=FALSE)
#--- CUSUM---

#-----------------Weighted_Means--------------------
#-----------------Pipeline--------------------------
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
T2(pc1, ncomp = 2) #T2 plot # c(17,43,49)

Xresids(pc1, ncomp = 2) #X-residuals plot
scoresplot(pc1) #scoresplot variable importance
(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11)) #score contribution # c(17,43,49,87)
plot(SC) #score contribution plot

#--- T2---
library("ConsRank")
#'Output','Input','Yeild','difference_in','difference_out'
T2_columns=c('AIR101','AIR121pH','PIR112','TIR101','Yeild')
T2_model <- plsFit(Yeild ~., scale = TRUE, data = Weighted_merged_df1[T2_columns],
                   ncomp = 2, validation = "oob")
T2(T2_model, ncomp = 2)

summary(T2_model)
# polyplot(X = c(10,13,10,15,16,9,5,8), L = NULL, Wk = NULL, nobj = 3)
#--- T2---

#--- MEWMA---
MEWMA_columns=c('AIR101','PIR112','TIR101')
pca_mewma <- pcaFit(Weighted_merged_df1[MEWMA_columns], scale = TRUE, ncomp = NULL)
pca_mewma
plot(pca_mewma) # ncomp=2

pca_mewma1 <- pcaFit(Weighted_merged_df1[PCA_columns], scale = TRUE, ncomp = 2)
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

library("ICSNP")
library("Hotelling")
hote=HotellingsT2(Weighted_merged_df1[MEWMA_columns], Y = NULL, mu = NULL, test = "f",na.action = na.fail)


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

OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 23, 24, 25, 26, 28, 35, 42, 43, 49, 54, 67, 69, 
              76, 77, 78, 90, 91)
OOC_sample_phase=c(1, 2, 3, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 22, 26, 28, 30, 33, 34, 
                   36, 37, 38, 39, 40, 42, 43, 44, 46, 47, 48, 50, 51, 55, 61, 66, 69, 71, 79, 85)
OOC_sample_99
Set1_columns=c('AIR101','AIR121pH','PIR112','TIR101')

OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 23, 35, 42,43, 49, 54, 67, 69, 77, 78, 90)
OOC_sample=c(1, 3, 5, 6, 8, 9, 11, 12, 15, 16, 17, 19, 22, 26, 30, 36, 38, 40, 42, 43, 46, 47, 
                   50, 66, 69, 71, 79, 85) # 0.1, 14.78
final_test(OOC_sample)
OOC_sample=c(1, 2, 3, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 22, 26, 28, 30, 33, 34, 36, 37, 38, 39, 
  40, 42, 43, 44, 46, 47, 48, 50, 51, 55, 61, 66, 69, 71, 79, 85) #0.05, 12.87 phase
OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 35, 42, 49, 54, 67) # 0.9, 14.73285
OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 23, 35, 42, 49, 54, 67, 69, 77, 
             78, 90) #0.8, 14.73285

OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 23, 24, 25, 26, 28, 35, 42, 43, 49, 
             54, 67, 69, 76, 77, 78, 90, 91)
OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 23, 24, 25, 26, 28, 35, 42, 43, 49, 50, 
             53, 54, 67, 68, 69, 74, 75, 76, 77, 78, 89, 90, 91)
OOC_sample=c(6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 23, 35, 42, 46, 49, 53, 54, 67, 69, 
             77, 78, 90)
OOC_sample=c(1, 2, 3, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 22, 26, 28, 30, 33, 34, 36, 
             37, 38, 39, 40, 42, 43, 44, 46, 47, 48, 50, 51, 55, 61, 66, 69, 71, 79, 85)
OOC_sample=c(1, 2, 3, 4, 5, 6, 8, 9, 11, 12, 14, 15, 16, 17, 18, 19, 22, 26, 30, 36, 38, 40, 42, 43, 
             46, 47, 50, 55, 66, 69, 71, 73, 79, 85, 90) #lambd=0.1 , h=16.94 per phase-wise
OOC_Sample=c(1, 3, 5, 6, 8, 9, 11, 12, 15, 16, 17, 19, 22, 26, 30, 36, 38, 40, 42, 43, 46, 47, 50, 
             66, 69, 71, 79, 85) #lambd=0.05, h=14.78 per phase-wise
final_test(OOC_sample)

OOC_Yeild=Weighted_merged_df[OOC_sample,'Yeild']
summary(OOC_Yeild)
IIC_Yeild=Weighted_merged_df[-OOC_sample,'Yeild']
summary(IIC_Yeild)

final_test=function(OOC_sample){
  OOC_df=Weighted_merged_df1[OOC_sample,Set1_columns]
  IC_df=Weighted_merged_df1[-OOC_sample,Set1_columns]
  fit <- hotelling.test(OOC_df,IC_df,perm = TRUE)
  if(fit$pval<=0.05){
    print(fit$pval)
    OOC_mshapiro=mshapiro.test(t(OOC_df))
    IC_mshapiro=mshapiro.test(t(IC_df))
    if(OOC_mshapiro$p.value>0.05 | IC_mshapiro$p.value>0.05){
      print(fit)
      print(OOC_mshapiro)
      print(IC_mshapiro)
    }
  }
}




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
