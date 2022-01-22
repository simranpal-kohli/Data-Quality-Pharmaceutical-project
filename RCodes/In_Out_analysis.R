x=c('tidyr','dplyr',"ggplot2","tidyverse",'spc','readxl','mvdalab','reshape2'
    ,"hash","qcc","bestNormalize","mvnormtest","ConsRank","ICSNP","Hotelling")

x=c('tidyr','dplyr',"ggplot2","tidyverse",'spc','readxl','mvdalab','reshape2'
    ,"hash","qcc","bestNormalize","mvnormtest","PerformanceAnalytics")
lapply(x, require, character.only = TRUE)


setwd('D:\\OneDrive\\OneDrive\\MSc_Project\\Data')

# importing in_out dataframe
in_out_df=read_excel('Input vs Output.xlsx')
full_zero_df=read.csv('Full_Zero_df.csv')
# View(in_out_df)
in_out_drops <- c('...4','Batch ID...5')
in_out_df=in_out_df[ , !(names(in_out_df) %in% in_out_drops)]
colnames(in_out_df)[1]="Batch"

in_out_df['difference_in']=in_out_df['Input']-in_out_df['Output']
in_out_df['difference_out']=in_out_df['Output']-in_out_df['Input']
View(in_out_df)

x <- in_out_df$Yeild
h<-hist(x, breaks=10, xlab="Yield")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

hist(x,probability=TRUE)
lines(density(energy),col="red")

hist(in_out_df$Yeild)

x=full_zero_df$TIR101
bestNormalize(x, allow_orderNorm = FALSE, out_of_sample = FALSE)

(arcsinh_obj <- arcsinh_x(x))
(boxcox_obj <- boxcox(x))
(yeojohnson_obj <- yeojohnson(x))
(orderNorm_obj <- orderNorm(x))

(BNobject <- bestNormalize(x))

par(mfrow = c(2,2))
MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)

par(mfrow = c(1,2))
MASS::truehist(BNobject$x.t, 
               main = paste("Best Transformation:", 
                            class(BNobject$chosen_transform)[1]), nbins = 12)
plot(xx, predict(BNobject, newdata = xx), type = "l", col = 1, 
     main = "Best Normalizing transformation", ylab = "g(x)", xlab = "x")

boxplot(log10(BNobject$oos_preds), yaxt = 'n')
axis(2, at=log10(c(.1,.5, 1, 2, 5, 10)), labels=c(.1,.5, 1, 2, 5, 10))

Normalized_zero_df=read.csv('Normalized_Zero_df.csv')
Corr_columns=c("AIR101","AIR121pH","PIR112","TIR101" )
chart.Correlation(Normalized_zero_df[Corr_columns],
                  method="pearson",
                  histogram=TRUE,
                  pch=16)
greater_batch=c(900889, 902493, 902676, 902780)
greater_batch=c(900889, 902676, 902780,901002)
#6 :900889, 102 18:902676, 102 19:902780, 101 7:901002 100
#43: 922661,90, 17: 902319, 9: 901300, 68: 944867, 70: 945225, 2: 899409
in_out_df[in_out_df$Batch %in%greater_batch,]

Greater_df=Normalized_zero_df[Normalized_zero_df$Batch==921747,Corr_columns]
chart.Correlation(Greater_df,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)
