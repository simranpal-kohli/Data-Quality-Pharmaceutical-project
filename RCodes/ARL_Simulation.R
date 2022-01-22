# NOT RUN {
# Rigdon (1995a), p. 357, Tab. 1
p <- 5
r <- 0.05
h4 <- c(8.37, 9.90, 11.89, 13.36, 14.82, 16.72)
for ( i in 1:length(h4) ) cat(paste(h4[i], "\t", round(mewma.arl(r, h4[i], p, ntype="ra")), "\n"))

h4 <- c(8.37, 9.90, 11.89, 13.36, 14.82, 16.72)
for ( i in 1:length(h4) ) cat(paste(h4[i], "\t", round(mewma.arl(r, h4[i], p, ntype=NULL)), "\n"))

r <- 0.1
h4 <- c(6.98, 8.63, 10.77, 12.37, 13.88, 15.88)
for ( i in 1:length(h4) ) cat(paste(h4[i], "\t", round(mewma.arl(r, h4[i], p, ntype="ra")), "\n"))



# Rigdon (1995b), p. 372, Tab. 1
# }
# NOT RUN {
r <- 0.1
p <- 4
h <- 12.73
for ( sdelta in c(0, 0.125, 0.25, .5, 1, 2, 3) )
  cat(paste(sdelta, "\t",
            round(mewma.arl(r, h, p, delta=sdelta^2, ntype="ra", r=25), digits=2), "\n"))

p <- 5
h <- 14.56
for ( sdelta in c(0, 0.125, 0.25, .5, 1, 2, 3) )
  cat(paste(sdelta, "\t",
            round(mewma.arl(r, h, p, delta=sdelta^2, ntype="ra", r=25), digits=2), "\n"))

p <- 5
# h <- 22.67



# h4=c(8.6336,9.6476,10.0830,10.4405,10.5816,11.8961, 12.3505,12.7143,12.8635,13.8641,
#      14.7077, 16.5965, 18.3935, 20.1223, 21.7980, 23.4307, 25.0278)

h4=list()
h4[[1]] = c(0, 8.6336,  10.8140, 12.7231, 14.5363, 16.2634, 17.9269, 19.5410, 21.1152, 22.6565)
h4[[2]] = c(0,  9.6476,  11.8961, 13.8641, 15.7293, 17.5038, 19.2113, 20.8665, 22.4796, 24.0579)
h4[[3]] = c(0,  10.0830, 12.3505, 14.3359, 16.2170, 18.0063, 19.7276, 21.3960, 23.0217, 24.6119)
h4[[4]] = c(0,  10.3114, 12.5845, 14.5760, 16.4629, 18.2578, 19.9845, 21.6581, 23.2887, 24.8838)
h4[[5]] = c(0,  10.4405, 12.7143, 14.7077, 16.5965, 18.3935, 20.1223, 21.7980, 23.4307, 25.0278)
h4[[6]] = c(0,  10.5152, 12.7880, 14.7818, 16.6711, 18.4687, 20.1982, 21.8747, 23.5082, 25.1062)
h4[[7]] = c(0,  10.5581, 12.8297, 14.8234, 16.7127, 18.5105, 20.2403, 21.9171, 23.5510, 25.1493)
h4[[8]] = c(0,  10.5816, 12.8524, 14.8460, 16.7352, 18.5331, 20.2631, 21.9401, 23.5742, 25.1728)
h4[[9]] = c(0,  10.5932, 12.8635, 14.8570, 16.7463, 18.5442, 20.2743, 21.9515, 23.5858, 25.1846)
lambda <- c(0.05,0.1,0.3,0.5,0.7,0.9)
# h4=16.5965
sdelta = c(0, 0.5,1,1.5,2,2.5,3)
# ntype=c("mc","ra","gl3","gl5")
ntype="gl3"
p=c(3,4)
summary_table<-expand.grid(r = lambda,sdelta=sdelta,ntype=ntype,p=p,h4=NA,ARL=NA)
dim(summary_table)
for (data in 1:dim(summary_table)[1]){
  
  h4_index=round(summary_table[data,"r"]*10)
  if(h4_index==10){
    h4_index=9
  }
  if(h4_index==0){
    h4_index=1
  }
  h4_col=summary_table[data,"p"]-1
  h4_val=h4[[h4_index]][h4_col]
  summary_table[data,"h4"]=h4_val
  summary_table[data,"ARL"]=
    round(mewma.arl(summary_table[data,"r"], h4_val, 
                    p=summary_table[data,"p"], delta=summary_table[data,"sdelta"]^2, 
                    ntype=summary_table[data,"ntype"],r=25), digits=2)
}


#----Simulation based on Lowry suggestion--------------
h4=list()
h4[[2]]=c(5.35,7,10.75,12.34)
h4[[3]]=c(7.08,9,13.10,14.78)
h4[[4]]=c(8.7,10.8,15.16,16.94)
lambda <- c(0.1)
# h4=16.5965
sdelta = c(0, 0.5,1,1.5,2,2.5,3)
# ntype=c("mc","ra","gl3","gl5")
ntype="gl3"
p=c(2,3,4)
# summary_table<-expand.grid(r = lambda,sdelta=sdelta,ntype=ntype)
# dim(summary_table)
dataframe_ls=list()

for (p_val in p){
  new_summary_table<-data.frame()
  for (delta_val in sdelta){
    h4_val_vec=h4[[p_val]]
    for(h4_val in h4_val_vec){
      ARL_val=round(mewma.arl(lambda, h4_val,p=p_val, delta=delta_val^2,ntype=ntype,r=25), digits=2)

      new_summary_table=rbind(new_summary_table,data.frame(r = lambda,sdelta=delta_val,ntype=ntype,
                                         p=p_val,h4=h4_val,ARL=ARL_val))
    }
  }
  dataframe_ls[[p_val]]=new_summary_table
}
View(dataframe_ls[[4]])
View(dataframe_ls[[3]])
#------------------------------------------------------
for ( sdelta in c(0, 0.125, 0.25, .5, 1, 2, 3) )
  cat(paste(sdelta, "\t",
            round(mewma.arl(r, h, p, delta=sdelta^2, ntype="ra", r=25), digits=2), "\n"))
# }
# NOT RUN {
# Runger/Prabhu (1996), p. 1704, Tab. 1
# }
# NOT RUN {
r <- 0.1
p <- 4
H <- 12.73



cat(paste(0, "\t", round(mewma.arl(r, H, p, delta=0, ntype="mc", r=50), digits=2), "\n"))
for ( delta in c(.5, 1, 1.5, 2, 3) )
  cat(paste(delta, "\t",
            round(mewma.arl(r, H, p, delta=delta, ntype="mc", r=25), digits=2), "\n"))
# compare with Fortran program (MEWMA-ARLs.f90) from Molnau et al. (2001) with m1 = m2 = 25
# H4      P     R   DEL  ARL
# 12.73  4.  0.10  0.00 199.78
# 12.73  4.  0.10  0.50  35.05
# 12.73  4.  0.10  1.00  12.17
# 12.73  4.  0.10  1.50   7.22
# 12.73  4.  0.10  2.00   5.19
# 12.73  4.  0.10  3.00   3.42

p <- 20
H <- 37.01
cat(paste(0, "\t",
          round(mewma.arl(r, H, p, delta=0, ntype="mc", r=50), digits=2), "\n"))
for ( delta in c(.5, 1, 1.5, 2, 3) )
  cat(paste(delta, "\t",
            round(mewma.arl(r, H, p, delta=delta, ntype="mc", r=25), digits=2), "\n"))
# compare with Fortran program (MEWMA-ARLs.f90) from Molnau et al. (2001) with m1 = m2 = 25
# H4      P     R   DEL  ARL
# 37.01 20.  0.10  0.00 199.09
# 37.01 20.  0.10  0.50  61.62
# 37.01 20.  0.10  1.00  20.17
# 37.01 20.  0.10  1.50  11.40
# 37.01 20.  0.10  2.00   8.03
# 37.01 20.  0.10  3.00   5.18
# }
# NOT RUN {
# Knoth (2017), p. 85, Tab. 3, rows with p=3
# }
# NOT RUN {
p <- 3
lambda <- 0.05
h4 <- mewma.crit(lambda, 200, p)
benchmark <- mewma.arl(lambda, h4, p, delta=1, r=50)

mc.arl  <- mewma.arl(lambda, h4, p, delta=1, r=25, ntype="mc")
ra.arl  <- mewma.arl(lambda, h4, p, delta=1, r=27, ntype="ra")
co.arl  <- mewma.arl(lambda, h4, p, delta=1, r=12, ntype="co2")
gl3.arl <- mewma.arl(lambda, h4, p, delta=1, r=30, ntype="gl3")
gl5.arl <- mewma.arl(lambda, h4, p, delta=1, r=25, ntype="gl5")

abs( benchmark - data.frame(mc.arl, ra.arl, co.arl, gl3.arl, gl5.arl) )
# }
# NOT RUN {
# Prabhu/Runger (1997), p. 13, Tab. 3
# }
# NOT RUN {
p <- 2
r <- 0.1
H <- 8.64
cat(paste(0, "\t",
          round(mewma.ad(r, H, p, delta=0, type="cycl", ntype="mc", r=60), digits=2), "\n"))
for ( delta in c(.5, 1, 1.5, 2, 3) )
  cat(paste(delta, "\t",
            round(mewma.ad(r, H, p, delta=delta, type="cycl", ntype="mc", r=30), digits=2), "\n"))

# better accuracy
for ( delta in c(0, .5, 1, 1.5, 2, 3) )
  cat(paste(delta, "\t",
            round(mewma.ad(r, H, p, delta=delta^2, type="cycl", r=30), digits=2), "\n"))
# }