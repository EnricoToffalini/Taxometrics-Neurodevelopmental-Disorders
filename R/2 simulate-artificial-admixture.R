
################################################

library(ggplot2)
library(RTaxometrics)
library(dplyr)
library(mclust)
if(file.exists("R/simulateAdmixtResults.RData")) load("R/simulateAdmixtResults.RData")

################################################

# visual demonstration

set.seed(0)
N = 1e7
X = rnorm(N,0,1)
x1 = scale(X*1.25 + rnorm(N,0,1))
x2 = scale(X*1.25 + rnorm(N,0,1))
x3 = scale(X*1.25 + rnorm(N,0,1))
diagnosis = ifelse(X > quantile(X,0.99), 2, 1)
population = data.frame(x1,x2,x3,diagnosis)

df = population[1:1000,]
df = rbind(df, population[diagnosis==2,][1000:1999,])
df$Recruitment = c(rep("General unselected\npopulation",1000),rep("Added clinical\nsample",1000))
df$Recruitment = relevel(as.factor(df$Recruitment),ref="General unselected\npopulation")

ggplot(df, aes(x=x1,y=x2,color=Recruitment))+
  scale_color_manual(values=c("#132b43","#46a1f7"))+
  geom_point(size=3.5,alpha=.4)+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(breaks=seq(-10,10,1))+
  theme(text=element_text(size=25))+
  xlab("x1 (z-score)")+ylab("x2 (z-score)")

df |> group_by(Recruitment) |>
  summarise(correlation=cor(x1,x2))

################################################

# monte carlo simulation with artificial admixture

niter = 1000
resultsAdmix = data.frame(CCFI.MAMBAC=rep(NA,niter),
                     CCFI.MAXEIG=NA,
                     CCFI.LMode=NA,
                     CCFI.mean=NA,
                     chosenG = NA,
                     gmmBIC1 = NA,
                     gmmBIC2 = NA,
                     time=NA)

N = 1e6
for(i in 1:niter){
  X = rnorm(N,0,1)
  x1 = scale(X*1.25 + rnorm(N,0,1))
  x2 = scale(X*1.25 + rnorm(N,0,1))
  x3 = scale(X*1.25 + rnorm(N,0,1))
  diagnosis = ifelse(X > quantile(X,0.99), 2, 1)
  population = data.frame(x1,x2,x3,diagnosis)
  
  df = population[1:1000,]
  df = rbind(df, population[diagnosis==2,][1000:1999,])
  
  start = Sys.time()
  taxo = RunTaxometrics(df)
  fitGMM = Mclust(df[,c("x1","x2","x3")],G=1:2)
  fitGMM1 = Mclust(df[,c("x1","x2","x3")],G=1)
  fitGMM2 = Mclust(df[,c("x1","x2","x3")],G=2)
  end = Sys.time()
  
  resultsAdmix$CCFI.MAMBAC[i] = taxo$CCFI.MAMBAC
  resultsAdmix$CCFI.MAXEIG[i] = taxo$CCFI.MAXEIG
  resultsAdmix$CCFI.LMode[i] = taxo$CCFI.LMode
  resultsAdmix$CCFI.mean[i] = taxo$CCFI.mean
  resultsAdmix$time[i] = difftime(end,start,units="mins")
  
  resultsAdmix$chosenG[i] = fitGMM$G
  resultsAdmix$gmmBIC1[i] = fitGMM1$bic
  resultsAdmix$gmmBIC2[i] = fitGMM2$bic
  
  print(resultsAdmix[i,])
}
save(resultsAdmix,file="R/simulateAdmixtResults.RData")

################################################

# monte carlo simulation without artificial admixture

niter = 1000
resultsNoAdmix = data.frame(CCFI.MAMBAC=rep(NA,niter),
                            CCFI.MAXEIG=NA,
                            CCFI.LMode=NA,
                            CCFI.mean=NA,
                            chosenG = NA,
                            deltaBIC1 = NA,
                            deltaBIC2 = NA,
                            time=NA)

N = 1e6
for(i in 1:niter){
  X = rnorm(N,0,1)
  x1 = scale(X*1.25 + rnorm(N,0,1))
  x2 = scale(X*1.25 + rnorm(N,0,1))
  x3 = scale(X*1.25 + rnorm(N,0,1))
  diagnosis = ifelse(X > quantile(X,0.99), 2, 1)
  population = data.frame(x1,x2,x3,diagnosis)
  
  df = population[1:2000,]
  
  start = Sys.time()
  taxo = RunTaxometrics(df)
  fitGMM = Mclust(df[,c("x1","x2","x3")],G=1:2)
  fitGMM1 = Mclust(df[,c("x1","x2","x3")],G=1)
  fitGMM2 = Mclust(df[,c("x1","x2","x3")],G=2)
  end = Sys.time()
  
  resultsNoAdmix$CCFI.MAMBAC[i] = taxo$CCFI.MAMBAC
  resultsNoAdmix$CCFI.MAXEIG[i] = taxo$CCFI.MAXEIG
  resultsNoAdmix$CCFI.LMode[i] = taxo$CCFI.LMode
  resultsNoAdmix$CCFI.mean[i] = taxo$CCFI.mean
  resultsNoAdmix$time[i] = difftime(end,start,units="mins")
  
  resultsNoAdmix$chosenG[i] = fitGMM$G
  resultsNoAdmix$gmmBIC1[i] = fitGMM1$bic
  resultsNoAdmix$gmmBIC2[i] = fitGMM2$bic
  
  print(resultsNoAdmix[i,])
}
save(resultsAdmix,file="R/simulateAdmixtResults.RData")

################################################

