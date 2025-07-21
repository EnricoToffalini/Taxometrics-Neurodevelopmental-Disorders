
##########################################

library(RTaxometrics)
library(MASS)
library(semTools)
library(moments)

N = 2000

##########################################

# About respecting assumptions

pop_N = 1e6
X = rnorm(pop_N,0,1)
pop = data.frame(
  x1 = X/2 + rnorm(pop_N,0,1),
  x2 = X/2 + rnorm(pop_N,0,1),
  x3 = X/2 + rnorm(pop_N,0,1),
  x4 = X/2 + rnorm(pop_N,0,1),
  classification = ifelse(X > 1, 2, 1)
)
cor(pop)

df = pop[sample(1:nrow(pop),N),]
(tm = RTaxometrics::RunTaxometrics(df))

##########################################

# Artificial admixture

pop_N = 1e6
X = rnorm(pop_N,0,1)
pop = data.frame(
  x1 = X/2 + rnorm(pop_N,0,1),
  x2 = X/2 + rnorm(pop_N,0,1),
  x3 = X/2 + rnorm(pop_N,0,1),
  x4 = X/2 + rnorm(pop_N,0,1),
  classification = ifelse(X > 1, 2, 1)
)
pop_cases = pop[pop$classification==2,]
pop_controls = pop[pop$classification==1,]

df = rbind(
  pop_cases[sample(1:nrow(pop_cases),N/2),],
  pop_controls[sample(1:nrow(pop_controls),N/2),]
)
(tm_aa = RTaxometrics::RunTaxometrics(df))

##########################################

# Strong redundancy

pop_N = 1e6
X = rnorm(pop_N,0,1)
pop = data.frame(
  x1 = X*3 + rnorm(pop_N,0,1),
  x2 = X*3 + rnorm(pop_N,0,1),
  x3 = X*3 + rnorm(pop_N,0,1),
  x4 = X*3 + rnorm(pop_N,0,1),
  classification = ifelse(X > 1, 2, 1)
)
cor(pop)

df = pop[sample(1:nrow(pop),N),]
(tm_sr = RTaxometrics::RunTaxometrics(df))

##########################################

# Strong correlation

pop_N = 1e6
X = rnorm(pop_N,0,1)
pop = data.frame(
  mvrnorm(pop_N,mu=rep(0,4),Sigma=matrix(.9,4,4)+diag(4)*(1-.9))
)
pop$classification = ifelse(scale(rowMeans(pop)+rnorm(pop_N,0,0.5)) > 2, 2, 1)
cor(pop)

df = pop[sample(1:nrow(pop),N),]
(tm_sc = RTaxometrics::RunTaxometrics(df))

##########################################

# Strong skewness with sum scores

pop_N = 1e6
X = rnorm(pop_N,0,1)
pop = data.frame(
  x1 = rbinom(pop_N,100,pnorm(-1+(X/2 + rnorm(pop_N,0,1)))),
  x2 = rbinom(pop_N,100,pnorm(-1+(X/2 + rnorm(pop_N,0,1)))),
  x3 = rbinom(pop_N,100,pnorm(-1+(X/2 + rnorm(pop_N,0,1)))),
  x4 = rbinom(pop_N,100,pnorm(-1+(X/2 + rnorm(pop_N,0,1)))),
  classification = ifelse(X > 1, 2, 1)
)
cor(pop)
hist(pop$x1); skew(pop$x1)

df = pop[sample(1:nrow(pop),N),]
(tm_ss = RTaxometrics::RunTaxometrics(df))

##########################################


