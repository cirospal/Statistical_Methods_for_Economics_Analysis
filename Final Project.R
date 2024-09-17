data = read.csv("LifeExpectancyData.csv", sep= ";")
# Import data
Inf_deaths=data$Infant_deaths
n=length(Inf_deaths)
par(mfrow=c(1,1))
# Plot all data 
plot(1:n,Inf_deaths,type="h",ylab="Morti")

df2=subset(data, data$Year == 2015)

Country=df2$Country
Region=df2$Region
Year=df2$Year
Inf_deaths=df2$Infant_deaths
Under5deaths=df2$Under_five_deaths
Adult_mort=df2$Adult_mortality
Alcohol=df2$Alcohol
Hepat=df2$Hepatitis_B
Measles=df2$Measles
BMI=df2$BMI
Polio=df2$Polio
Diphtheria=df2$Diphtheria
HIV=df2$HIV
GDP=df2$GDP
Pop=df2$Population
Thin10_19=df2$Thinness_10_19
Thin5_9=df2$Thinness_5_9
School=df2$Schooling
Devel=df2$Developed
Life_exp=df2$Life_expectancy
lGDP=log(df2[,"GDP"])
df2[,"GDP"]=lGDP
names(df2)[14]="lGDP"

df4 <- subset(df3, select = -c(1,2,3,5,9,12,13))
#Import data 
data1 = data[,c(20, 4:9)]
data2 = data[,c(20, 10:14)]
data3 = data[,c(20, 15:19)]
plot(data1)
plot(data2)
plot(data3)



data4 = df2[,c(20, 4:9)]
data5 = df2[,c(20, 10:14)]
data6 = df2[,c(20, 15:19)]
plot(data4)
plot(data5)
plot(data6)


labs=dimnames(df2)[[2]]

par(mfrow=c(1,1))
for(i in 20){
  boxplot(df2[,i],xlab=labs[i],main="")
}
for(i in 4:18){
  boxplot(df2[,i],xlab=labs[i],main="")
}


for(i in 20){
  mad1=mad(df2[,i])
}
mad1

for(i in 4:18){
  mad2[i]=mad(df2[,i])
}
mad2


lin_mod=lm(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV +  lGDP + Thin5_9 + School + Devel, data=df3)
summary(lin_mod)

boxplot(lin_mod$residuals/2.276,xlab="Residuals non robust")
plot(density(lin_mod$resid/2.276),xlab="Residuals non robust",main="",ylab="Kernel Density")
plot(lin_mod,which=c(1,2), compute.MD=F)

par(mfrow=c(1,4))

lin_modl=lmrob(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV +  lGDP + Thin5_9 + School + Devel, data=df3)
summary(lin_modl)

scale=lin_modl$scale
boxplot(lin_modl$residuals/scale,xlab="Residuals robust")
plot(density(lin_modl$resid/scale),xlab="Residuals robust",main="",ylab="Kernel Density")
plot(lin_modl,which=c(4,2), compute.MD=F)

lin_mod_res=lin_mod$residuals/2.276
par(mfrow=c(3,3))
for(i in 1:9){
  plot(df3[,i],lin_mod_res, xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}
par(mfrow=c(1,3))
for(i in 4:6){
  plot(df3[,i],lin_mod_res, xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}
par(mfrow=c(1,3))
for(i in 7:9){
  plot(df3[,i],lin_mod_res,xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}


lin_modl_res=lin_modl$residuals/scale
par(mfrow=c(3,3))
for(i in 1:9){
  plot(df3[,i],lin_modl_res, xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}
par(mfrow=c(1,3))
for(i in 4:6){
  plot(df3[,i],lin_modl_res, xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}
par(mfrow=c(1,3))
for(i in 7:9){
  plot(df3[,i],lin_modl_res,xlab=labs[i],ylab="Standardized Residuals")
  abline(h=0)
  abline(h=-1.96,lty=2)
  abline(h=1.96,lty=2)
}


run.again=T #Set it to T when running for the first time after opening Rstudio
if(run.again){
  M=1000
  n = length(df3$Life_exp)
  scale=lin_modl$scale
  hat.beta=lin_modl$coef
  hat.beta[2]=0 #value under H_0
  X.mat=cbind(rep(1,n), Alcohol, Polio, Measles, BMI, HIV, lGDP, Thin5_9, School, Devel)
  L_E.predicted.H0 =X.mat%*%cbind(hat.beta)
  myseed = 12343
  rob_t_test=1:M
  df3.sim=df3
  set.seed(myseed)
  for(i in 1:M){
    df3.sim$Life_exp=rnorm(n,L_E.predicted.H0,scale)
    lm.result=lmrob(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV +  lGDP + Thin5_9 + School + Devel, data=df3.sim, init = list(coefficients = hat.beta, scale = scale))
    rob_t_test[i]=coef(summary(lm.result))["Alcohol","Pr(>|t|)"]
  }
}

fit.noNA=!is.na(rob_t_test)
rob_t_test_noNA=rob_t_test[fit.noNA]
emp.size=sum(rob_t_test_noNA<=0.05)/(sum(fit.noNA))
print(paste("Empirical Test Size for the Robust t-test:  ",as.character(emp.size)))


# Prepare the data
labs=dimnames(X)[[2]]
Y = df4[,10]
X = as.matrix(df4[, 1:9])
n=length(Y)
library(glmnet)
lasso.model=glmnet(X, Y, alpha=1)
plot(lasso.model,xvar="lambda",label=T)

set.seed(76543)
lasso_cv10 = cv.glmnet(X, Y, alpha=1)
best_lambda_cv10 = lasso_cv10$lambda.min
par(mfrow=c(1,2))
plot(lasso_cv10) 
plot(lasso.model,xvar="lambda",label=T)
abline(v=log(best_lambda_cv10),col="red")
j=0
for(i in seq(-0.2,-4.4,-0.4)){
  j=j+1
  text(2,i,paste(j,": ",labs[j]))
}
best_model <- glmnet(X, Y, alpha=1, lambda = best_lambda_cv10)
anal=lm(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV + lGDP + Thin5_9 + School+ Devel ,data=df4)
anal.coef=as.matrix(coef(anal))

coef(best_model)
print.output=cbind(coef(best_model),anal.coef)
dimnames(print.output)[[2]]=c("Lasso","OLS")
print(print.output,digit=4)

coef(lin_mod)
coef(lin_modl)
print.output=cbind(coef(lin_mod),coef(lin_modl))
dimnames(print.output)[[2]]=c("OLS","Robust")
print(print.output,digit=4)

prova1=lm(Life_exp~Thin5_9, data=df3)
prova2=lmrob(Life_exp~Thin5_9, data=df3)
coef(prova1)
coef(prova2)
print.output=cbind(coef(prova1),coef(prova2))
dimnames(print.output)[[2]]=c("OLS","Robust")
print(print.output,digit=4)

nunu=lin_modl$residuals/scale
bubu=(nunu<2)

plot(Life_exp~Thin5_9)
abline(lm(Life_exp~Thin5_9), col = "red")
abline(lmrob(Life_exp~Thin5_9), col = "blue")

prova1=lm(Life_exp~HIV, data=df3)
prova2=lmrob(Life_exp~HIV, data=df3)
coef(prova1)
coef(prova2)
print.output=cbind(coef(prova1),coef(prova2))
dimnames(print.output)[[2]]=c("OLS","Robust")
print(print.output,digit=4)

plot(Life_exp~HIV)
abline(lm(Life_exp~HIV), col = "red")
abline(lmrob(Life_exp~HIV), col = "blue")
legend("topright", pch = c(18, 18), c("OLS", "Robust"), col = c("red", "blue"))

set.seed(123)
beta <- function(formula, data, indices){
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$coefficients[6,1])
}
results <- boot(data=df2, statistic=beta, R=10000, formula=Life_exp ~ Alcohol + Polio + Measles + BMI + HIV +  lGDP + Thin5_9 + School + Devel)
boot.ci(results)
reg_model <- lmrob(as.formula("Life_exp ~ Alcohol + Polio + Measles + BMI + HIV + lGDP + Thin5_9 + School + Devel"), df2)
confint(reg_model)
se_boot <- sd(results$t)
se_ols <- summary(reg_model)$coefficients[2,2]
print(sprintf("OLS SE: %f", se_ols))
print(sprintf("BTS SE: %f", se_boot))


df3 <- subset(df2, select = -(1:3) )
corr <- round(cor(df3), 2)
corrplot(corr, method="upper")



# Lasso on the original data

set.seed(1914)
X = model.matrix(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV + lGDP + Thin5_9 + School + Devel - 1, data = df3)
n = nrow(X)
p = ncol(X)
lasso.cv = cv.glmnet(X, df3$Life_exp, intercept = TRUE)
beta = coef(lasso.cv, s = "lambda.min")
index = which(beta != 0)
index = index[-1] # Remove intercept
index = index - 1
# LS on the lasso support (post-lasso)
post_lasso = lm(Life_exp ~ X[,index], data = df3)
X_post_lasso = model.matrix(Life_exp ~ X[,index])
beta_post_lasso = post_lasso$coefficients
sigma = summary(post_lasso)$sigma
beta_simulation = rep(0, p+1)
beta_simulation[which(beta != 0)] = beta_post_lasso
X_simulation = model.matrix(Life_exp ~ Alcohol + Polio + Measles + BMI + HIV + lGDP + Thin5_9 + School + Devel, data = df3)
if(run.again){
  # Simulation setting
  B = 10^3
  beta_lasso = beta_ls = beta_plasso = matrix(0, B, p + 1)
  l2_norms = matrix(NA, B, 3)
  # Start Monte Carlo
  for (i in 1:B){
    set.seed(i)
    y = X_simulation%*%beta_simulation + sigma*rnorm(n)
    # Lasso with CV (min)
    lasso.cv = cv.glmnet(X, y, intercept = TRUE)
    beta_lasso[i,] = as.numeric(coef(lasso.cv, s = "lambda.min"))
    # LS
    beta_ls[i, ] = lm(y ~ X)$coefficients
    # Post-lasso
    index = which(beta_lasso[i,] != 0)
    index = index[-1] # Remove intercept
    index = index - 1
    mod_post_lasso = lm(y ~ X[,index])
    beta_plasso[i,which(beta_lasso[i,] != 0)] = mod_post_lasso$coefficients
  }
}

par(mfrow=c(2,5))
for(i in 1:11){
  boxplot(beta_lasso[,i], beta_ls[,i], beta_plasso[,i], col = "grey",
          names = c("Lasso", "LS", "Post-Lasso"), main = colnames(X_simulation)[i])
  abline(h = beta_simulation[i], col = "red")
}
par(mfrow=c(1,3))
for(i in 4:6){
  boxplot(beta_lasso[,i], beta_ls[,i], beta_plasso[,i], col = "grey",
          names = c("Lasso", "LS", "Post-Lasso"), main = colnames(X_simulation)[i])
  abline(h = beta_simulation[i], col = "red")
}
par(mfrow=c(1,3))
for(i in 7:9){
  boxplot(beta_lasso[,i], beta_ls[,i], beta_plasso[,i], col = "grey",
          names = c("Lasso", "LS", "Post-Lasso"), main = colnames(X_simulation)[i])
  abline(h = beta_simulation[i], col = "red")
}
par(mfrow=c(1,3))
for(i in 10:11){
  boxplot(beta_lasso[,i], beta_ls[,i], beta_plasso[,i], col = "grey",
          names = c("Lasso", "LS", "Post-Lasso"), main = colnames(X_simulation)[i])
  abline(h = beta_simulation[i], col = "red")
}

barplot(table(apply(beta_lasso, 1, how_many_zero))/B,xlab="number of zero slopes", main="Proportion of fitted models with the lasso, among B=10,000, as a function of the number of zero slopes ")