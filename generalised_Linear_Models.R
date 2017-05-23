# SET THE WORKING DIRECTORY 
setwd("folder_path")
dat <- NULL


# # read the data ##
ILPD <- read.csv(file="http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
                 header=FALSE, col.names=c("age", "gender", "TB", "DB", "alkphos",
                                           "sgpt", "sgot", "TP", "alb", "AGratio", "liver"))

dim(ILPD); 
head(ILPD)

##Rate of liver disease

library(MASS)                 # load the MASS package 
liver = ILPD$liver      
liver.freq = table(liver)
liver.relfreq = liver.freq / nrow(ILPD)
liver.relfreq

na_count <-sapply(ILPD, function(y) sum(length(which(is.na(y)))))
na_count  ## 4 NA exists in AG ratio

# IMPUTATION BY METHOD I: MICE
# ---------------------
install.packages("mice")
library(mice)

md.pattern(ILPD)
fit.mice <- mice(ILPD, m=1, maxit = 50, method = 'pmm', seed = 500)
summary(fit.mice)

dat.imputed <- complete(fit.mice,1)
dim(dat.imputed)
ILPD1 <- dat.imputed
na_count1 <-sapply(ILPD1, function(y) sum(length(which(is.na(y)))))
na_count1

str(ILPD1)  ##continuous 5, integer 5, categorical 1
dat <-ILPD1  ## renaming for convenience 
# ---------------------
#EDA and Variable Screening: 
# ---------------------  
# for categorical variable gender
tbl = table(dat$gender, dat$liver) 
tbl
chisq.test(tbl)
a <- t.test(dat$TB,dat$liver)
b <- t.test(dat$DB,dat$liver)
c <- t.test(dat$alkphos,dat$liver)
d <- t.test(dat$sgpt,dat$liver)
e <- t.test(dat$sgot,dat$liver)
f <- t.test(dat$TP,dat$liver)
g <- t.test(dat$alb,dat$liver)
h <- t.test(dat$AGratio,dat$liver)
tbl1 <- rbind(a,b,c,d,e,f,g,h)


# ======================================
# FITTING THE FULL  MODEL 
# =======================================
count(dat,'liver')
x<-dat$liver
as.factor(dat$gender)
dat$liver<- replace(x, x==2, 0)
formula0 <- liver ~ age+factor(gender)+TB+DB+alkphos+sgpt +sgot+TP+alb+AGratio 
fit.full <- glm(formula0, family=binomial, data=dat)
summary(fit.full)
names(summary(fit.full))


# ==============================
# Stepwise Variable Selection
# ==============================
library(MASS)
fit <- stepAIC(fit.full, direction = "both", k=log(nrow(dat)))
summary(fit) 
fit.step <- glm(liver ~ age + DB + sgpt, family=binomial, data=dat)
summary(fit.step)

#Call:
#  glm(formula = liver ~ age + DB + sgpt, family = binomial, data = dat)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0299  -1.1238   0.4760   0.9075   1.3869  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.122642   0.326753  -3.436 0.000591 ***
#  age          0.019936   0.006123   3.256 0.001129 ** 
#  DB           0.657824   0.175644   3.745 0.000180 ***
#  sgpt         0.015096   0.003816   3.957  7.6e-05 ***

# ==========================================================
#  SCAD VIA R PACKAGE ncvreg 
# ==========================================================

install.packages("ncvreg")
library(ncvreg); 
y <- dat$liver
X <- model.matrix(object=~ age + gender + TB + DB + alkphos + sgpt + sgot + TP 
                  + alb + AGratio ,data=dat)

cvfit.SCAD <- cv.ncvreg(X=X,y=y, nfolds=5, family="binomial", penalty="SCAD", 
                        lambda.min=.01, nlambda=100, eps=.01, max.iter=1000) 

plot(cvfit.SCAD)
result.SCAD <- cvfit.SCAD$fit
beta.hat <- as.vector(result.SCAD$beta[-1, cvfit.SCAD$min])
cutoff <- 0
terms <- colnames(X)[beta.hat > cutoff]; terms
formula.SCAD <- as.formula(paste(c("liver ~ 1", terms), collapse=" + "))
fit.SCAD <- glm(formula.SCAD, data = dat, family="binomial")
summary(fit.SCAD)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.1767  -1.1028   0.4277   0.9230   1.5479  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.2232466  0.8236960  -1.485 0.137525    
#age          0.0193461  0.0063611   3.041 0.002356 ** 
#  DB           0.5674897  0.1712871   3.313 0.000923 ***
# alkphos      0.0012515  0.0007961   1.572 0.115931    
#sgpt         0.0136242  0.0038592   3.530 0.000415 ***
#  TP           0.0730263  0.0977757   0.747 0.455138    
#AGratio     -0.5552142  0.3732488  -1.488 0.136878   

# Age DB and SGPT have very smal p-value. Thus from, three predictors  are found to be significant with very small p-value.
# 
fit.pen <- fit.SCAD

# 4 (a) Compute the jackknife predicted probabilities from every model.


# ===================================================
#  Predicted Probabilities Using JACKKNIFE  for fit.full
# ====================================================
install.packages("caret")
library(caret)
y<-dat$liver
n <- NROW(dat)
p.jk <- rep(0, n)
for (i in 1:n){
  print(i)
  fit.full.i <- glm(formula(fit.full), data=dat[-i,], family = "binomial")
  p.jk[i] <- predict(fit.full.i, newdata=dat[i,], type="response")
}
confusionMatrix(sign(p.jk >= 0.5), y)

# ===================================================
#  Predicted Probabilities Using JACKKNIFE  for fit.step
# ====================================================
y<-dat$liver
n <- NROW(dat)
p.jk.step <- rep(0, n)
for (i in 1:n){
  print(i)
  fit.step.i <- glm(formula(fit.step), data=dat[-i,], family = "binomial")
  p.jk.step[i] <- predict(fit.step.i, newdata=dat[i,], type="response")
}

# WITH CUTOFF c=0.5

confusionMatrix(sign(p.jk.step >= 0.5), y)

# ===================================================
#  Predicted Probabilities Using JACKKNIFE  for fit.pen
# ====================================================
y<-dat$liver
n <- NROW(dat)
p.jk.pen <- rep(0, n)
for (i in 1:n){
  print(i)
  fit.pen.i <- glm(formula(fit.pen), data=dat[-i,], family = "binomial")
  p.jk.pen[i] <- predict(fit.pen.i, newdata=dat[i,], type="response")
}

# WITH CUTOFF c=0.5


confusionMatrix(sign(p.jk.pen >= 0.5), y)



# (b) Plot their ROC curves and find their AUC values. Which model provides the largest
# AUC?


# =====================================================
# COMPARING THREE MODELS IN TERMS OF ROC CURVES AND AUC 
# =====================================================

cbind(y, p.jk, p.jk.step,p.jk.pen)

# install.packages("pROC")

library(pROC)  

par(mfrow=c(1, 3), mar=rep(4,4))

# PANEL (a)
roc.step <- plot.roc(y, p.jk.step,  main="(I) ROC using fit.step", percent=TRUE, 
                     print.auc=TRUE, print.auc.cex=1.5, col="black")
# PANEL (b)
roc.pen <- plot.roc(y, p.jk.pen,  main="(b) ROC for fit.pen", percent=TRUE, 
                    print.auc=TRUE, print.auc.cex=1.5, col="green")
#PANEL(c)

roc.full<- plot.roc(y, p.jk,  main="(b) ROC for fit.full", percent=TRUE, 
                    print.auc=TRUE, print.auc.cex=1.5, col="red")




# PABEL (d)    
plot(roc.full, col="black", main="(c) Comparing ROC")  
#Area under the curve: 74.22%

plot(roc.step, col="red", main="(c) Comparing ROC")
# Area under the curve: 74.4%

plot(roc.pen, col="green", main="(c) Comparing ROC")
## Area under the curve: 74.86%


lines(roc.pen, col="green")
#testobj <- roc.test(roc.step, roc.pen,roc.full)  
#text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5), cex=1.5)  
#legend("bottomright", legend=c("step", "pen", "full"), col=c("black","green", "red"), lwd=2)


# 


# 5. Finally, present your final best logistic model and output the 95% confidence intervals for
# coefficients Bj's, as well as their associated odds ratio (i.e., exp(Bj)). Interpret the results
# within the liver disease diagnostic context.



# From the above test, it shows that the final best logistic model is fit.pen
# 
# 
fit.final <- fit.pen

summary(fit.final)

# CONFIDENCE INTERVAL FOR BETA'S
library(MASS)
ci <- confint(fit.final, level = 0.95); 
ci


## odds ratios only
exp(coef(fit.final))


exp(ci)  # CI FOR OR



## odds ratios and 95% CI

exp(cbind(OR = coef(fit.final), confint(fit.final)))

# 

##                 OR      2.5 %   97.5 %
#(Intercept) 0.2942732 0.05774069 1.468742
#age         1.0195344 1.00700932 1.032480
#DB          1.7638338 1.32664094 2.580495
#alkphos     1.0012523 0.99988558 1.003008
#sgpt        1.0137174 1.00680426 1.022120
#TP          1.0757588 0.88846743 1.304583
#AGratio     0.5739493 0.27693513 1.198484



head(ILPD,5)
dim(ILPD)
head(dat)
dim(dat)


