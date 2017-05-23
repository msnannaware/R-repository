dat.i <- read.table(file=
                         "http://www.amstat.org/publications/jse/datasets/baseball.dat.txt",
                       header = F, col.names=c("salary", "batting.avg", "OBP", "runs", "hits",
                                               "doubles", "triples", "homeruns", "RBI", "walks", "strike.outs",
                                               "stolen.bases", "errors", "free.agency.elig", "free.agent.91",
                                               "arb.elig", "arb.91", "name"))
str(dat.i)
head(dat.i)

# =============================
# Missing Values
# =============================

apply(is.na(dat.i),2,sum)  # found no missing values

hist(dat.i$salary,     main="Histogram for salary", xlab="salary", border="blue", col="green" ) 
hist(log(dat.i$salary),     main="Histogram for logsalary", xlab="log salary", border="blue", col="green" ) 

dat <-dat.i
dat$salary <- log(dat$salary)



qqnorm(dat$salary, main="Q-Q Plot of Log(Salary)")
qqline(dat$salary)

colnames(dat)
names(dat)[1] <- "logsalary"
# =============================
# split data
# =============================

install.packages("caTools")
library(caTools)

sp = sample.split(dat, SplitRatio=2/3)
set.seed(111)
table(dat$salary,sp)
train = subset(dat, sp == TRUE)
test = subset(dat, sp == FALSE)
dim(train)
dim(test)

#dat <- scale(dat, center = TRUE, scale = TRUE)  # STANDARDIZE DATA
#dat <- as.data.frame(dat)
colnames(dat)
names(dat)[1] <- "logsalary"
names(train)[1] <- "logsalary"
names(test)[1] <- "logsalary"

# ===========================================
# FIT Hoaglin and Velleman's (HV, 1995) Model
# ===========================================
# ADD THE FOUR PREDICTORS
train$x1 <- train$runs
train$x2 <- train$batting.avg
train$x3 <- train$OBP
train$x4 <- (train$runs)/(train$hits + train$strike.outs)
fit.HV <- lm(logsalary ~ x1 + x2 + x3 + x4, data=train); 
summary(fit.HV)
BIC(fit.HV)
AIC(fit.HV, k=2)

# ============================================
# THE FULL MODEL WITH ALL PREDICTORS INCLUDED
# ============================================

fit.full <- lm(logsalary ~ batting.avg +  OBP +runs+hits+doubles+triples+homeruns+RBI+walks+strike.outs
               +stolen.bases+errors+free.agency.elig+free.agent.91+arb.elig+arb.91, data = train)
BIC(fit.full)

# =====================================================
# ALL SUBSETS - THE bestglm{} PACKAGE (RECOMMENDED)
# =====================================================

install.packages("bestglm")
library(bestglm)
train <-data.frame(train)
formula0 <- logsalary ~ batting.avg +  OBP +runs+hits+doubles+triples+homeruns+RBI+walks+strike.outs+stolen.bases+errors+free.agency.elig+free.agent.91+arb.elig+arb.91-1		# NO QUOTATION MARKS & NO INTERCEPT
y <- train[,names(train)==as.character(formula0)[2]]; # ALTERNATIVE WAY
#y <- train[, all.vars(formula0)[1]]
X <- model.matrix(as.formula(formula0),train)
train.tmp <- as.data.frame(cbind( X, y))	
result.bestBIC <- bestglm(Xy=train.tmp, IC="BIC", intercept=F); 
names(result.bestBIC)
result.bestBIC$BestModels

fit.subset <- result.bestBIC$BestModel
summary(fit.subset)
beta.hat <- fit.subset$"coefficients"; beta.hat
#y <- train[, all.vars(formula0)[1]]
# X <- model.matrix(as.formula(formula0),train)
#train.tmp <- as.data.frame(cbind(1, X, y))	
# =====================
# Stepwise Regression
# =====================
install.packages("MASS")
library(MASS)
fit.step <- stepAIC(fit.full, direction="backward", k=log(nrow(train)))  
fit.step$anova 
summary(fit.step)

# ===============
# LASSO
# ===============

 install.packages("ncvreg"); library(glmnet)
install.packages("glmnet")
library(ncvreg);

#10-FOLD CV FOR SELECTING THE TUNING PARAMETER
cvfit.L1 <- cv.ncvreg(X=X,y=y, nfolds=10, family="gaussian", penalty="lasso", 
                      lambda.min=.005, nlambda=100, eps=.001, max.iter=1000) 
plot(cvfit.L1)
names(cvfit.L1)
beta.hat <- coef(cvfit.L1)  # THE COEFFICIENTS WITH MINIMUM CV ERROR
cutoff <- 0
terms <- names(beta.hat)[abs(beta.hat) > cutoff]
formula.LASSO <- as.formula(paste(c("logsalary ~ ", terms[-1]), collapse=" + "))
fit.L1 <- lm(formula.LASSO, data = train)
summary(fit.L1)




## Prdiction :

##1. for subsets method

predict(fit.subset, test)
SL <- cbind(predict(fit.subset, test), test$logsalary)
subset.SSPE <- sum((SL[, 2]-SL[,1])^2)
subset.SSPE

## 2. for stepwise regression
predict(fit.step, test)
StL <- cbind(predict(fit.step, test), test$logsalary)
step.SSPE <- sum((StL[, 2]-StL[,1])^2)
step.SSPE


## 3. for LASSO
predict(fit.L1, test)
LL <- cbind(predict(fit.L1, test), test$logsalary)
Lasso.SSPE <- sum((LL[, 2]-LL[,1])^2)
Lasso.SSPE

###stepwise has minimum SSPE 29.97411

###final fit

fit.step$coefficients
fit.final <-  lm(logsalary ~ hits + RBI + strike.outs + free.agency.elig +
                   arb.elig, dat)

summary(fit.final)
fit.final$coefficients

# THE x=TRUE ARGUMENT OUTPUTS THE DESIGN MATRIX X AS fit$x
n <- NROW(dat); 
p <- length(coef(fit.final))-1 # NUMBER OF SLOPES


# ================================
# 0. DIFFERENT TYPES OF RESIDUALS
# ================================


# ALTERNATIVE WAY OF GETTING h
infl <- influence.measures(fit.final); 
infl.mat <- as.data.frame(infl$infmat)
h <- infl.mat$hat
sigma <- summary(fit.final)$sigma  	# SIMGA HAT
df.r <- summary(fit.final)$df[2]	# RESIDUAL DF

e <- fit.final$residuals 				# RAW RESIDUAL
z <- e/sigma 					# STANDARDIZED RESIDUAL
r <- e/(sigma*sqrt(1-h))			# STUDENTIZED RESIDUAL
r.jack.0 <- r*sqrt((df.r - 1)/(df.r - r^2)) # JACKKNIFE RESIDUAL

#options(digits=4)
#cbind(y=logsalary, hits , RBI , strike.outs , free.agency.elig ,
 #       arb.elig, yhat=fit.final$fitted.values,h=h, 
## TWO LAST COLUMNS SHOULD BE THE SAME

# =============================
# I. ASSUMPTION CHECKING
# =============================

# OBTAIN THE STUDENTIZED JACKKNIFE RESIDUALS 
r.jack <- rstudent(fit.final)

# NORMALITY
# -----------

par(mfrow=c(1,2),mar=c(8,4,8,4)) 
# The fisrt plot: Histogram 
hist(r.jack, xlab="Jackknife Residual", col="green4",
     main="(a) Histogram") 
# USING PACKAGE {car}
# install.packages("car")
library(car)
# A fancier qq plot for studentized jackknife residuals 
qqPlot(fit.final, pch=19, cex=.8, col="blue", main="(b) Q-Q Plot") 

# THE SHAPIRO-WILKS NORMALITY TEST: A LARGE P-VALUE WOULD JUSTIFY NORMALITY
shapiro.test(r.jack) 

# HOMOSCEDASTICITY
# --------------------
# USING PACKAGE {car}
install.packages("car")
library(car)
# homoscedasticity
# The Breusch-Pagan Test for Non-Constant Error Variance 
ncvTest(fit.final) 
# A LARGE P-VALUE (>0.05) JUSTIFIES EQUAL VARIANCE

# Plot Absolute Jackknife Residuals vs. Fitted values 
# Power Box-Cox Transformation on the response Y is suggested 
par(mfrow=c(1,1),mar=c(4, 4, 4, 4)) 
spreadLevelPlot(fit.final, pch=18, cex=0.5, col="blue",
                main="HV Model on Baseball Salary: Heteroscedasticity")
# IF THE LINES ARE FLAT, THEN EQUAL VARIANCE IS JUSTIFIED. 

# INDEPENDENCE
# -----------------
 library(car)
# Test for Autocorrelated Errors
durbinWatsonTest(fit.final)
# LARGE P-VALUE (>0.05) JUSTIFIES INDEPENDENCE

# LINEARITY
# ------------

# Evaluate Nonlinearity VIA THE component + residual plot (partial residual)
# library(car)
crPlots(fit.final, main="Partial Residual Plots")

# leverage plots or partial regression plot
leveragePlots(fit.final, main="Partial Regression (Leverage) Plots") 

# GLOBAL TEST OF MODEL ASSUMPTIONS 
# -----------------------------------

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit.final, alphalevel = 0.05) 
summary(gvmodel)

# ============================
# II. OUTLIER DETECTION
# ============================

infl <- influence.measures(fit.final); 
infl.mat <- as.data.frame(infl$infmat)
# Cook's Distance
cook.d <- infl.mat$cook.d
infl <- summary(influence.measures(fit.final)); infl 
write.csv(infl, file="Infleunce-Mat.csv", row.names=TRUE)

# library(car)
outlierTest(fit.final) # Bonferonni p-value for most extreme obs

# Plot of Cook's Distance
cutoff <- 4/(n-p-2)
plot(fit.final, which=4, cook.levels=cutoff, col="gray65", lwd=1.5)
points(1:n, cook.d, pch=1, cex=1, col="blue")   

# EXTRACT INFLUETIAL POINTS
dat[cook.d > 0.05, ]   # HIGH COOK'S DISTANCE

# Interactive Plot for Identifying Influential Points
# Press ESC to stop when you are done with identification
influencePlot(fit.final, id.method="identify", 
              col="red", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's d")


# ===============================
# III. CHECK ON MULTICOLINEARITY 
# ===============================

# CONDITION NUMBER 
fit1 <- lm(logsalary ~ hits + RBI + strike.outs + free.agency.elig + arb.elig, dat, x=TRUE); 

kappa(fit1$x);

# WITHOUT INTERCEPT
b <-kappa(lm(logsalary ~ hits + RBI + strike.outs + free.agency.elig + arb.elig, dat, x=TRUE)$x);
b
# COMPUTE VIF USING FUNCTION vif DIRECTLY 
vif(fit1) 

##new data

dat2<- read.csv(file="C://Users/Madhuri Nannaware/Desktop/stat 5474/bb92-test.csv",head=TRUE,sep=",")
str(dat2)
# 95% CI FOR ESTIMATING MEAN RESPONSE
predict1 <- predict(fit.final, newdata = dat2, se.fit = TRUE, interval = "confidence", level = 0.95)

# 95% PI FOR PREDICTING INDIVIDUAL RESPONSE 
predict2 <-predict(fit.final, newdata = dat2, se.fit = TRUE, interval = "prediction", level = 0.95)

 salary1 <- exp(predict1$fit)
 salary2 <- exp(predict2$fit)
 
 plot(predict1$se.fit, type = "h", col = "green", lwd = 16,main = "error plot")
 plot(predict2$se.fit, type = "h", col = "green", lwd = 16,main = "error plot")



salary1
salary2




