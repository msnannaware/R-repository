# SET THE WORKING DIRECTORY 
setwd("path")
install.packages("kernlab")

library(kernlab)

dat <- data(spam)
dim(spam) # 4601   58

na_count <-sapply(spam, function(y) sum(length(which(is.na(y)))))
na_count  ## no missing values

count(spam, 'type')
#type freq
#1 nonspam 2788
#2    spam 1813

as.factor(spam$type)
spam$type <- ifelse(spam$type == "spam", 1, 0)

summary(spam)
plot(spam$capitalTotal,type ="h")
str(spam)  
spam$type<-factor(spam$type)

dim(spam)  #  4601*58

# (b) each variable using numerical and graphical EDA techniques. For example, what is the
# percentage of spam emails? What are types (categorical or continuous) of the inputs? Are
# there any peculiar features for any variable that we should pay attention to? Don't present
# any R output for this part unless really necessary. Instead, summarize your findings in
# concise language.

## 55 continuous,2 numeric & 1 character variable (type) which converted to binary

spam$num650
table(spam$type)  ## nonspam    spam 
                   # 2788    1813 
1813/4601

sort(spam$charExclamation,decreasing=FALSE)

summary(spam$charExclamation)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2691  0.3150 32.4800

table(spam$charExclamation)
table(spam$type,useNA="ifany")[1]*(100/4601)
#nonspam 
#60.59552
#install.packages("Hmisc")
#library(Hmisc)
#correlations <- rcorr(as.matrix(spam))



table(spam$type,useNA="ifany")[2]*(100/4601)
#spam 
#39.40448 

sapply(spam, function(x) length(unique(x)))


# (c) Randomly divide your data sets into the training sample and for the test sample with a
# ratio of 2:1. We shall use the training sample to train a number of models and then use
# the test sample to compare them.



##(2/3)rd of the sample size
smp_size <- floor((2/3) * nrow(spam))

## set the seed 
set.seed(123)
train_ind <- sample(seq_len(nrow(spam)), size = smp_size)

train <- spam[train_ind, ]  # [1] 3067   58
test <- spam[-train_ind, ]  #[1] 1534   58
dim(train)
head(train)
table(train$type)
#nonspam    spam 
#1872    1195 
table(test$type)
#nonspam    spam 
#916     618 
str(train)

train$type<-as.factor(train$type)

names(spam)

# ======================================
#   LOGISTIC REGRESSION 
# ======================================


is.factor(spam$type)

contrasts(spam$type)  
#spam
#nonspam    0
#spam       1

spam$type <- ifelse(spam$type=="spam", 1, 0);

spam$type<-factor(spam$type)

table(train$type)

#nonspam    spam 
#1872    1195
install.packages("ncvreg")
library(ncvreg);
dat <- data.frame(train)
y <- train$type
X <- model.matrix(object=~ make + address + all + num3d + our + over + remove + internet
                  + order + mail + receive + will + people + report +addresses
                  + free + business + email + you +credit +your + font + num000
                  +money +hp + hpl + george + num650 + lab + labs +telnet + num857
                  + data + num415 +num85 +technology + num1999 + parts + pm + direct +
                    cs + meeting + original +project +re + edu + table + conference + charSemicolon 
                  +charRoundbracket +charSquarebracket + charExclamation + charDollar +charHash + 
                    capitalLong + capitalTotal ,train)
cvfit.SCAD <-cv.ncvreg(X=X,y=y, nfolds=5, family="binomial", penalty="SCAD", 
                       lambda.min=.01, nlambda=100, eps=.01, max.iter=1000) 
# USING THE ARGUMENT penalty="MCP" TO CHOOSE AMONG DIFFERENT PENALTY FUNCTIONS
plot(cvfit.SCAD)
result.SCAD <- cvfit.SCAD$fit
beta.hat <- as.vector(result.SCAD$beta[-1, cvfit.SCAD$min])
cutoff <- 0
terms <- colnames(X)[abs(beta.hat) > cutoff]; terms
formula.SCAD <- as.formula(paste(c("type ~ 1", terms), collapse=" + "))
fit.SCAD <- glm(formula.SCAD, data = train, family="binomial")
summary(fit.SCAD)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7401  -0.1924   0.0000   0.1151   4.9980 

#Null deviance: 4101.1  on 3066  degrees of freedom
#Residual deviance: 1207.0  on 3020  degrees of freedom
#AIC: 1301

#Number of Fisher Scoring iterations: 13

# MAKE PREDICTIONS
predReg <-  predict(fit.SCAD, test, interval = "confidence")
install.packages("pROC")
library(pROC)
library("ROCR")    
Rpred <- prediction(predReg, test$type)    
perf <- performance(Rpred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve-regression ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1)

y <- ifelse(test$type=="spam", 1, 0)
require(verification)
AUC.1tree <- roc.area(obs=y, pred=predReg)$A; 
AUC.1tree   ##[1] 0.9734612


#===============================================
#ONE SINGLE DECISION TREE
#===============================================


library(rpart)
control0 <- rpart.control(minsplit=10, minbucket=3, maxdepth=15,
                          cp=0, maxcompete=4, 
                          maxsurrogate=5, usesurrogate=2, surrogatestyle=0,  		# SURROGATE SPLITS FOR MISSING DATA
                          xval=10)									# SET THE VALUE V FOR V-FOLD CROSS VALIDATION
tre0 <- rpart(type~ make + address + all + num3d + our + over + remove + internet
              + order + mail + receive + will + people + report +addresses
              + free + business + email + you +credit +your + font + num000
              +money +hp + hpl + george + num650 + lab + labs +telnet + num857
              + data + num415 +num85 +technology + num1999 + parts + pm + direct +
                cs + meeting + original +project +re + edu + table + conference + charSemicolon 
              +charRoundbracket +charSquarebracket + charExclamation + charDollar +charHash + 
                capitalLong + capitalTotal,
              data=train, method='class', control=control0,
              parms=list(split='information'))
plot(tre0)
plotcp(tre0)
dev.print(postscript, 'spam-fig1.ps', paper='special', height=6, width=10)
printcp(tre0)

btre <- prune(tre0, cp=.0028)
plot(btre, uniform=T, compress=T, margin=.05)
text(btre, use.n=T)
dev.print(postscript, 'spam-fig2.ps', paper='special', height=8.5, width=11)

print(btre, cp=.05)
#Root node error: 1195/3067 = 0.38963

#n= 3067 

#CP nsplit rel error  xerror     xstd
#1  0.47029289      0  1.000000 1.00000 0.022600
#2  0.13640167      1  0.529707 0.55397 0.019066
#3  0.04518828      2  0.393305 0.41423 0.017050
#4  0.04435146      3  0.348117 0.38745 0.016592
#5  0.02092050      4  0.303766 0.30293 0.014952
#6  0.01255230      5  0.282845 0.27699 0.014380
#7  0.01004184      6  0.270293 0.26527 0.014108
#8  0.00864714      8  0.250209 0.25356 0.013828
#9  0.00753138     12  0.208368 0.24686 0.013664
#10 0.00585774     13  0.200837 0.23013 0.013240
#11 0.00446304     14  0.194979 0.22845 0.013197
#12 0.00418410     18  0.174059 0.22762 0.013175
#13 0.00376569     21  0.158996 0.22762 0.013175
#14 0.00334728     23  0.151464 0.22259 0.013043
#15 0.00251046     25  0.144770 0.20753 0.012634
#16 0.00209205     27  0.139749 0.19498 0.012279
#17 0.00167364     30  0.133054 0.19582 0.012303
#18 0.00125523     41  0.113808 0.20084 0.012446
#19 0.00111576     53  0.098745 0.20418 0.012541
#20 0.00095637     56  0.095397 0.21004 0.012704
#21 0.00083682     63  0.088703 0.20251 0.012494
#22 0.00041841     68  0.084519 0.20251 0.012494
#23 0.00010460     72  0.082845 0.20502 0.012564
#24 0.00000000     80  0.082008 0.20921 0.012681

# TRAINING ERROR
btre.train.class <- predict(btre, type='class')
table(predicted=btre.train.class, actual=train$type)
#          actual
#predicted nonspam spam
#nonspam    1794   95
#spam         78 1100

# TEST ERROR
btre.test.class <- predict(btre, type='class', newdata=test)
table(predicted=btre.test.class, actual=test$type)
#            actual
#predicted nonspam spam
#nonspam     860   85
#spam         56  533


# PREDICTED PROBABILITY FOR MAKING ROC CURVE
btre.test.p <- predict(btre, type='prob', newdata=test)[,2]
a<-table(predicted=btre.test.p, actual=test$type)

library("ROCR")    
pred <- prediction(btre.test.p, test$type)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1)

#y <- ifelse(test$type=="spam", 1, 0)

AUC.1tree <- roc.area(obs=test$type, pred=btre.test.p)$A; 
AUC.1tree ##  0.9479127 

#===============================
# BAGGING
#==============================

install.packages("ipred")
library(ipred)


fit.bagging <- bagging(factor(type) ~ make + address + all + num3d + our + over + remove + internet
                       + order + mail + receive + will + people + report +addresses
                       + free + business + email + you +credit +your + font + num000
                       +money +hp + hpl + george + num650 + lab + labs +telnet + num857
                       + data + num415 +num85 +technology + num1999 + parts + pm + direct +
                         cs + meeting + original +project +re + edu + table + conference + charSemicolon 
                       +charRoundbracket +charSquarebracket + charExclamation + charDollar +charHash + 
                         capitalLong + capitalTotal,
                       data=train, nbagg=50, coob=TRUE)

print(fit.bagging)
## Out-of-bag estimate of misclassification error:  0.0593 

summary(fit.bagging) # SHOWING ALL THE DETAILS

# MAKE PREDICTIONS
yhat <- predict(fit.bagging, newdata=test, type="prob")[, 2]

# ROC CURVE AND C-INDEX
# install.packages("verification")
library(verification)
test$type <- as.numeric(as.character(test$type))

#y <- ifelse(test$type=="spam", 1, 0)
AUC <- roc.area(obs=test$type, pred=yhat)$A; 
AUC    ## [1] 0.9713799
mod.glm <- verify(obs=y, pred=yhat)
roc.plot(mod.glm, plot.thres = NULL, col="red")
text(x=0.7, y=0.2, paste("Area under ROC =", round(AUC, digits=4), 
                         sep=" "), col="blue", cex=1.2)
AUC.bagging <- AUC

#======================
#  RANDOM FOREST
#======================
install.packages("randomForest")
library(randomForest)
fit.rf <- randomForest(factor(type) ~ make + address + all + num3d + our + over + remove + internet
                       + order + mail + receive + will + people + report +addresses
                       + free + business + email + you +credit +your + font + num000
                       +money +hp + hpl + george + num650 + lab + labs +telnet + num857
                       + data + num415 +num85 +technology + num1999 + parts + pm + direct +
                         cs + meeting + original +project +re + edu + table + conference + charSemicolon 
                       +charRoundbracket +charSquarebracket + charExclamation + charDollar +charHash + 
                         capitalLong + capitalTotal, 
                       data=train, importance=TRUE, proximity=TRUE, ntree=500)
fit.rf; 
#Confusion matrix:
#          nonspam spam class.error
#nonspam    1820   52  0.02777778
#spam         93 1102  0.07782427

plot(fit.rf, main="Out-of-Bag Estimate of Error")

# PREDICTION
predrf <- predict(fit.rf, newdata=test, type="response")

predrf <- as.numeric(as.character(predrf))

test$type <- as.numeric(as.character(test$type))

#y <- ifelse(test$type=="spam", 1, 0)
AUC <- roc.area(obs=test$type, pred=predrf)$A; 
AUC    ## [1]  0.9424012
mod.glm <- verify(obs=y, pred=predrf)
mod.glm <- as.numeric(as.character(mod.glm))

roc.plot(mod.glm, plot.thres = NULL, col="red")
text(x=0.7, y=0.2, paste("Area under ROC =", round(AUC, digits=4), 
                         sep=" "), col="blue", cex=1.2)
AUC.rf <- AUC

rnfr<-table(predicted=predrf, actual=test$type)

plot(test$type, predrf, col="blue", cex=0.8)
abline(a=0, b=1, col="red", lwd=2)
#MSE <- mean((spam$type- predrf)^2); MSE

# VARIABLE IMPORTANCE RANKING
round(importance(fit.rf), 2)
varImpPlot(fit.rf, main="Variable Importance Ranking")

# PARTIAL DEPENDENCE PLOT
par(mfrow=c(1,2))
partialPlot(fit.rf, pred.data=test, x.var=make, rug=TRUE)
partialPlot(fit.rf, pred.data=test, x.var=num3d, rug=TRUE)


# OUTLIER MEASURES (FOR CLASSIFICATION ONLY)
plot(outlier(fit.rf), type="h", ylab="Outlier Measures",
     col=c("red", "green")[as.numeric(train$type)],
     main="Outlier Detection via RF (spam Data)")


# =============================================================================
#  MODEL I: ADABOOST WITH STUMPS (EXPONENTIAL LOSS & DISCRETE LINK FUNCTION)
# =============================================================================
# YOU MAY TRY OUT DIFFERENT COMBINCATIONS OF LOSS AND LINK FUNCTIONS

# SET OPTIONS FOR DIFFERENT WEAK LEARNERS
## Stumps with two leaves
install.packages("ada")
library(ada)

stump <- rpart.control(cp=-1, maxdepth=1, minsplit=0)

# DISCRETE ADABOOST
fit.stump <- ada(type~., data=train, iter=2000, loss="e", type="discrete", control=stump);
fit.stump

#Final Confusion Matrix for Data:
#  Final Prediction
#True value nonspam spam
#nonspam    1806   66
#spam         97 1098

#Train Error: 0.053 

#Out-Of-Bag Error:  0.058  iteration= 1967 

#Additional Estimates of number of iterations:
  
#  train.err1 train.kap1 
#1941       1977 

head(test)


#  EXPLORATION
names(fit.stump)
summary(fit.stump)
#Training Results
#Accuracy: 0.947 Kappa: 0.888 
summary(fit.stump, n.iter=36)   #Accuracy: 0.908 Kappa: 0.803 
plot(fit.stump, kappa =FALSE, test=FALSE)
plot(fit.stump, kappa =TRUE, test=FALSE)  # KAPPA AGREEMENT

# IF YOU WANT TO BASE THE MODEL ASSESSMENT ON THE TEST DATA  
fit1.stump <- addtest(x=fit.stump, test.x=test, test.y=test$type)
names(fit1.stump)
plot(fit1.stump, kappa =FALSE, test=TRUE)  

# VARIABLE IMPORTANCE
varplot(fit.stump, plot.it=TRUE,type="scores")
vip <- varplot(fit.stump, plot.it=FALSE, type="scores")
round(vip,4)

# PREDICTION 
yhatst <- predict(fit.stump, newdata=test, type="probs")[, 2]

# ROC CURVE AND C-INDEX
# install.packages("verification")
library(verification)
y <- ifelse(test$type=="spam", 1, 0)
AUCst <- roc.area(obs=test$type, pred=yhatst)$A; 
AUCst   # 0.9821874
mod.glm <- verify(obs=y, pred=yhatst)
mod.glm <- as.numeric(as.character(mod.glm))
roc.plot(mod.glm, plot.thres = NULL, col="red")
text(x=0.7, y=0.2, paste("(st) Area under ROC =", round(AUC, digits=4), 
                         sep=" "), col="blue", cex=1.2)
AUC.stump <- AUCst



#========================================
# Question 3(Additional Features from RF)

# Train an RF model with B = 2, 000 trees using the entire data set. 
#Make sure that you set these two options: importance = TRUE and proximity = TRUE.
#========================================
# RANDOM FOREST
# ======================

fit.rf <- randomForest(factor(type) ~ make + address + all + num3d + our + over + remove + internet
                       + order + mail + receive + will + people + report +addresses
                       + free + business + email + you +credit +your + font + num000
                       +money +hp + hpl + george + num650 + lab + labs +telnet + num857
                       + data + num415 +num85 +technology + num1999 + parts + pm + direct +
                         cs + meeting + original +project +re + edu + table + conference + charSemicolon 
                       +charRoundbracket +charSquarebracket + charExclamation + charDollar +charHash + 
                         capitalLong + capitalTotal, 
                       data=spam, importance=TRUE, proximity=TRUE, ntree=2000)
fit.rf; 
plot(fit.rf, main="Out-of-Bag Estimate of Error")
fit.rf1 <- fit.rf


?predict.randomForest

# PREDICTION
pred <- predict(fit.rf1, newdata=test, type="response")
plot(test$type, pred, col="blue", cex=0.8)
abline(a=0, b=1, col="red", lwd=2)
MSE <- mean((test$type- pred)^2);

#================================ 
# VARIABLE IMPORTANCE RANKING
# ===============================
round(importance(fit.rf1), 2)
varImpPlot(fit.rf1, main="Variable Importance Ranking")

# ================================
# PARTIAL DEPENDENCE PLOT
# ================================

par(mfrow=c(1,2))
partialPlot(fit.rf1, pred.data=train, x.var=charExclamation, rug=TRUE)
partialPlot(fit.rf1, pred.data=train, x.var=capitalLong, rug=TRUE)
partialPlot(fit.rf1, pred.data=train, x.var=charDollar, rug=TRUE)

# OUTLIER MEASURES (FOR CLASSIFICATION ONLY)
plot(outlier(fit.rf1), type="h", ylab="Outlier Measures",
     col=c("red", "green")[as.numeric(spam$type)],
     main="Outlier Detection via RF1 (spam Data)")

# 3. PROXIMITY MATRIX

D <- fit.rf$proximity
# A SELF-DEFINED OUTLIER MEASURE
outlier.measure <- apply(D, 2, mean); outlier.measure  
# PLOT THE OUTLIER MEASURE
n <- length(outlier.measure)
plot(1:n, outlier.measure, type="p", pch=1, cex=0.8, col="blue", xlab="ID", 
     ylab="Outlier Measure", main="Average Distance from Others via RF Proximity Matrix")
segments(x0=1:n, y0=0, x1=1:n, y1=outlier.measure)


# ==============================
# MDS WITH THE DISTANCE MATRIX
# ==============================
fit.mds1 <- cmdscale(1 - fit.rf1$proximity, eig=TRUE)
plot(fit.mds1$points, xlab="dim I", ylab="dim II", pch=19, cex=0.8, 
     col=c("red", "green")[as.numeric(spam$type)])
#legend(-.25, .23, legend=class, col=c("red", "green"), pch=19)
#

# =======================
# PREDICTION
# ======================
yhat <- predict(fit.rf1, newdata=test, type="prob")[, 2]
# ROC CURVE AND C-INDEX
# install.packages("verification")
library(verification)

AUC <- roc.area(obs=test$type, pred=yhat)$A; 
AUC   ## 0.9998048
mod.glm <- verify(obs=y, pred=yhat)






