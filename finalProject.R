# SET THE WORKING DIRECTORY 
setwd("C:\\Users\\Madhuri Nannaware\\Desktop\\stat 5474\\Final_Project")
dat <- NULL

# # read the data ##
read <- paste("C:\\Users\\Madhuri Nannaware\\Desktop\\stat 5474\\Final_Project\\german_credit.csv", sep="")


dat.i <- read.csv(read, sep=",")
C<-dat.i
C$Account_Balance <- factor(C$Account_Balance)
C$Creditability <- factor( C$Creditability)
C$Payment_Status_of_Previous_Credit <- factor( C$Payment_Status_of_Previous_Credit)
C$Purpose <- factor( C$Purpose)
C$Value_Savings_Stocks <- factor( C$Value_Savings_Stocks)
C$Length_of_current_employment <- factor( C$Length_of_current_employment)
C$Instalment_per_cent  <- factor( C$Instalment_per_cent )
C$Sex_Marital_Status  <- factor( C$Sex_Marital_Status )
C$Guarantors  <- factor( C$Guarantors )
C$CurrentAddressDuration  <- factor( C$CurrentAddressDuration )
C$Most_valuable_available_asset  <- factor( C$Most_valuable_available_asset )
C$SConcurrent_Credits  <- factor( C$Concurrent_Credits )
C$TypeofApartment  <- factor( C$TypeofApartment )
C$No_of_Credits_at_this_Bank  <- factor( C$No_of_Credits_at_this_Bank )
C$Occupation   <- factor( C$Occupation  )
C$No_of_dependents  <- factor( C$No_of_dependents )
C$Telephone  <- factor( C$Telephone )
C$Foreign_Worker  <- factor( C$Foreign_Worker )
# find missing values


na_count <-sapply(dat.i, function(y) sum(length(which(is.na(y)))))
na_count  ##no NA values

margin.table(prop.table(table( C$Account_Balance, C$Payment_Status_of_Previous_Credit, C$Value_Savings_Stocks,
                               C$Length_of_current_employment,C$Instalment_per_cent,C$Sex_Marital_Status,
                               C$TypeofApartment, C$Guarantors,C$Purpose)),9)

#C$Account_Balance[C$Account_Balance == 4] <- 3

#C$Payment_Status_of_Previous_Credit[C$Payment_Status_of_Previous_Credit == 1] <- 0
#C$Payment_Status_of_Previous_Credit[C$Payment_Status_of_Previous_Credit == 2] <- 1
#C$Payment_Status_of_Previous_Credit[C$Payment_Status_of_Previous_Credit == 3] <- 2
#C$Payment_Status_of_Previous_Credit[C$Payment_Status_of_Previous_Credit == 4] <- 2

#C$Value_Savings_Stocks[C$Value_Savings_Stocks == 4] <- 3
#C$Value_Savings_Stocks[C$Value_Savings_Stocks == 5] <- 4

#C$Length_of_current_employment[C$Length_of_current_employment == 1] <- 2

#C$Occupation[C$Occupation == 1] <- 2

#C$Sex_Marital_Status[C$Sex_Marital_Status == 1] <- 2

#C$No_of_Credits_at_this_Bank[C$No_of_Credits_at_this_Bank == 3] <- 2
#C$No_of_Credits_at_this_Bank[C$No_of_Credits_at_this_Bank == 4] <- 2
#C$Guarantors[C$Guarantors == 3] <- 2

#C$Concurrent_Credits[C$Concurrent_Credits == 1] <- 2

#C$Purpose[C$Purpose == 10] <- 0    #other is 0 
#C$Purpose[C$Purpose == 9] <- 0
#C$Purpose[C$Purpose == 8] <- 0
#C$Purpose[C$Purpose == 5] <- 3
#C$Purpose[C$Purpose == 6] <- 3
#C$Purpose[C$Purpose == 4] <- 3



margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),1)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),2)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),3)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),4)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),5)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),6)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker)),7)
margin.table(prop.table(table( C$Most_valuable_available_asset, C$Concurrent_Credits,
                               C$No_of_Credits_at_this_Bank,C$Occupation,C$No_of_dependents,
                               C$Telephone, C$Foreign_Worker, C$CurrentAddressDuration)),8)

CrossTable(C$Creditability, C$Account_Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Payment_Status_of_Previous_Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(C$Creditability, C$Value_Savings_Stocks, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Length_of_current_employment, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Instalment_per_cent, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(C$Creditability, C$Sex_Marital_Status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Guarantors, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$CurrentAddressDuration , digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(C$Creditability, C$TypeofApartment, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Most_valuable_available_asset, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(C$Creditability, C$No_of_Credits_at_this_Bank, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Occupation, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Concurrent_Credits, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

CrossTable(C$Creditability, C$No_of_dependents, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(C$Creditability, C$Telephone, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

t.test(C$Creditability,dat$Duration_of_Credit_month)
t.test(C$Creditability,dat$Credit_Amount)
t.test(C$Creditability,dat$Age
## descriptive stat for continuous variable

attach(C) 
summary(Duration_of_Credit_month) # Summary statistics are printed for this variable
brksCredit <- seq(0, 80, 10) # Bins for a nice looking histogram
hist(Duration_of_Credit_month, breaks=brksCredit, xlab = "Credit Month", ylab = "Frequency", main = " ", cex=0.4) # produces nice looking histogram
boxplot(Duration_of_Credit_month, bty="n",xlab = "Credit Month", cex=0.4) # For boxplot

summary(Credit_Amount) 
brksCredit <- seq(0, 80, 10) 
hist(Credit_Amount, xlab = "Credit Amount", ylab = "Frequency", main = " ") 
boxplot(Credit_Amount, bty="n",xlab = "Cr Amount", cex=0.4) 

summary(Age) 
brksCredit <- seq(0, 80, 10) 
hist(Age, xlab = "Age", ylab = "Frequency", main = " ") 
boxplot(Age, bty="n",xlab = "Age", cex=0.4) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#19.00   27.00   33.00   35.54   42.00   75.00 
> brksCredit <- seq(0, 80, 10) 
> hist(Age, xlab = "Age", ylab = "Frequency", main = " ") 
> boxplot(Age, bty="n",xlab = "Age", cex=0.4)

## Data Partitioning 
set.seed(1)
partition = sample(1:nrow(C), size=0.7*nrow(C)) # sample of 70% of row numbers created
Train <- C[partition,] # Training data 
Test <- C[-partition,] # Test data 

##Logistic Modelling

fit <- glm(Creditability ~ Account_Balance + Payment_Status_of_Previous_Credit + Purpose + Value_Savings_Stocks + Length_of_current_employment
           + Sex_Marital_Status + Most_valuable_available_asset + TypeofApartment  + Concurrent_Credits + Duration_of_Credit_month 
           + Credit_Amount + Age, family=binomial, data = Train)
summary(fit)

## remove purpose, Length_of_current_employment, TypeofApartment, No_of_Credits_at_this_Bank, Credit_Amount
fit1 <- glm(Creditability ~ Account_Balance + Payment_Status_of_Previous_Credit  + Value_Savings_Stocks + 
            + Sex_Marital_Status + Most_valuable_available_asset 
            + Concurrent_Credits + Duration_of_Credit_month  + Age, family=binomial, data = Train)
summary(fit1)
## remove Most_valuable_available_asset  & Sex_Marital_Status
fit2 <-  glm(Creditability ~ Account_Balance + Payment_Status_of_Previous_Credit  + Value_Savings_Stocks + 
           + Concurrent_Credits + Duration_of_Credit_month
             + Age, family=binomial, data = Train)

summary(fit2)
#Min       1Q   Median       3Q      Max  
#-2.6373  -0.9100   0.4813   0.7944   1.7782  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                        -1.392395   0.686246  -2.029 0.042458 *  
#  Account_Balance2                    0.415731   0.228998   1.815 0.069457 .  
#Account_Balance3                    1.283163   0.418924   3.063 0.002191 ** 
 # Account_Balance4                    1.640896   0.248901   6.593 4.32e-11 ***
#  Payment_Status_of_Previous_Credit1 -0.065300   0.609106  -0.107 0.914625    
#Payment_Status_of_Previous_Credit2  0.578865   0.453385   1.277 0.201686    
#Payment_Status_of_Previous_Credit3  0.696782   0.522996   1.332 0.182765    
#Payment_Status_of_Previous_Credit4  1.179169   0.476480   2.475 0.013333 *  
#  Value_Savings_Stocks2               0.028625   0.307931   0.093 0.925937    
#Value_Savings_Stocks3               0.349112   0.435224   0.802 0.422470    
#Value_Savings_Stocks4               0.454950   0.552349   0.824 0.410130    
#Value_Savings_Stocks5               1.047379   0.288459   3.631 0.000282 ***
 # Concurrent_Credits                  0.269313   0.130867   2.058 0.039598 *  
#  Duration_of_Credit_month           -0.034882   0.007371  -4.732 2.22e-06 ***
#  Age                                 0.017634   0.008375   2.106 0.035240 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 873.04  on 699  degrees of freedom
#Residual deviance: 717.32  on 685  degrees of freedom
#AIC: 747.32


fit.final <-fit2

fit <- fitted.values(fit.final)


#Threshold <- rep(0,700)
#for (i in 1:700)
#  if(fit[i] >= 0.7) Threshold[i] <- 1  ## with 0.7 confidnce interval 395/479

#CrossTable(Train$Creditability, Threshold, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train)
#install.packages("ROCR")
#library(ROCR)


pred <-  predict(fit.final, Test, interval = "confidence")


pred[pred>= 0.7]  <- 1
pred[pred < 0.7] <- 0
## confusion matrix for regression 
table(Test$Creditability, pred)

#pred
#0   1
#0  54  25
#1  69 152


#  if(pred[i] >= 0.7) count[i] <- 1     ## with 0.7 confidnce interval 187/221
                                      ## with 0.8 confidnce interval 174/221
                                      ## with 0.9 confidnce interval 165/221
                                     ## with 0.99 confidnce interval 161/221
Test$cred <- pred
pred1 <- prediction(Test$cred , Test$Creditability)
PRF <- performance(pred, "tpr", "fpr")
plot(PRF)



### TREE BASED METHOD
install.packages("tree")
library(tree)
Train_tree <- tree(Creditability ~ Account_Balance+Duration_of_Credit_month +Payment_Status_of_Previous_Credit
                   +Purpose+Credit_Amount+Value_Savings_Stocks + Length_of_current_employment + Instalment_per_cent +Sex_Marital_Status 
                   +Guarantors+CurrentAddressDuration+Most_valuable_available_asset+Age+Concurrent_Credits+TypeofApartment+No_of_Credits_at_this_Bank+Occupation
                   + No_of_dependents+Telephone, data=Train, method="class")
summary(Train_tree)

#Number of terminal nodes:  14 
#Residual mean deviance:  0.9261 = 635.3 / 686 
#Misclassification error rate: 0.2286 = 160 / 700

plot(Train_tree)
text(Train_tree, pretty=0,cex=0.6)
Test_pred <- predict(Train_tree, Test, interval = "confidence")

Test_pred[Test_pred>= 0.7]  <- 1
Test_pred[Test_pred < 0.7] <- 0
## confusion matrix
table(Test$Creditability, Test_pred)


Train_prune8 <- prune.tree(Train_tree, best=8)
summary(Train_prune8)
#Number of terminal nodes:  8 
#Residual mean deviance:  1.005 = 695.6 / 692 
#Misclassification error rate: 0.2714 = 190 / 700 

plot(Train_prune8)
text(Train_prune8, pretty=0,cex=0.6)
Test_prune8_pred <- predict(Train_prune8, Test, interval = "prediction")
table(Test_prune8_pred, Test$Creditability)

Test_prune8_pred[Test_prune8_pred>= 0.7]  <- 1
Test_prune8_pred[Test_prune8_pred < 0.7] <- 0
## confusion matrix
table(Test$Creditability, Test_prune8_pred)


## supervised Random forest 
install.packages("randomForest")
library(randomForest)
rf <- randomForest( Creditability ~ Account_Balance+Duration_of_Credit_month +Payment_Status_of_Previous_Credit
                     +Purpose+Credit_Amount+Value_Savings_Stocks + Length_of_current_employment + Instalment_per_cent +Sex_Marital_Status 
                     +Guarantors+CurrentAddressDuration+Most_valuable_available_asset+Age+Concurrent_Credits+TypeofApartment+No_of_Credits_at_this_Bank+Occupation
                     + No_of_dependents+Telephone+Foreign_Worker, data = Train, ntree=200, importance=T, proximity=T)
plot(rf, main="")
rf
Test_rf_pred <- predict(rf, Test, type="class")
table(Test_rf_pred, Test$Creditability)
importance(rf)
varImpPlot(rf,  main="", cex=0.8)

Test_rf_pred[Test_rf_pred>= 0.7]  <- 1
Test_rf_pred[Test_rf_pred < 0.7] <- 0
## confusion matrix
table(Test$Creditability, Test_rf_pred)
#Test_rf_pred
#    0   1
#0  32  47
#1  16 205
printcp()



