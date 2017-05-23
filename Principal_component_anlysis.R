dat <- NULL
red <- paste("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep="")
dat.i <- read.csv(red, sep=";")
dat.i[,1]
dim(dat.i)

dat <- dat.i[complete.cases(dat.i),]  #to see rows with missing values
str(dat)
head(dat)

dim(dat)  #1599 12
install.packages("corrplot")
library(corrplot)
mat <- cor(dat)
corrplot(mat, method="number")
mat

names(dat) <- c("FAcdty", "VAcdty", "Citric", "RSugar",
                "chlrd", "FH2SO4", "TH2SO4", "Dnsty", "pH","slphts", "alchl", "qlty") #Renaming

install.packages("car")
library(car)
install.packages("plyr")
library(plyr)
freq <- count(dat, "qlty")

dat$qlty <- recode(dat$qlty, " 1:4='bad'; 5:6='mid'; 7:10='good' ")
freqen <- count(dat, "qlty")
freqen
dat1 <- dat [,-c(12)]
str(dat1)
prin_comp <- prcomp(dat1, scale. = T, retx=TRUE)

biplot(prin_comp, scale = 0, col=c("green", "red"))
abline(v=0, col="black")
abline(h=0, col="black")
points(prin_comp$rotation[,1], prin_comp$rotation[,2], pch=18, cex=1, col="black")


names(prin_comp)
summary(prin_comp)
prin_comp$scale


# use a screeplot:
plot(prin_comp) 
screeplot(prin_comp); 
screeplot(prin_comp, type="lines")

# THE FIRST TWO PC DIRECTIONS
PC.directions <- prin_comp$rotation; PC.directions
a1.a2 <- prin_comp$rotation[,1:2]; a1.a2

# first two principal components:
prin_comp$x[,1:2]

biplot(prin_comp$x[,1:2], pch="", main="PC.1 and PC.2 for wine' data ")
text(prin_comp$x[,1:2], labels=c(1:n), col=c(rep("blue"),rep("red")))
abline(v=0, lty=2, col="blue")
abline(h=0, lty=2)

# PLOT BOTH PCs FOR OBSERVATIONS AND LOADINGS FOR VARIABLES
windows()
biplot(prin_comp, scale=0, pc.biplot=F, col=c("green", "red"))
abline(v=0, col="black")
abline(h=0, col="black")
points(prin_comp$rotation[,1], prin_comp$rotation[,2], pch=18, cex=1, col="black")



apply(prin_comp$x, 2, var)



b <- dat$qlty
Wqlty <- as.numeric(as.factor(b))
plot(prin_comp$x[,1], prin_comp$x[,2], col=Wqlty,xlab="PC1", ylab="PC2")




