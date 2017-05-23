# SET THE WORKING DIRECTORY 
setwd("C:\\Users\\Madhuri Nannaware\\Desktop\\stat 5474")
dat <- NULL

# # read the data ##
read <- paste("C:\\Users\\Madhuri Nannaware\\Desktop\\stat 5474\\HMEQ.csv", sep="")
dat.i <- read.csv(read, sep=",")


# count number of missing values and its percentage #
propmiss <- function(dataframe) lapply(dataframe,function(x) 
data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=(sum(is.na(x))/length(x)))*100)
propmiss(dat.i)

dat.i$JOB <- sapply(dat.i$JOB, as.character) # since your values are `factor`
dat.i$JOB[is.na(dat.i$JOB)] <- "Unknown"

dat.i$REASON <- sapply(dat.i$REASON, as.character) # since your values are `factor`
dat.i$REASON[is.na(dat.i$REASON)] <- "Unknown"
install.packages("plyr")


# plot frequency table #
library(plyr)
jobfreq <- count(dat.i, "JOB")
Reasonfreq <-count(dat.i, "REASON")

#log transformations#
logmortdue <- log(dat.i$MORTDUE)
count(dat.i,"YOJ")
dat.i$YOJ[dat.i$YOJ < 0.01] <- 0.01   ## YOJ has 415 zeros . so value changed before log
dat.i$CLAGE[dat.i$CLAGE < 0.01] <- 0.01

dat.i$YOJ <- log(dat.i$YOJ)
dat.i$CLAGE <- log(dat.i$CLAGE)
head(dat.i$YOJ)
tail(dat.i$YOJ)
head(dat.i$CLAGE)
tail(dat.i$CLAGE)

dat <- dat.i
DAT2 <- dat.i

 ## dealing with missing values ###

miss.info <- function(dat, filename=NULL){
  vnames <- colnames(dat); vnames
  n <- nrow(dat)
  out <- NULL
  for (j in 1: ncol(dat)){
    vname <- colnames(dat)[j]
    x <- as.vector(dat[,j])
    n1 <- sum(is.na(x), na.rm=T)
    n2 <- sum(x=="NA", na.rm=T)
    n3 <- sum(x=="", na.rm=T)
    nmiss <- n1 + n2 + n3
    ncomplete <- n-nmiss
    out <- rbind(out, c(col.number=j, vname=vname, 
                        mode=mode(x), n.levels=length(unique(x)), 
                        ncomplete=ncomplete, miss.perc=nmiss/n))
  }
  out <- as.data.frame(out)
  row.names(out) <- NULL 
  if (!is.null(filename)) write.csv(out, file = filename, row.names=F)
  return(out)
}

dir.create("Results")   # Create a new folder under the current working directory
 miss.info(dat.i, filename="./Results/miss-info.csv")
 miss.info(dat, filename="./Results/miss-info.csv")
 
 # IMPUTATION BY METHOD I: MICE
 # ---------------------
  install.packages("mice")
 library(mice)
 
 md.pattern(dat)
 fit.mice <- mice(dat, m=1, maxit = 50, method = 'pmm', seed = 500)
 summary(fit.mice)
 
 dat1.imputed <- complete(fit.mice,1)
 dim(dat1.imputed)
 
 # IMPUTATION II: RANDOM FORESTS
 # -------------------------------
 # install.packages("missForest")
# library(missForest)

 #dat2.imputed <- missForest(DAT2)
 #dim(dat2.imputed)
 
 # ===================================================================
 # DEAL WITH CATEOGIRCAL VARIABLES IN COMPUTING THE DISTANCE MATRIX
 # ===================================================================
 
 dat <- as.data.frame(dat1.imputed)
 names(dat); dim(dat)
 head(dat)

 # CHECK THE VARIABLE TYPES
 apply(dat, 2, FUN=class)
 str(dat)
 
 # METHOD II: RANDOM FORESTS
 # --------------------------
 dat$REASON <- factor(dat$REASON)
  dat$JOB <- factor(dat$JOB)

 library(randomForest)

 dat$y <- rnorm(n=NROW(dat))
 fit.RF <-randomForest(y~., data = dat, mtry=3, proximity=TRUE, ntree=2000)
 names(fit.RF)
 m<- as.matrix(1-fit.RF$proximity)
 D <- as.dist(as.matrix(1-fit.RF$proximity), diag = TRUE, upper = TRUE)

 head(D)
 
 
 ## removing variable bad 
 newdata <- dat[, -(1)]
 newdata <- newdata[, -(13)]


 
 # ===========================
 # CLUSTERING (HIERARCHICAL)
 # ===========================
 
 
 fit.ward <- hclust(D, method="ward.D2") 
 plot(fit.ward) 
 
 # SCREE PLOT OF HEIGHT IN HIERARCHICAL CLUSTERING
 K.max <- 1000
 height <- tail(fit.ward$height, n=K.max)
 n.cluster <- tail((nrow(newdata)-1):1, n=K.max)
 plot(n.cluster, height,  type="b", pch=19, cex=.5, xlab="number of clusters", 
      ylab="height", col="blue", lwd=2)
 rect.hclust(fit.ward, k=4, border="red")
 
 # DENDROGRAM WITH FINAL CLUSTERS
 plot(fit.ward) 
 groups <- cutree(fit.ward, k=3) 
 rect.hclust(fit.ward, k=3, border=2:4)

 # =========================================
 #  EXPLORING THE FINAL CLUSTERS
 # =========================================
 
 
 # EDA
 clustering <- cutree(fit.ward, k=4)
 n.cluster <- length(unique(clustering))
 vnames <- names(newdata); vnames
 head(newdata)
 cols.x <- 1:12
 cols.cat.x <- c(1, 4:7, 9:10, 12)
 # par(mfrow=c(1, 1))
 for (j in cols.x){
   if (is.element(j, cols.cat.x)) {
     print(table(clustering, dat[,j]))
   } else {
     # windows()
     boxplot(dat[,j]~clustering, xlab="cluster", ylab=vnames[j], col=1:n.cluster)
   }
 }
 
 ## deciding numbr of clusters 
 

 
 
 z<- pam(D, 4, diss = TRUE)
 
 
 
 
install.packages("mclust")
  library(mclust)

 

 
 
 pam.res <- pam(newdata, 3)
 plot(pam.res)
 pam.res$cluster
plot(pam.res)
 
 k.max <- 15 # Maximal number of clusters
 data <- newdata
 wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=10 )$tot.withinss})
 plot(1:k.max, wss,
      type="b", pch = 19, frame = FALSE, 
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")
 abline(v = 3, lty =2)
 
 newdata$REASON <- recode(newdata$REASON, " 'HomeImp'= '1'; 'DebtCon'='2'; 'Unknown' ='3' ")
newdata$JOB <- recode(newdata$JOB, " 'Mgr'= '1'; 'office'='2'; 'Other' ='3';'ProfExe'= '4'; 'Sales'='5'; 'Self' ='6' ; 'Unknown' ='7' ")
 
 
library(fpc)
pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(d, pamk.best$nc))


cluster_similarity(iris_kmeans, iris_hclust)
 
 


