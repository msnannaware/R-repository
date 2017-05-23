dat <- NULL
current.month <- 8
for (i in 1:current.month){
  mth <- paste("20160", i, sep="")
  bom <- paste("http://www.bom.gov.au/climate/dwo/", mth,
               "/text/IDCJDW2801.", mth, ".csv", sep="")
  dat.i <- read.csv(bom, skip=6, check.names=FALSE,
                    na.strings = c("NA", "", " "))
  dat.i[, 1] <-toupper(month.abb[i])
  # USE month.name() TO GET FULL MONTH NAMES
  dat <- rbind(dat, dat.i)
}
head(dat,10)
dim(dat)         #258 22

Vnames<-ncol(dat)
for(j in 1:Vnames){
x<-dat[,j]
print(sort(unique(x)))
}

names(dat)

dat1<-dat[,-c(6,7,10)]
dat<-dat[,-c(10)]

names(dat) <- c("Month", "Date", "MinTemp", "MaxTemp", "Rainfall",

"Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed",

"Temp9am", "Humidity9am", "Cloud9am", "WindDir9am",

"WindSpeed9am", "Pressure9am", "Temp3pm", "Humidity3pm",

"Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")

dat$"WindSpeed9am"[ dat$"WindSpeed9am"== "Calm"]<-"0"

str(dat)
 dat$"WindSpeed9am"<-as.numeric( dat$"WindSpeed9am")
dat$"WindSpeed3pm"[ dat$"WindSpeed3pm"== "Calm"]<-"0"
dat$"WindSpeed3pm"<-as.numeric( dat$"WindSpeed3pm")
str(dat)
table(dat$"WindDir3pm", useNA="ifany")

dat$RainToday <- ifelse(dat$Rainfall > 1 , 1 , 0)

dat$RainTomorrow <- c(dat$RainToday[2:nrow(dat)], NA)

save(dat, file="dataAUG")

dat$tempdiff <- (dat$MaxTemp - dat$MinTemp)

tab1<- table(dat$tempdiff,dat$RainTomorrow, useNA="no"); tab1

chisq.test(tab1)

fisher.test(tab1, simulate.p.value =TRUE)

tab <- table(dat$Month, dat$RainTomorrow, useNA="no"); tab

chisq.test(tab)

fisher.test(tab, simulate.p.value =TRUE)


tab2<- table(dat$WindGustSpeed, dat$RainTomorrow, useNA="no"); tab2


chisq.test(tab2)

fisher.test(tab2, simulate.p.value =TRUE)

names(dat)
plot(dat$Rainfall,  dat$Pressure9am, type = "h", col="red")
par(new=TRUE)
plot(dat$Rainfall,  dat$Pressure3pm, type = "p", col="green")
)

plot(dat$tempdiff, dat$Rainfall,type = "h", col="red")

dat$PrDiff <- abs(dat$Pressure9am - dat$Pressure3pm)
dat$HumidityDiff <- abs(dat$Humidity9am - dat$Humidity3pm)
wilcox.test(dat$HumidityDiff, dat$RainTomorrow, paired=TRUE)
wilcox.test(dat$PrDiff, dat$RainTomorrow, paired=TRUE)
wilcox.test(dat$HumidityDiff, dat$Rainfall, paired=TRUE)

tab3<- table(dat$PrDiff, dat$RainTomorrow, useNA="no"); tab3


chisq.test(tab3)

fisher.test(tab3, simulate.p.value =TRUE)

tab4<- table(dat$HumidityDiff, dat$RainTomorrow, useNA="no"); tab4


chisq.test(tab4)

fisher.test(tab4, simulate.p.value =TRUE)




