setwd("/Users/madhurinannaware/Desktop/interactions")
read <- paste("/Users/madhurinannaware/Desktop/interactions/interactions/.txt", sep="/")

d1<- read.table(read, header = TRUE, fill=TRUE)

dim(d1)

d1 <- na.omit(d1)

dim(d1)
df <- d1
read3 <- paste("/Users/madhurinannaware/Desktop/interactions/kd copy.txt", sep="")
#avlbl_KD <- read.table(read3, header = FALSE)
avlbl_KD<-scan(read3, character(), quote = "")

d1<-subset(df, PDB %in% avlbl_KD)

write.table(d1,file="filteredKD.csv")


read4 <- paste("/Users/madhurinannaware/Desktop/interactions/filteredKD.csv", sep="/")
d1<- read.table(read4, header = TRUE, fill=TRUE)


vndrwal <- cbind(c("H","B","C","N","O","F","CL","BR","I","HE","AS","AU","CD","CU","GA","HG","IN","K","KR",
                   
                   "LI","MG","NA","NE","NI","P","PB","PD","PT","S","SE","SI","SN","TE","TL","U","XE","ZN","AL","BA","BE",
                   
                   "CA","CO","CR","CS","D","EU","FE","GD","HO","IR","MN",
                   
                   "MO","PA","PR","PU","RB","RE", "RU","SR","TA","V","W","X","Y","YB",""),
                 
                 c(1.20,2.08,1.85,1.54,1.40,1.35,1.80,1.95,
                   
                   2.15,0.99,1.85,1.66,1.58, 1.40,1.8,1.55,1.93,2.75,2.02,1.82,1.73,2.27,1.54,1.63,
                   
                   1.80,2.02,1.63,1.72,1.80,1.90,2.10,2.17,2.06,1.96,1.86,2.16,1.39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

type1level <- levels(d1$Ty)

print(type1level)

type2level <- levels(d1$Ty.1)

print(type2level)

#write.table(c(type1level,type2level), file = "optype.txt")







m1 = match(d1$Ty,vndrwal[,1])

m2 = match(d1$Ty.1,vndrwal[,1])

M <- cbind(m1,m2)



m6 <- dim(M)[1]

m10 <- c()



for(q in 1:m6)  {
  
  m7=m1[q]
  
  m8 = m2[q]
  
  
  
  m3 = vndrwal[m7,2]
  
  m4 = vndrwal[m8,2]
  
  m3 <- noquote(m3[1])
  
  m4 <- noquote(m4[1])
  
  
  
  
  
  m5 = noquote(cbind(m3,m4))
  
  m5 = as.numeric(m5[1,1])+as.numeric(m5[1,2])
  
  if(d1$Dis[q]>= m5) {
    
    
    
    m9 = paste(q)
    
    m10 <- rbind (m10,q)
    
    
    
  } else m10 <- m10
  
  
  
}

m11 <- noquote(paste(m10))



d2 <-d1

d2 <- d2[-c(m10),]

dat <- d2

write.table(d2,file="filtered_KD_and_dist.csv")

str(dat)



dim(dat)



pdb<-table(dat$PDB)



pdb_values <- (t(names(pdb)))



frequency1 <- paste(dat$Ty, dat$Ty.1, sep="_")



colname <- table(frequency1)







frequency2 <- paste(dat$PDB, dat$Ty, dat$Ty.1)



f2<- table(frequency2)




Mat <- matrix(0, nrow=dim(pdb_values)[2], ncol=dim(colname)[1])

colnames(Mat) <- names(colname)



row.names(Mat) <- names(pdb)



dim <- dim(Mat)[2]




for(i in 1:dim(pdb_values)[2]) {
  
  
  
  k <- pdb_values[1,i]
  
  
  
  name <- paste(pdb_values[1,i], sep = "")
  
  
  
  l<-assign(name, subset(dat, PDB ==  k))
  
  
  
  
  
  
  
  vector <- (table(paste(l$Ty, l$Ty.1, sep="_")))
  
  
  
  dim1 <- dim(vector)
  
  
  
  for (p in 1:dim1) {
    
    
    
    (j = (names(vector)[p]))
    
    
    
    
    
    
    
    for(z in 1:dim) {
      
      
      
      qt = colnames(Mat)[z]
      
      
      
      {
        
        if (is.null(j) | is.null(qt))
          
          
        {Mat <- Mat} else (
          
          if (j==qt )
            
            
            
          {
            
            b11<-paste("^",qt,"$", sep="")
            
            b <- grep(b11, colnames(Mat))
            
            
            
            
            #e<-which(names(vector) == j)
            
            e11<-paste("^",j,"$", sep="")
            e<- grep(e11, names(vector))
            
            
            # e1 <- e[1]
            Mat[name,qt] <- vector[e]
            
            
            
            
            
            write.table(Mat,file="vndrwlouttemp.csv")
            
          }
          
        )
        
        
        
        
        
        
        
      }
      
      
      
    }
    
    
    
    
    
    
    
  }
  
  
  
  
  
  
  
}



write.table(Mat,file="vndrwloutput.csv")