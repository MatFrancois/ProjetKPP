train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
gg<-knn(train, test, cl, k = 3, prob=TRUE)


install.packages("imager")
require(imager)

nonbruit<-img10==img100
bruit<-img10!=img100
img10[1,2,]

don<-NULL
#création nv jeux de données
for (i in 1:256){
  for(j in 1:256){
    don$x<-c(don$x,i)
    don$y<-c(don$y,j)
    don$r<-c(don$r,img10[i,j,1])
    don$g<-c(don$g,img10[i,j,2])
    don$b<-c(don$b,img10[i,j,3])
  }
}


#création classes pour img que l'on souhaite
cl<-function(obj){
  cltrue<-NULL
  for (i in 1:256){
    for(j in 1:256){
      cltrue<-c(cltrue,paste0(obj[i,j,1],"_",obj[i,j,2],"_",obj[i,j,3]))
    }
  }
  return(cltrue)
}

classe<-cl(img100)
classe2<-cl(img10)

nonbruit<-classe==classe2
bruit<-classe!=classe2

train.gc<-cbind(don$x[nonbruit],don$y[nonbruit])
test.gc<-cbind(don$x[bruit],don$y[bruit])
cl<-classe2[nonbruit]

require(class)
res.knn<-knn(train = train.gc,test = test.gc,cl = cl,k = 3)
summary(res.knn)

don$classe<-classe2
don<-data.frame(don)
don[don$classe!=classe,]<-res.knn

