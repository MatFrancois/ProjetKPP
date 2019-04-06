nonbruit<-img20==img100
bruit<-img20!=img100
img20[1,2,]
img20new<-img20

don<-NULL
#création nv jeux de données
for (i in 1:256){
  for(j in 1:256){
    don$x<-c(don$x,i)
    don$y<-c(don$y,j)
    don$r<-c(don$r,img20[i,j,1])
    don$g<-c(don$g,img20[i,j,2])
    don$b<-c(don$b,img20[i,j,3])
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
classe2<-cl(img20)

nonbruit<-classe==classe2
bruit<-classe!=classe2

#création des train test et classes
train.gc<-cbind(don$x[nonbruit],don$y[nonbruit])
test.gc<-cbind(don$x[bruit],don$y[bruit])
cl<-classe2[nonbruit]

#algo knn
require(class)
res.knn<-knn(train = train.gc,test = test.gc,cl = cl,k = 3)
summary(res.knn)

don$classe<-classe2
don<-data.frame(don)

#mise en forme des classes
qq<-strsplit(sapply(res.knn,as.character),"_")
ee<-data.frame(matrix(unlist(qq),nrow = length(qq),ncol = 3,byrow = TRUE))
ee$X1<-sapply(sapply(ee$X1,paste),as.numeric)
ee$X2<-sapply(sapply(ee$X3,paste),as.numeric)
ee$X3<-sapply(sapply(ee$X3,paste),as.numeric)

#remplacement donnée rgb
don$r[don$classe!=classe]<-ee[,1]
don$g[don$classe!=classe]<-ee[,2]
don$b[don$classe!=classe]<-ee[,3]

#reconstitution image pour display
img20new[,,1]<-matrix(don$r,nrow = 256,byrow = TRUE)
img20new[,,2]<-matrix(don$g,nrow = 256,byrow = TRUE)
img20new[,,3]<-matrix(don$b,nrow = 256,byrow = TRUE)
display(img20)
display(img20new)
display(img100)

#####img20

