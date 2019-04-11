img50new<-img50

# don<-NULL
# #création nv jeux de données
# for (i in 1:256){
#   for(j in 1:256){
#     don$x<-c(don$x,i)
#     don$y<-c(don$y,j)
#     don$r<-c(don$r,img50[i,j,1])
#     don$g<-c(don$g,img50[i,j,2])
#     don$b<-c(don$b,img50[i,j,3])
#   }
# }
load("don50.Rdata")

####partage de l'image en manquant / no nmanquant
nonbruit<-don50$r==c(t(img100[,,1]))
bruit<-don50$r!=c(t(img100[,,1]))

#création des train test et classes
train.gc<-cbind(don50$x[nonbruit],don50$y[nonbruit])
test.gc<-cbind(don50$x[bruit],don50$y[bruit])

#3 algo knn : 1 par couleur
#r :
r.cl<-don50$r[nonbruit]
#g :
g.cl<-don50$g[nonbruit]
#b : 
b.cl<-don50$b[nonbruit]

#algo knn
require(class)
res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = 3)
# summary(res.knn)
res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = 3)
res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = 3)

#remplacement donnée rgb
don50$r[bruit]<-as.numeric(paste(res.r))
don50$g[bruit]<-as.numeric(paste(res.g))
don50$b[bruit]<-as.numeric(paste(res.b))

#reconstitution image pour display
img50new[,,1]<-matrix(don50$r,nrow = 256,byrow = TRUE)
img50new[,,2]<-matrix(don50$g,nrow = 256,byrow = TRUE)
img50new[,,3]<-matrix(don50$b,nrow = 256,byrow = TRUE)
display(img50)
display(img50new)
display(img100)

#####img50
######
######variation de K pour chaque r / g / b
mse.r<-NULL
mse.g<-NULL
mse.b<-NULL
for (i in c(1:5,10,20,30)){
  res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = i)
  mse.r<-c(mse.r,mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit])**2))
  res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = i)
  mse.g<-c(mse.g,mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit])**2))
  res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = i)
  mse.b<-c(mse.b,mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit])**2))
  print(i)
}

