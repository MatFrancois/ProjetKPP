img20new<-img20

# don<-NULL
# #création nv jeux de données
# for (i in 1:256){
#   for(j in 1:256){
#     don$x<-c(don$x,i)
#     don$y<-c(don$y,j)
#     don$r<-c(don$r,img20[i,j,1])
#     don$g<-c(don$g,img20[i,j,2])
#     don$b<-c(don$b,img20[i,j,3])
#   }
# }

load("don20.Rdata")

####partage de l'image en manquant / no nmanquant
nonbruit<-don20$r==c(t(img100[,,1]))
bruit<-don20$r!=c(t(img100[,,1]))

#3 algo knn : 1 par couleur
#r :
r.cl<-don20$r[nonbruit]
#g :
g.cl<-don20$g[nonbruit]
#b : 
b.cl<-don20$b[nonbruit]

#création des train test et classes
train.gc<-cbind(don20$x[nonbruit],don20$y[nonbruit])
test.gc<-cbind(don20$x[bruit],don20$y[bruit])

#algo knn
require(class)
res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = 3)
# summary(res.knn)
res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = 3)
res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = 3)


#remplacement donnée rgb
don20$r[bruit]<-as.numeric(paste(res.r))
don20$g[bruit]<-as.numeric(paste(res.g))
don20$b[bruit]<-as.numeric(paste(res.b))

#reconstitution image pour display
img20new[,,1]<-matrix(don20$r,nrow = 256,byrow = TRUE)
img20new[,,2]<-matrix(don20$g,nrow = 256,byrow = TRUE)
img20new[,,3]<-matrix(don20$b,nrow = 256,byrow = TRUE)
display(img20)
display(img20new)
display(img100)

#####img20

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
