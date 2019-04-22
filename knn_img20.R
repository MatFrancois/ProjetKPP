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
nonbruit20<-don20$r==c(t(img100[,,1]))
bruit20<-don20$r!=c(t(img100[,,1]))

#3 algo knn : 1 par couleur
#r :
r.cl20<-don20$r[nonbruit20]
#g :
g.cl20<-don20$g[nonbruit20]
#b : 
b.cl20<-don20$b[nonbruit20]

#création des train test et classes
train.gc20<-cbind(don20$x[nonbruit20],don20$y[nonbruit20])
test.gc20<-cbind(don20$x[bruit20],don20$y[bruit20])

#algo knn
require(class)
res.r<-knn(train = train.gc20,test = test.gc20,cl = r.cl20,k = 1)
res.g<-knn(train = train.gc20,test = test.gc20,cl = g.cl20,k = 1)
res.b<-knn(train = train.gc20,test = test.gc20,cl = b.cl20,k = 1)


#remplacement donnée rgb
don20$r[bruit20]<-as.numeric(paste(res.r))
don20$g[bruit20]<-as.numeric(paste(res.g))
don20$b[bruit20]<-as.numeric(paste(res.b))

#reconstitution image pour display
img20new[,,1]<-matrix(don20$r,nrow = 256,byrow = TRUE)
img20new[,,2]<-matrix(don20$g,nrow = 256,byrow = TRUE)
img20new[,,3]<-matrix(don20$b,nrow = 256,byrow = TRUE)
display(img20)
display(img20new)
display(img100)

#calcul mse
mse.rk2<-mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit20])**2)
mse.gk2<-mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit20])**2)
mse.bk2<-mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit20])**2)

#####img20

######
######variation de K pour chaque r / g / b
mse.r20<-NULL
mse.g20<-NULL
mse.b20<-NULL
for (i in c(1:10)){
  res.r<-knn(train = train.gc20,test = test.gc20,cl = r.cl20,k = i)
  mse.r20<-c(mse.r20,mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit20])**2))
  res.g<-knn(train = train.gc20,test = test.gc20,cl = g.cl20,k = i)
  mse.g20<-c(mse.g20,mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit20])**2))
  res.b<-knn(train = train.gc20,test = test.gc20,cl = b.cl20,k = i)
  mse.b20<-c(mse.b20,mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit20])**2))
  print(i)
}

plot(mse.r20,col="red",type="b",ylim = c(min(c(mse.r20,mse.b20,mse.g20)),max(c(mse.r20,mse.b20,mse.g20))))
lines(mse.g20,col="green",type="b")
lines(mse.b20,col="blue",type="b")

#meilleur K : 
              # r : 1
              # b : 3
              # g : 4