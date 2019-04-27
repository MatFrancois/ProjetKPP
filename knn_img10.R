img10new<-img10

# don<-NULL
# #création nv jeux de données
# for (i in 1:256){
#   for(j in 1:256){
#     don$x<-c(don$x,i)
#     don$y<-c(don$y,j)
#     don$r<-c(don$r,img10[i,j,1])
#     don$g<-c(don$g,img10[i,j,2])
#     don$b<-c(don$b,img10[i,j,3])
#   }
# }

load("don10.Rdata")


####partage de l'image en manquant / no nmanquant
nonbruit<-don10$r==c(t(img100[,,1]))
bruit<-don10$r!=c(t(img100[,,1]))

#création des train test et classes
train.gc<-cbind(don10$x[nonbruit],don10$y[nonbruit])
test.gc<-cbind(don10$x[bruit],don10$y[bruit])

#3 algo knn : 1 par couleur
#r :
r.cl<-don10$r[nonbruit]
#g :
g.cl<-don10$g[nonbruit]
#b : 
b.cl<-don10$b[nonbruit]

#algo knn *3

res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = 2)
res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = 2)
res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = 2)

# #remplacement donnée rgb
don10$r[bruit]<-as.numeric(paste(res.r))
don10$g[bruit]<-as.numeric(paste(res.g))
don10$b[bruit]<-as.numeric(paste(res.b))

#reconstitution image pour display
img10new[,,1]<-matrix(don10$r,nrow = 256,byrow = TRUE)
img10new[,,2]<-matrix(don10$g,nrow = 256,byrow = TRUE)
img10new[,,3]<-matrix(don10$b,nrow = 256,byrow = TRUE)
display(img10)
display(img100)
display(img10new)

res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = 1)
res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = 1)
res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = 1)

mse.rk<-mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit])**2)
mse.gk<-mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit])**2)
mse.bk<-mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit])**2)


######
######variation de K pour chaque r / g / b
mse.r10<-NULL
mse.g10<-NULL
mse.b10<-NULL
for (i in c(1:10)){
  res.r<-knn(train = train.gc,test = test.gc,cl = r.cl,k = i)
  mse.r10<-c(mse.r10,mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit])**2))
  res.g<-knn(train = train.gc,test = test.gc,cl = g.cl,k = i)
  mse.g10<-c(mse.g10,mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit])**2))
  res.b<-knn(train = train.gc,test = test.gc,cl = b.cl,k = i)
  mse.b10<-c(mse.b10,mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit])**2))
  print(i)
}#plus K augmente plus le MSE augmente

plot(mse.r10,col="red",type="b",ylim = c(min(c(mse.r10,mse.b10,mse.g10)),max(c(mse.r10,mse.b10,mse.g10))))
lines(mse.g10,col="green",type="b")
lines(mse.b10,col="blue",type="b")
#meilleur k : 1

##Vu que le meilleur k général semble etre 1, on fait l'imputation par k=1 pour chaque image et compare ensuite le MSE de chacun

