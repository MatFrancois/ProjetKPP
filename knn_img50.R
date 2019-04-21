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
nonbruit50<-don50$r==c(t(img100[,,1]))
bruit50<-don50$r!=c(t(img100[,,1]))

#création des train test et classes
train.gc50<-cbind(don50$x[nonbruit50],don50$y[nonbruit50])
test.gc50<-cbind(don50$x[bruit50],don50$y[bruit50])

#3 algo knn : 1 par couleur
#r :
r.cl50<-don50$r[nonbruit50]
#g :
g.cl50<-don50$g[nonbruit50]
#b : 
b.cl50<-don50$b[nonbruit50]

#algo knn
require(class)
res.r<-knn(train = train.gc50,test = test.gc50,cl = r.cl50,k = 1)
res.g<-knn(train = train.gc50,test = test.gc50,cl = g.cl50,k = 1)
res.b<-knn(train = train.gc50,test = test.gc50,cl = b.cl50,k = 1)

#remplacement donnée rgb
don50$r[bruit50]<-as.numeric(paste(res.r))
don50$g[bruit50]<-as.numeric(paste(res.g))
don50$b[bruit50]<-as.numeric(paste(res.b))

#reconstitution image pour display
img50new[,,1]<-matrix(don50$r,nrow = 256,byrow = TRUE)
img50new[,,2]<-matrix(don50$g,nrow = 256,byrow = TRUE)
img50new[,,3]<-matrix(don50$b,nrow = 256,byrow = TRUE)
display(img50)
display(img50new)
display(img100)

#calcul mse
mse.rk5<-mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit50])**2)
mse.gk5<-mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit50])**2)
mse.bk5<-mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit50])**2)

#####img50
######
######variation de K pour chaque r / g / b
mse.r50<-NULL
mse.g50<-NULL
mse.b50<-NULL
for (i in c(1:10)){
  res.r<-knn(train = train.gc50,test = test.gc50,cl = r.cl50,k = i)
  mse.r50<-c(mse.r50,mean((as.numeric(paste(res.r))-c(t(img100[,,1]))[bruit50])**2))
  res.g<-knn(train = train.gc50,test = test.gc50,cl = g.cl50,k = i)
  mse.g50<-c(mse.g50,mean((as.numeric(paste(res.g))-c(t(img100[,,2]))[bruit50])**2))
  res.b<-knn(train = train.gc50,test = test.gc50,cl = b.cl50,k = i)
  mse.b50<-c(mse.b50,mean((as.numeric(paste(res.b))-c(t(img100[,,3]))[bruit50])**2))
  print(i)
}

plot(mse.r50,col="red",type="b",ylim = c(min(c(mse.r50,mse.b50,mse.g50)),max(c(mse.r50,mse.b50,mse.g50))))
lines(mse.g50,col="green",type="b")
lines(mse.b50,col="blue",type="b")
#meilleur K : 
# r : 1
# b : 1
# g : 1

##comparaison des mse / images
df.mse<-data.frame(mse=rbind(mse.rk,mse.gk,mse.bk,mse.rk2,mse.gk2,mse.bk2,mse.rk5,mse.gk5,mse.bk5))
df.mse$couleur<-rep(c("rouge","vert","bleu"),3)
df.mse$image<-sort(rep(c("image10","image20","image50"),3))

ggplot(df.mse,aes(x = image,y = mse,fill=couleur))+
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values=c("#3F93E8","#FF4F4A","#B8FF4F"))

