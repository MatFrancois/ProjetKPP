#### Packages nécessaires ####
#install.packages("png")
require(png)

#### Importation images ####

#image complète
img100 <- readPNG("images/StarWars.png")

#images incomplètes
img10<- readPNG("images/Image10pourcent.png")
img20<- readPNG("images/Image20pourcent.png")
img50<- readPNG("images/Image50pourcent.png")

#Affichage images
display<-function(x){
  imageList<-list(x)
  totalWidth<-0
  maxHeight<-0
  for (img in imageList){
    if(is.character(img))
      img<-readPNG(img)
    dimg<-dim(img)
    totalWidth<-totalWidth+dimg[2]
    maxHeight<-max(maxHeight,dimg[1])
  }
  par(mar=c(0,0,0,0))
  plot(c(0,totalWidth),c(0,maxHeight),type="n",asp=1,xaxt="n",yaxt="n",xlab="x",ylab="y")
  offset<-0
  for (img in imageList){
    dimg<-dim(img)
    rasterImage(img,offset,0,offset+dimg[2],dimg[1])
    offset<-offset+dimg[2]
  }
}

display(img100)
display(img50)

hist(img50)#représente le taux de blanc sur l'image
# install.packages("class")
require(class)
##exemple knn :
##https://rstudio-pubs-static.s3.amazonaws.com/123438_3b9052ed40ec4cd2854b72d1aa154df9.html




####Partie codage à revoir ensemble####


##knn sur 10% de blanc
# nonblanc<-array(img10[img10[,,1]!=1&img10[,,2]!=1&img10[,,3]!=1],
                # c(length(img10[img10[,,1]!=1&img10[,,2]!=1&img10[,,3]!=1])/6,2,3))

nonbruit<-img10==img100
bruit<-img10!=img100

train.gc<-data.frame(matrix(img10[nonbruit],ncol = 3,byrow = TRUE))
tt<-NULL
for (i in 1:nrow(train.gc)){
  tt<-c(tt,paste0(train.gc[i,1],"_",train.gc[i,2],"_",train.gc[i,3]))
}

train.def<-tt#ou alors paste0 les 3 quantités de chaque pixel

test.gc<-data.frame(matrix(img10[bruit],ncol = 3,byrow = TRUE))#refaire cette ligne

test.def<-matrix(img100[bruit],ncol = 3,byrow = TRUE)
dim(test.gc)

knn.3<-knn(train = train.gc,test = test.gc,cl = train.def,k = 10)

knn.3
dim(train.gc)

imsu






