#### Packages nécessaires ####
# install.packages("png")
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

