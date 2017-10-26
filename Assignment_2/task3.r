library(pixmap)

N<-400;H<-112;W<-92

readImages <- function(){
  #img.data will contain the picture files in unaltered matrix format
  #img.transpose transposes the image
  img_data<-array(NA,dim=c(N,H,W))
  img_transpose<-array(NA,dim=c(N,W,H))
  for(i in 1:N){
    img<-read.pnm(file=paste("Faces/", paste(i,"pgm",sep="."), sep=""), cellres=1)
    img_data[i,,]<-img@grey
    img_transpose[i,,]<-t(img_data[i,,])
  }
  return(list(img_data, img_transpose))
}

images <- readImages()
img_data <- images[[1]]
img_transpose <- images[[2]]

reOrientateImage <- function(){
  img_up<-array(NA,dim=c(N,W,H))
  for(i in 1:N){
    for(j in 1:H){
      img_up[i,,(H-j+1)]<-img_transpose[i,,j]
    }
  }
  return(img_up)
}

img_up <- reOrientateImage()


getMeanAndSDImage <- function(){
  #we wish to find the correct scaling
  img_mean<-matrix(NA,nrow=W,ncol=H)
  img_sd<-matrix(NA,nrow=W,ncol=H)
  for(j in 1:W){
    for(k in 1:H){
      img_mean[j,k]<-mean(img_up[,j,k])
      img_sd[j,k]<-sd(img_up[,j,k])
    }
  }
  return(list(img_mean, img_sd))
}

mean_sd <- getMeanAndSDImage()
img_mean <- scaledImage[[1]]
img_sd <- scaledImage[[2]]

scaleImage <- function(){
  img_scale<-array(NA,dim=c(N,W,H))
  for(i in 1:N){
    img_scale[i,,]<-(img_up[i,,]-img_mean)/img_sd
  }
  return(img_scale)
}

img_scale <- scaleImage()

conversionToInputMatrix <- function(){
  #Conversion to input Matrix
  X<-matrix(NA,nrow=N,ncol=(W*H))
  for(i in 1:N){
    vec_pic<-img_scale[i,1,]
    for(j in 2:W){
      vec_pic<-cbind(vec_pic,img_scale[i,j,])
    }
    X[i,]<-vec_pic
  }
  return(X)
}

X <- conversionToInputMatrix()

getCovarianceMatrix <- function(){
  A<-X%*%t(X) #covariance matrix, C<-t(X)%*%X
  store<-prcomp(A)
  img_eig<-t(X)%*%store$rotation
  return(img_eig)
}

img_eig <- getCovarianceMatrix()

eigenfaceImage <- function(EigenfaceVec,W,H){
  Eigenface<-matrix(NA,ncol=H,nrow=W)
  for(i in 1:W){
    Eigenface[i,]<-EigenfaceVec[((H*(i-1))+1):(H*i)]
  }
  return(Eigenface)
}

i=1
eig<-eigenfaceImage(img_eig[,i],W,H)
image(eig,col = grey(seq(0, 1, length = 256)))

#par(mfrow=c(1,7))
#ar(mar=c(0.1,0.2,0.2,0.1))
#eig<-eigenfaceImage(img_eig[,1],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,2],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,3],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,4],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,5],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,6],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#eig<-eigenfaceImage(img_eig[,7],W,H)
#image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
#par(mar=c(2,2,2,2))

svg(filename='mean_face.svg')
image(img_mean,col = grey(seq(0, 1, length = 256)))
dev.off()

