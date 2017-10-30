library(pixmap)

setwd('/home/tkleiven/Documents/UCT/MachineLearning/Assignment_2/Faces')

N<-400;H<-112;W<-92
img.data<-array(NA,dim=c(N,H,W))
img.transpose<-array(NA,dim=c(N,W,H))
#img.data will contain the picture files in unaltered matrix format
#img.transpose transposes the image
for(i in 1:N){
  img<-read.pnm(file=paste(i,"pgm",sep="."))
  img.data[i,,]<-img@grey
  img.transpose[i,,]<-t(img.data[i,,])
}

#img.up puts the picture in the correct orientation to be displayed by the image function
img.up<-array(NA,dim=c(N,W,H))
for(i in 1:N){
  for(j in 1:H){
    img.up[i,,(H-j+1)]<-img.transpose[i,,j]
  }
}

#we wish to find the correct scaling
img.mean<-matrix(NA,nrow=W,ncol=H)
img.sd<-matrix(NA,nrow=W,ncol=H)
for(j in 1:W){
  for(k in 1:H){
    img.mean[j,k]<-mean(img.up[,j,k])
    img.sd[j,k]<-sd(img.up[,j,k])
  }
}


# we wish to scale the images
img.scale<-array(NA,dim=c(N,W,H))
for(i in 1:N){
  img.scale[i,,]<-(img.up[i,,]-img.mean)/img.sd
}
#plot the mean face, the sd face as well as a random face before and after scaling
image(img.mean,col = grey(seq(0, 1, length = 256)))
image(img.sd,col = grey(seq(0, 1, length = 256)))
n=sample(1:N,1)
image(img.up[n,,],col = grey(seq(0, 1, length = 256)))
image(img.scale[n,,],col = grey(seq(0, 1, length = 256)))

img.scale[n,,]

#Conversion to input Matrix
X<-matrix(NA,nrow=N,ncol=(W*H))
for(i in 1:N){
  vec.pic<-img.scale[i,1,]
  for(j in 2:W){
    vec.pic<-cbind(vec.pic,img.scale[i,j,])
  }
  X[i,]<-vec.pic
}

A<-X%*%t(X) #covariance matrix, C<-t(X)%*%X
store<-prcomp(A)
img.eig<-t(X)%*%store$rotation

#Convert each eigenface to image
EigenfaceImage=function(EigenfaceVec,W,H){
  Eigenface<-matrix(NA,ncol=H,nrow=W)
  for(i in 1:W){
    Eigenface[i,]<-EigenfaceVec[((H*(i-1))+1):(H*i)]
  }
  return(Eigenface)
}
i=1
eig<-EigenfaceImage(img.eig[,i],W,H)
image(eig,col = grey(seq(0, 1, length = 256)))

n=sample(1:N,1) #n=164
image(img.scale[n,,],col = grey(seq(0, 1, length = 256)))

tes.img<-EigenfaceImage(X[164,],W,H)
image(tes.img,col = grey(seq(0, 1, length = 256)))

#Given an input picture, it converts it to a lower dimensional input
CompressImage=function(img,eigenvector,R){
  b<-rep(NA,R)
  H<-dim(img)[2]
  W<-dim(img)[1]
  vec.pic<-img[1,]
  for(i in 2:W){
    vec.pic<-c(vec.pic,img[i,])
  }
  for(i in 1:R){
    b[i]<-crossprod(vec.pic,eigenvector[,i])
  }
  #print(b)
  #print(eigenvector[1:10,1])
  #print(b[1]*eigenvector[1:10,1])
  compress.img<-rep(0,(H*W))
  for(i in 1:R){
    compress.img<-compress.img+(b[i]*eigenvector[,i])
  }
  recreate<-EigenfaceImage(compress.img,W,H)
  return(recreate)
}

par(mfrow=c(1,2))
n<-sample(1:N,1)
image(img.scale[n,,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
K<-400
recreate=CompressImage(img.scale[n,,],img.eig,K)
image(recreate,col = grey(seq(0, 1, length = 256)),axes=FALSE)
par(mfrow=c(1,1))

par(mfrow=c(1,10))
par(mar=c(0.1,0.2,0.2,0.1))
inds<-sample(1:N,10)
image(img.eig[inds[1],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[2],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[3],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[4],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[5],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[6],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[7],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[8],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[9],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
image(img.scale[inds[10],,],col = grey(seq(0, 1, length = 256)),axes=FALSE)
par(mar=c(2,2,2,2))

par(mfrow=c(1,7))
par(mar=c(0.1,0.2,0.2,0.1))
eig<-EigenfaceImage(img.eig[,1],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,2],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,3],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,4],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,5],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,6],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
eig<-EigenfaceImage(img.eig[,7],W,H)
image(eig,col = grey(seq(0, 1, length = 256)),axes=FALSE)
par(mar=c(2,2,2,2))
