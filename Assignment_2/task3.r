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
img_mean <- mean_sd[[1]]
img_sd <- mean_sd[[2]]

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
  A<-X%*%t(X) #covariance matrix, C<-t(X)%*%X)
  store<-prcomp(A)
  img_eig<-t(X)%*%store$rotation
  return(list(img_eig, store))
}

temp <- getCovarianceMatrix()
img_eig <- temp[[1]]
dim(X)
pca <- prcomp(X, center = TRUE, scale. = TRUE)



eigenfaceImage <- function(EigenfaceVec,W,H){
  Eigenface<-matrix(NA,ncol=H,nrow=W)
  for(i in 1:W){
    Eigenface[i,]<-EigenfaceVec[((H*(i-1))+1):(H*i)]
  }
  return(Eigenface)
}

rotate <- function(x) t(apply(x, 2, rev))

reconstructImage <- function(number_of_eigenfaces){
  # reconstruct matrix
  restr <- pca$x[,1:number_of_eigenfaces] %*% t(pca$rotation[,1:number_of_eigenfaces])
  
  # unscale and uncenter the data
  if(all(pca$scale != FALSE)){
    restr <- scale(restr, center = FALSE , scale=1/pca$scale)
  }
  if(all(pca$center != FALSE)){
    restr <- scale(restr, center = -1 * pca$center, scale=FALSE)
  }
  
  par(mfcol=c(1,2), mar=c(1,1,2,1))
  # plot the original image and reconstructed image
  image(img_scale[115,,],col = grey(seq(0, 1, length = 256)), xaxt='n', ann=FALSE, yaxt='n')
  
  rst <- rotate(rotate(rotate(matrix(data=(restr[115,]), nrow=112, ncol=92))))
  image(rst,col = grey(seq(0, 1, length = 256)), xaxt='n', ann=FALSE, yaxt='n')
}

reconstructImage(5)


