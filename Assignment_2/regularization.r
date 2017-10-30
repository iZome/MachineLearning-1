# Choose n points on the sine function
simul=function(n){
	data<-matrix(rep(NA,n*2),nrow=n,ncol=2)
	x<-runif(n,-1,1)
	y<-sin(pi*x)
	data[,1]<-x
	data[,2]<-y
	return(data)
}

# Shows sin fn with 2 random points as well as the two fitted models
data<-simul(2)
lin=function(xx,data){ #lin is the fitted linear model
	l<-lm(data[,2]~data[,1])
	return(l$coefficients[1]+(xx*l$coefficients[2]))
}
x<-seq(-1,1,0.01)
plot(c(-1,1),c(-3,3),type="n",main=expression(f(x)),xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",main=expression(f(x)))
points(data)
lines(x,lin(x,data),lty=2,col="cyan")

# Legendre polynomials
Legendre=function(x,n){
	val=0
	for(i in 0:n){
		val=val+((x^i)*choose(n,i)*choose((n+i-1)/2,n))
	}
	return((2^n)*val)
}

# Regularised estimate
RegEst=function(data,Q,lam){
	d=dim(data)
	y=matrix(data[,2],nrow=d[1])
	Z=matrix(rep(NA,(d[1]*(Q+1))),nrow=d[1])
	for(i in 1:d[1]){
		Z[i,1]<-1
		for(j in 2:(Q+1)){
			Z[i,j]=Legendre(data[i,1],j-1)
		}
	}
	w=solve((t(Z)%*%Z)+(lam*diag(Q+1)))%*%(t(Z)%*%y)
	return(w)
}

# polynomial model of order "Q" based on "data" and regularisation parameter "lam"
RegLin=function(xx,data,lam,Q){ #lin is the fitted linear model
	l<-RegEst(data,Q,lam)
	j<-l[1]

	for(i in 2:(Q+1)){
		j<-j+(l[i]*(Legendre(xx,i-1)))
	}

	return(j)
}

# show M linear models fitted to sin model (with and without regularization)
M=100;x<-seq(-1,1,0.01)
layout(matrix(c(1,2),nrow=1,ncol=2,byrow=T),widths=c(2,2),heights=c(1))
plot(c(-1,1),c(-3,3),type="n",main="without regularization",xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",lwd=2,col="blue")
curves.org=matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M);
curves.reg=matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M);
for(i in 1:M){
	data<-simul(2)
	curves.org[i,]<-lin(x,data)
	curves.reg[i,]<-RegLin(x,data,0.1,1)
}
for(i in 1:M){
	lines(x,curves.org[i,],type="l",lwd=0.5)
}
plot(c(-1,1),c(-3,3),type="n",main="with regularization",xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",lwd=2,col="blue")
for(i in 1:M){
	lines(x,curves.reg[i,],type="l",lwd=0.5)
}

# estimate Eout from M linear models fitted to sin model (with and without regularization)
M=1000
curves.org=matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M);avg.org=rep(NA,length(x));vari.org=rep(NA,length(x))
curves.reg=matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M);avg.reg=rep(NA,length(x));vari.reg=rep(NA,length(x))
for(i in 1:M){
	data<-simul(2)
	curves.org[i,]<-lin(x,data)
	curves.reg[i,]<-RegLin(x,data,0.1,1)
}
for(i in 1:length(x)){
	avg.org[i]<-mean(curves.org[,i])
	vari.org[i]<-var(curves.org[,i])
	avg.reg[i]<-mean(curves.reg[,i])
	vari.reg[i]<-var(curves.reg[,i])
}
layout(matrix(c(1,2),nrow=1,ncol=2,byrow=T),widths=c(1,1),heights=c(1))
plot(c(-1,1),c(-3,3),type="n",main="without regularization",xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",lwd=2,col="blue")
lines(x,avg.org,type="l",lwd=2,lty=2,col="red")
bias.org=(t(sin(pi*x)-avg.org)%*%(sin(pi*x)-avg.org))*0.01
lines(x,avg.org+sqrt(vari.org),type="l",lwd=0.5,lty=2,col="red")
lines(x,avg.org-sqrt(vari.org),type="l",lwd=0.5,lty=2,col="red")
c(bias.org,mean(vari.org),bias.org+mean(vari.org))
plot(c(-1,1),c(-3,3),type="n",main="with regularization",xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",lwd=2,col="blue")
lines(x,avg.reg,type="l",lwd=2,lty=2,col="red")
bias.reg=(t(sin(pi*x)-avg.reg)%*%(sin(pi*x)-avg.reg))*0.01
lines(x,avg.reg+sqrt(vari.reg),type="l",lwd=0.5,lty=2,col="red")
lines(x,avg.reg-sqrt(vari.reg),type="l",lwd=0.5,lty=2,col="red")
c(bias.reg,mean(vari.reg),bias.reg+mean(vari.reg))


# Quadratic function with few data points
fdiff=function(x,target,model){
	f=fit(x,model)
	return((t(f-target)%*%(f-target))*(x[2]-x[1]))
}

generator=function(n,x,func,sig){
	l<-length(func)
	dat<-matrix(rep(NA,2*n),ncol=n)
	xdat<-floor(runif(n)*l)+1
	ydat<-func[xdat]+rnorm(n,0,sig)
	xdat<-x[xdat]
	Data<-data.frame(xdat,ydat)
	return(Data)
}

# Creates 2 by 2 plot showing regularised fit to data with quadratic target function for various values of lambda
x<-seq(-1,1,0.01);n=5;sig=0.5;t=2;
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T),widths=c(1,1),heights=c(1,1))
func=(3*x^2)-(0.1*x)-2
d<-generator(n,x,func,sig)

plot(c(-1,1),c(-4,4),main=expression(lambda==0),type="n",xlab="x",ylab=expression(f(x)))
lines(x,func,type="l",lwd=2,col="blue")
points(d$xdat,d$ydat)
four<-RegLin(x,d,0,4)
lines(x,four,lty=2,col="red",lwd=0.5)

plot(c(-1,1),c(-4,4),main=expression(lambda==0.01),type="n",xlab="x",ylab=expression(f(x)))
lines(x,func,type="l",lwd=2,col="blue")
points(d$xdat,d$ydat)
four<-RegLin(x,d,0.01,4)
lines(x,four,lty=2,col="red",lwd=0.5)

plot(c(-1,1),c(-4,4),main=expression(lambda==0.1),type="n",xlab="x",ylab=expression(f(x)))
lines(x,func,type="l",lwd=2,col="blue")
points(d$xdat,d$ydat)
four<-RegLin(x,d,0.1,4)
lines(x,four,lty=2,col="red",lwd=0.5)

plot(c(-1,1),c(-4,4),main=expression(lambda==1),type="n",xlab="x",ylab=expression(f(x)))
lines(x,func,type="l",lwd=2,col="blue")
points(d$xdat,d$ydat)
four<-RegLin(x,d,1,4)
lines(x,four,lty=2,col="red",lwd=0.5)
