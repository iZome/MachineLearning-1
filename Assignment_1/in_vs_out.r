rm(list = ls(all = TRUE))
# Simulates data of size n from a regression y=a+bx+e where e~N(0,sig) and x is
# the interval [0,1]:
simul=function(n,a,b,sig)
{
	x    <- runif(n)
	e    <- rnorm(n,0,sig)
	y    <- a + b*x + e
  data <- cbind(x, y)
	return(data)
}

# Fix the parameters:
n=10; a=0; b=2; sig=0.5

# f.mod is the true model:
f.mod=function(x)
{
  return(a+b*x)
}
dat <- simul(n,a,b,sig);

# xx is simulated input and yy is the simulated output:
xx<-dat[1,]
yy<-dat[2,]
x<-seq(0,1,0.01);y<-a+(b*x) # x is the domain and y is the true output

# g.mod is the fitted model:
g <- lm(yy~xx)
g.mod=function(x)
{
  return(g$coefficients[1]+(g$coefficients[2]*x))
}
err.g <- (t(g$residuals)%*%g$residuals)/n   # In sample error for the fitted model
err.f <- (t(yy-(a+b*xx))%*%(yy-(a+b*xx)))/n # In sample error for the actual model

#--- Now, let's plot the data:
plot(c(0,1),c(-1,3),type="n",main="In-sample Errors",xlab=expression(x),ylab=expression(y))
lines(x,y,type="l",col="blue",lwd=2)
points(dat[1,],dat[2,]);
lines(x,g.mod(x))
text1=bquote(italic(E)["in"](g)==.(format(err.g,digits=3)))
text2=bquote(italic(E)["in"](f)==.(format(err.f,digits=3)))
text(x=0.15,y=2.8,labels=text1)
text(x=0.15,y=2.6,labels=text2)
#--- End of plot.

fdiff=function(x,a,b,a.est,b.est)
{
		print(((b-b.est)*x)^2)
		print(a-a.est)
		print((a-a.est+(b-b.est)*x)^2)
		quit()
   return((a-a.est+(b-b.est)*x)^2)
}
# Finds bias between f(x) and g(x):
diff=function(x)
{
  fdiff(x,a,b,g$coefficients[1],g$coefficients[2])
}
# Integrate (numerically) over bias:
biassq<-integrate(diff,0,1)$value
err.out<-biassq+(sig^2)

# Add more text to plot:
text3=bquote(italic(E)["out"](g)==.(format(err.out,digits=2)))
text4=bquote(italic(E)["out"](f)==.(format((sig^2),digits=2)))
text(x=0.5,y=2.8,labels=text3)
text(x=0.5,y=2.6,labels=text4)

# Note if you stick to hypothesis g.mod independent of data that
# Err_{in}(g) is unbiased for E_{out}(g)
M      <- 10000
g.hist <- rep(NA,M)
f.hist <- g.hist

for(i in 1:M)
{
	dat2      <- simul(n,a,b,sig)
	g.hist[i] <- (t(dat2[2,]-g.mod(dat2[1,]))%*%(dat2[2,]-g.mod(dat2[1,])))/n
	f.hist[i] <- (t(dat2[2,]-f.mod(dat2[1,]))%*%(dat2[2,]-f.mod(dat2[1,])))/n
}
c(mean(g.hist),mean(f.hist))

# E_{in}(g) underestimates the true error but E_{in}(f) is unbiased
M       <- 10000 # no of samples used to measure diff between E_{in} and E_{out}
g2.hist <- rep(NA,M);
f2.hist <- g2.hist

# Arrays store the scaled differences between E_{in} and E_{out} for f, g:
for(i in 1:M)
{
	dat3 <- simul(n,a,b,sig)
  x2   <- dat3[1,]
  y2   <- dat3[2,] # x2 is simulated input and y2 is the simulated output
  # g2 is the fitted model:
  g2     <- lm(y2~x2);
  g2.mod=function(x)
  {
    return(g2$coefficients[1]+(g2$coefficients[2]*x))
  }
	err2.f <- (t(y2-(a+(b*x2)))%*%(y2-(a+(b*x2))))/n # In sample error for the actual model
	err2.g <- (t(g2$residuals)%*%g2$residuals)/n # In sample error for the fitted model

  #Gives the difference between fitted model and true:
  diff2  <- function(x)
  {
     fdiff(x,a,b,g2$coefficients[1],g2$coefficients[2])
  }
	biassq2<-integrate(diff2,0,1)$value
	err2.out<-biassq2+(sig^2)
	f2.hist[i]<-(err2.f-(sig^2))/(sig^2)   #compare in sample vs out of sample errors for true model
	g2.hist[i]<-(err2.g-err2.out)/err2.out #compare in sample vs out of sample errors for fitted model
}
c(mean(f2.hist),mean(g2.hist)) # True vs Fitted (in vs out)

#===============================================================================
# Choose n points on the sine function
simul=function(n)
{
	x    <- runif(n,-1,1)
	y    <- sin(pi*x)
  data <- cbind(x,y)
  return(data)
}

# Shows sin fn with 2 random points as well as the two fitted models:
data <- simul(2)

#cons is the fitted constant function:
cons=function(xx,data)
{
  return(mean(data[,2])+(0*xx))
}
#lin is the fitted linear model:
lin=function(xx,data)
{
	l<-lm(data[,2]~data[,1])
	return(l$coefficients[1]+(xx*l$coefficients[2]))
}
x<-seq(-1,1,0.01)
plot(x,sin(x*pi),type="l",main=expression(f(x)))
points(data)
lines(x,cons(x,data),lty=2,col="blue")
lines(x,lin(x,data),lty=2,col="cyan")

#===============================================================================
# Let's see the trade-off for varying model complexity:
#===============================================================================
# Fit M constant models to random data:
M = 10000;
x = seq(-1,1,0.01)
plot(x,sin(x*pi), type="l", main=expression(f(x)), lwd=2, col="blue")

curves = matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M)
avg    = rep(NA,length(x))
vari   = rep(NA,length(x))
for(i in 1:M)
{
	data       <- simul(5)  # 5 data points for later comparison to poly models
	curves[i,] <- cons(x,data)
}
for(i in 1:length(x))
{
	avg[i]  <- mean(curves[,i])
	vari[i] <- var(curves[,i])
}
lines(x,avg,type="l",lwd=2,lty=2,col="red")
bias=(t(sin(pi*x)-avg)%*%(sin(pi*x)-avg))*0.01
lines(x, avg+sqrt(vari),type="l",lwd=0.5,lty=2,col="red")
lines(x, avg-sqrt(vari),type="l",lwd=0.5,lty=2,col="red")
c(bias, mean(vari), bias + mean(vari))

# fit M linear models to random data
M = 10000;
x <- seq(-1,1,0.01)
plot(c(-1,1),c(-3,3),type="n",main=expression(f(x)),xlab=expression(x),ylab=expression(sin(pi*x)))
lines(x,sin(x*pi),type="l",lwd=2,col="blue")
curves = matrix(rep(NA,M*length(x)),ncol=length(x),nrow=M)
avg    = rep(NA,length(x))
vari   = rep(NA,length(x))
for(i in 1:M)
{
	data       <- simul(5)
	curves[i,] <- lin(x,data)
}
for(i in 1:length(x))
{
	avg[i]  <- mean(curves[,i])
	vari[i] <- var(curves[,i])
}
lines(x,avg,type="l",lwd=2,lty=2,col="red")
bias=(t(sin(pi*x)-avg)%*%(sin(pi*x)-avg))*0.01
lines(x,avg+sqrt(vari),type="l",lwd=0.5,lty=2,col="red")
lines(x,avg-sqrt(vari),type="l",lwd=0.5,lty=2,col="red")
c(bias,mean(vari),bias+mean(vari))

bias=function(x){return((abs(sin(x*pi)))^2)}
integrate(bias,-1,1)
