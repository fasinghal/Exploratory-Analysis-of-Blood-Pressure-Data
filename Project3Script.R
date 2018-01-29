bp<- read.csv(file = "D:/RWD Final/bp.txt", header = T, sep = "\t"); # reading the input file
boxplot(x = bp$armsys,range = 1.5, main="Arm Systolic BP") #plotting first data
boxplot(x = bp$fingsys,range = 1.5, main="Finger Systolic BP",add = T) #plotting second data set
summary(bp$fingsys) # 5 point summary for data set1
summary(bp$armsys) #5point summary for second data set
#calculating IQR
 IQR(bp$armsys)
# [1] 28.5
 IQR(bp$fingsys)
# [1] 28.5
#plotting Histogram and adding density curves for two data sets
hist(x = bp$armsys,main = "Histogram of Arm Systolic BP",xlab = "BP",ylab = "frequency", probability = T)
lines(x = density(bp$armsys),col="red")
hist(x = bp$fingsys,main = "Histogram of Finger Systolic BP",xlab = "BP",ylab = "frequency", probability = T)
lines(x = density(bp$fingsys),col="blue")
# QQ plot for the two data sets with normal distribution quantiles
qqnorm(y = bp$armsys,main = "Normal QQ Plot for Arm Systolic BP", ylab = "Armsys Quantiles");
qqline(y = bp$armsys, col="red")
qqnorm(y = bp$fingsys,main = "Normal QQ Plot for Finger Systolic BP", ylab = "Fingersys Quantiles");
qqline(y = bp$fingsys, col="blue")
# D = difference on two data points
D<-bp$fingsys-bp$armsys
Dbar<-mean(D)
Sd_D<-sd(D)
# constructing a CI for D
cp<-qt(p = 0.975, df = 199)
lower<-Dbar-(cp*Sd_D/sqrt(length(D)))
upper<-Dbar+(cp*Sd_D/sqrt(length(D)))
# CI for D
c(lower, upper)
#Normality assumption holds since D follows N(0,1) as n is large
qqnorm(D,main = "QQ Plot of D = X-Y", ylab = "Quantiles of X-Y")
qqline(D)

# Construct a Single Confidence Interval for phat
conf.int <- function(sample.size, p, alpha=0.05) {
  U <- runif(sample.size); # generate Uniform (0,1)
  X <- 1*(U<=p); # Generate Bernoulli(p) from U
  phat<-mean(X); # estimated p = phat
  est.std.err <- sqrt(phat*(1-phat)/sample.size); # standard error in Phat
  ci <-phat + c(-1, 1) * qnorm(1 - (alpha/2)) * est.std.err; # CI
  return(ci);
}
#Repeat the experiment and calculate accuracy
Accuracy_Ci<-function(nsim=1000,size, phat){
  acc=rep(0,5); #accuracy data = 0
  for(i in 1:length(size)){
    cimat<-replicate(nsim,conf.int(size[i],phat)); # create nsim CIs for n=size[i] and phat
    acc[i]<-mean((phat>=cimat[1,])*(phat<=cimat[2,]))*100; # calculate the accuracy
  }
  return(acc);
}
# Sizes
size<-seq(10,200, by=15);
#Accuracy data sets fpr different p and n values
data1<-Accuracy_Ci(1000,size,0.2)
data5<-Accuracy_Ci(1000,size,0.95)
data4<-Accuracy_Ci(1000,size,0.9)
data3<-Accuracy_Ci(1000,size,0.25)
data2<-Accuracy_Ci(1000,size,0.1)
#PLotting the data sets
plot(x=size,y=data1, ylim=c(0,100),type="b",col="red",ann=F)
#par(new=TRUE) adds the next plot to the same graph
par(new=TRUE)
plot(x=size,y=data2, ylim=c(0,100),type="b",col="blue",ann=F)
par(new=TRUE)
plot(x=size,y=data3, ylim=c(0,100),type="b",col="green",ann=F)
par(new=TRUE)
plot(x=size,y=data4, ylim=c(0,100),type="b",col="black",ann=F)
par(new=TRUE)
plot(x=size,y=data5, ylim=c(0,100),type="b",col="violet",ann=F)
# Horizontal Line at 95%
abline(h = 95)