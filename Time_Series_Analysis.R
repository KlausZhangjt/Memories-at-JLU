###########chapter 1###########

# 生成服从标准正态分布的随机变量的时间序列
n=60
set.seed(12345)
sim.random.walk=ts(-cumsum(rnorm(n,0,1)),freq=1,start=100)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

# 生成滑动平均随机变量的时间序列
n=60
set.seed(12345)
sim.random.walk=ts(cumsum((rnorm(60)+rnorm(60))/2),freq=1,start=100)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

#生成期望均为1的指数随机变量与泊松随机变量相减的时间序列
n=60
set.seed(12345)
sim.random.walk=ts(cumsum((rexp(60,1)-rpois(60,1))),freq=1,start=100)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

# oil data-time series
library(TSA)
data(oilfilters);
plot(oilfilters,type='l',ylab='Sales')
points(y=oilfilters,x=time(oilfilters),pch=as.vector(season(oilfilters)))
#画出当前值与前期值的散点图
win.graph(width=3, height=3,pointsize=8)
plot(x=zlag(oilfilters),y=oilfilters,ylab='Sales',
     xlab='Prevous Sales')

win.graph(width=3, height=3,pointsize=8)
plot(x=zlag(oilfilters,2),y=oilfilters,ylab='当年',
     xlab='前年')












###########chapter 2###########

#生成具有不同均值函数的正态随机变量时间序列
# ut=5
set.seed(12345)
Yt=ts(5+rnorm(n),freq=1,start=0)
plot(Yt,type='o',ylab='Yt')

Xt=Yt-mean(Yt)
cc=ts(Xt,freq=1,start=0)
plot(cc,type='o',ylab='残差')

hist(cc,xlab='残差')
win.graph(width=2.5,height=2.5,pointsize=8)

qqnorm(cc)

# ut为季节均值模型

x<-seq(from=0,to=15,by=5)
u<-rep(x,times=25)

n=100
set.seed(12345)
Yt=ts(u+rnorm(n),freq=1,start=0)
plot(Yt,type='o',ylab='Yt') 

#利用Yt的周期性数据估计估计ut的四个参数
i1<-seq(from=1,to=97,by=4)
u1<-sum(Yt[i1])/25
i2<-seq(from=2,to=98,by=4)
u2<-sum(Yt[i2])/25
i3<-seq(from=3,to=99,by=4)
u3<-sum(Yt[i3])/25
i4<-seq(from=4,to=100,by=4)
u4<-sum(Yt[i4])/25

#画出残差序列的时间序列图、残差直方图、QQ图
y<-c(u1,u2,u3,u4)
ut<-rep(y,times=25)
Xt=Yt-ut
plot(Xt,type='o',ylab='残差')
hist(Xt,xlab='残差')
win.graph(width=5,height=5,pointsize=8)
qqnorm(Xt)
qqline(Xt)

# ut为线性趋势，自定ut=1+2t

ut<-seq(from=1,to=99,by=2)

n=50
set.seed(12345)
Yt=ts(ut+rnorm(n),freq=1,start=0)
plot(Yt,type='o',ylab='Yt')

t<-seq(from=0,to=49,by=1)
ut<-seq(from=1,to=99,by=2)

n=50
set.seed(12345)
Yt=ut+rnorm(n)
lm.sol<-lm(Yt ~ 1+t)
summary(lm.sol)

coefficients(lm.sol)

#根据估计的参数，做出残差序列的时间序列图、残差直方图、QQ图
ut2<-seq(from=1.12,to=99.12,by=2)
Xt=ts((Yt-ut2),freq=1,start=0)
plot(Xt,type='o',ylab='残差')
hist(Xt,xlab='残差')
win.graph(width=5,height=5,pointsize=8)
qqnorm(Xt)
qqline(Xt)







###########chapter 3###########
# 模拟MA序列
# MA（2）
set.seed(777)
Yt<-arima.sim(n=100,list(ma=c(1,1)))
plot(Yt,type="o",ylab="Yt")

#Y[t]与Y[t-2]的散点图
plot(y=Yt,x=zlag(Yt,2),ylab='Y[t]',xlab='Y[t-2]')
#Y[t]与Y[t-4]的散点图
plot(y=Yt,x=zlag(Yt,4),ylab='Y[t]',xlab='Y[t-4]')
#模型的理论ACF图
x<-c(1,2/3,1/3,rep(0,17))
plot(x,type="h",xlab="lag",ylab="ACF理论值")
#样本{Yt}的ACF图   
acf(Yt,lag=20)


#MA(5)

set.seed(777)
Yt<-arima.sim(n=200,list(ma=c(1,1,1,1,2)))
plot(Yt,type="o",ylab="Yt")


plot(y=Yt,x=zlag(Yt,4),ylab='Y[t]',xlab='Y[t-4]')

plot(y=Yt,x=zlag(Yt,6),ylab='Y[t]',xlab='Y[t-6]')

#模型的理论ACF图
x<-c(1,6/9,5/9,4/9,3/9,2/9,rep(0,14))
plot(x,type="h",xlab="lag",ylab="ACF理论值")

acf(Yt)







###########chapter 4###########

#判断可逆性
#MA（2）模型为Y[t]=e[t]-e[t-1]-e[t-2]
#相应的特征方程为：1-x-x^2=0,容易求得特征方程的根为[5^(1/2)-1]/2和[-5^(1/2)-1]/2，
#可以看出特征方程的根的模不全大于1，所以该MA（2）模型是不可逆的

#计算并画出相应参数的AR（1）模型的自相关函数图
#参数为0.6
m<-seq(from=0,by=1,to=40)
y<-0.6^m
plot(y,type="h",xlab="lag",ylab="自相关函数值")

#参数为-0.6
m<-seq(from=0,by=1,to=40)
y<-(-0.6)^m
plot(y,type="h",xlab="lag",ylab="自相关函数值")

#参数为0.95，20个滞后期
m<-seq(from=0,by=1,to=20)
y<-0.95^m
plot(y,type="h",xlab="lag",ylab="自相关函数值")

#参数为0.3
m<-seq(from=0,by=1,to=40)
y<-0.3^m
plot(y,type="h",xlab="lag",ylab="自相关函数值")

#由于题目要求模型平稳和AR特征方程两个根为共轭虚根，
#所以选取满足要求的两个根为1+i和1-i，将两根带入特征方程解出两个参数的值，
#根据计算，得到两个参数为1和-1/2，模型为Y[t]=Y[t-1]-1/2*Y[t-2]

set.seed(777)
Yt<-arima.sim(n=300,list(ar=c(1,-1/2)))
plot(Yt,type="o",ylab="Yt")

#Y[t]与Y[t-2]的散点图
library(TSA)
plot(y=Yt,x=zlag(Yt,2),ylab='Y[t]',xlab='Y[t-2]')
#出Y[t]与Y[t-3]的散点图
plot(y=Yt,x=zlag(Yt,3),ylab='Y[t]',xlab='Y[t-3]')

#理论ACF图
#根据p(0)=1，p(1)=2/3，以及p(n)=p(n-1)-1/2*p(n-2),
#建立递归函数求出p(0)到p(15)的值，并做出相应的图即为理论ACF图
p<-function(n){
  if(n==0){
    return(1)
  }
  else if(n==1){
    return(2/3)
  }
  else{
    return(p(n-1)-1/2*p(n-2))
  }
}

x<c(p(0),p(1),p(2),p(3),p(4),p(5),p(6),p(7),p(8),p(9),p(10),p(11),p(12),p(13),p(14),p(15))

plot(x,type="h",xlab="lag",ylab="ACF理论值")
abline(h=0,col="red")


#时间序列{Yt}的样本ACF图
acf(Yt,lag=15)

#分析：在样本量为300的情况下，可以看出样本的ACF图与理论ACF图很吻合，
#为考察样本ACF图与理论ACF图的吻合性是否与样本量的大小有关，
#再减少样本量到50，画出样本ACF图
#分析：可以看出样本ACF图与理论ACF图具有明显的差距，
#所以认为样本自相关系数的估计值在样本量较大的情况下更为准确、更接近理论值。










###########chapter 5###########

#产生均值为5的ARMA（1，1）的模型，要求平稳可逆，
#所以AR（1）与MA（1）的特征方程的根的模都应该大于1，于是两个参数的绝对值都应该小于1。
#于是选取参数1/2和1/3满足要求，模型为：Y[t]=1/2*Y[t-1]+e[t]-1/3*e[t-1]

set.seed(777)
Yt<-arima.sim(n=200,list(ar=1/2, ma=1/3))+5
plot(Yt,type="o",ylab="Yt")

#Y[t]与Y[t-1]的散点图
library(TSA)
plot(y=Yt,x=zlag(Yt,1),ylab='Y[t]',xlab='Y[t-1]')
#Y[t]与Y[t-4]的散点图
plot(y=Yt,x=zlag(Yt,4),ylab='Y[t]',xlab='Y[t-4]')

#模型理论ACF图
m<-seq(from=1,by=1,to=20)
n<-m-1
y<-(5/28)*(1/2)^n
plot(y,type="h",xlab="lag",ylab="ACF理论值")

#{Yt}的样本ACF图
acf(Yt,lag=20)

#生成零均值的ARIMA（2，2，1）模型，即差分两次后的时间序列为ARMA（2，1），
#要求平稳可逆，所以AR（2）与MA（1）的特征方程的根的模都应该大于1，于是三个参数的绝对值都应该小于1。
#于是选取参数1，-1/2和1/3满足要求，模型为：Y[t]=Y[t-1]-1/2*Y[t-2]+e[t]-1/3*e[t-1]

set.seed(777)
Yt<-arima.sim(n=200,list(order=c(2,2,1),ar=c(1,-1/2),ma=1/3))
plot(Yt,type="o",ylab="Yt")

#一次差分后得到的新时间序列图
y1<-diff(Yt,diff=1)
plot(y1,type="o",ylab="差分一次")

#差分两次得到的新时间序列图
y2<-diff(Yt,diff=2)
plot(y2,type="o",ylab="差分两次")











###########chapter 6###########

#生成参数为-1/2,1/2,1/4的AR（3）模型，模型平稳
set.seed(777)
Yt<-arima.sim(n=50,list(ar=c(-1/2,1/2,1/4)))
plot(Yt,type="o",ylab="Yt")

#递归算出模型的一至二十阶理论自相关系数

p<-function(n){
  if(n==0){
    return(1)
  }
  else if(n==1){
    return(-2/3)
  }
  else if(n==2){
    return(2/3)
  }
  else if(n==3){
    return(-5/12)
  }
  else{
    return(-(1/2)*p(n-1)+1/2*p(n-2)+1/4*p(n-3))
  }
}

for(i in 0:20){
  print(p(i))
}

x<-rep(0,20)
for(j in 1:20){
  x[j]=p(j)
}

plot(x,type="h",xlab="lag",ylab="ACF理论值")
abline(h=0,col="red")

#算出模型的一至二十阶样本自相关系数，不调包

##样本ACF计算
a<-mean(Yt)
x=sum((Yt-a)^2)
r=rep(0,20)
k=1;
n=50
repeat{
  i=k+1
  y=0
  repeat{
    y=y+(Yt[i]-a)*(Yt[i-k]-a)
    i=i+1
    if(i>n){
      break
    }
  }
  r[k]=y/x
  k=k+1
  if(k>20){
    break
  }
}
plot(r,type="h",xlab="lag",ylab="样本ACF值")
abline(h=2/sqrt(200),col="red",lty = 2)
abline(h=-2/sqrt(200),col="red",lty = 2)
abline(h=0)

##计算理论偏自相关系数的值
##理论pacf计算
q<-function(n,m){
  if(m==1&n==1){
    return(p(1))
  }
  else if(m==2&n==2){
    return((p(2)-p(1)^2)/(1-p(1)^2))
  }
  else if(m<n){
    return(q(n-1,m)-q(n,n)*q(n-1,n-m))
  }
  else if(m==n&m!=1&m!=2){
    b=0
    c=0
    j=1
    repeat{
      a<-q(n-1,j)*p(n-j)
      b<-b+a
      d<-q(n-1,j)*p(j)
      c<-c+d
      j=j+1
      if(j>n-1){
        break
      }
    }
    return((p(n)-b)/(1-c))
  }
}
l<-rep(0,20)
for(i in 1:8){
  l[i]=q(i,i)
}
plot(l,type="h",xlab="lag",ylab="PACF理论值")

#计算样本偏自相关系数的值
##样本pacf计算

q1<-function(n,m){
  if(m==1&n==1){
    return(r[1])
  }
  else if(m==2&n==2){
    return((r[2]-r[1]^2)/(1-r[1]^2))
  }
  else if(m<n){
    return(q1(n-1,m)-q1(n,n)*q1(n-1,n-m))
  }
  else if(m==n&m!=1&m!=2){
    b=0
    c=0
    j=1
    repeat{
      a<-q1(n-1,j)*r[n-j]
      b<-b+a
      d<-q1(n-1,j)*r[j]
      c<-c+d
      j=j+1
      if(j>n-1){
        break
      }
    }
    return((r[n]-b)/(1-c))
  }
}
Q<-rep(0,20)
for(i in 1:20){
  Q[i]=q1(i,i)
}
plot(Q,type="h",xlab="lag",ylab="样本PACF值")
















###########chapter 7###########

set.seed(777)
Yt<-arima.sim(n=50,list(ar=c(-1/2,1/2,1/4)))

##估计三个参数
#n=50
##样本acf计算

a<-mean(Yt)
x=sum((Yt-a)^2)
r=rep(0,20)
k=1;
repeat{
  i=k+1
  y=0
  repeat{
    y=y+(Yt[i]-a)*(Yt[i-k]-a)
    i=i+1
    if(i>n){
      break
    }
  }
  r[k]=y/x
  k=k+1
  if(k>20){
    break
  }
}

A<-matrix(c(1,r[1],r[2],r[1],1,r[1],r[2],r[1],1),nrow=3,ncol=3)
b<-c(r[1],r[2],r[3])
q<-solve(A,b)

##估计白噪声的方差
a<-mean(Yt)
Var_Yt<-(sum((Yt-a)^2))/(n-1)
e<-Var_Yt*(1-q[1]*r[1]-q[2]*r[2]-q[3]*r[3])
#当n=50，100,200,500,2000时


set.seed(777)
Yt<-arima.sim(n=2000,list(ma=c(1/4,1/8)))
##样本acf计算
a<-mean(Yt)
x=sum((Yt-a)^2)
r=rep(0,20)
k=1;
repeat{
  i=k+1
  y=0
  repeat{
    y=y+(Yt[i]-a)*(Yt[i-k]-a)
    i=i+1
    if(i>n){
      break
    }
  }
  r[k]=y/x
  k=k+1
  if(k>20){
    break
  }
}

###牛顿迭代法

Newtons<-function(fun, x, ep=1e-5, it_max=100){
  index<-0; k<-1 
  while (k<=it_max){
    x1 <- x; 
    obj <- fun(x); 
    x <- x - solve(obj$J, obj$f);
    norm <- sqrt((x-x1) %*% (x-x1)) 
    if (norm<ep){
      index<-1; 
      break
    }
    k<-k+1
  }
  obj <- fun(x); 
  list(root=x, it=k, index=index, FunVal= obj$f)
}

###
fun<-function(x){ 
  f<-c((-x[1]+x[1]*x[2])/(1+x[1]^2+x[2]^2)-r[1], -x[2]/(1+x[1]^2+x[2]^2)-r[2])
  J<-matrix(c((-1+x[1]^2-x[2]^2+x[2]^3-x[1]^2*x[2]+x[2])/(1+x[1]^2+x[2]^2)^2,(x[1]+x[1]^3+x[1]*x[2]^2+2*x[1]*x[2]-2*x[1]*x[2]^2)/(1+x[1]^2+x[2]^2)^2 ,-2*x[1]/(1+x[1]^2+x[2]^2)^2 ,(-1-x[1]^2+x[2]^2)/(1+x[1]^2+x[2]^2)^2 ),
            nrow=2, ncol=2)
  list(f=f, J=J)
}
Newtons(fun,c(0,0))
##估计白噪声的方差
a<-mean(Yt)
Var_Yt<-(sum((Yt-a)^2))/(n-1)
k<-c(-0.169376707,0.003549102)##当n不同时，替换为对应的牛顿迭代得到的数值解##
e<-Var_Yt/(1+k[1]^2+k[2]^2)

##结论：矩估计的方法对于AR模型比较适用，而且当样本量越来越大时，对于参数的估计越来越准确。
##但矩估计的方法对于MA模型并不是很适用，因为要通过迭代法或者其他方法才能求得参数的数值解，
##对于初值的依赖性较强而且误差较大，并且比较繁琐。











###########chapter 8###########
#从中国统计局网站中搜索得到的1970年-2020年每年小麦总产量（万吨）数据，考虑对其进行时间序列模型的建立。

#读取数据并将其改为时间序列格式，画出其时间序列图像
##读取1970年-2020年每年小麦总产量（万吨）
x<-scan("C:/Users/HP/Desktop/d.csv")
Xt<-ts(x,freq=1,start=0)
plot(Xt,type='o',ylab='Xt')

##对原始数据构成的时间序列进行单位根检验，看其是否平稳
library(tseries)
adf.test(Xt)
##差分一次看是否平稳
Xt1<-diff(Xt,lag=1)
adf.test(Xt1)
##差分两次看是否平稳
Xt2<-diff(Xt,lag=2)
adf.test(Xt2)
plot(Xt2,type='o',ylab='二次差分')

##尝试给arma模型确定阶数
acf(Xt2,lag=20)
pacf(Xt2,lag=20)
eacf(Xt2)

#分别对两个模型进行参数估计（选用极大似然估计法）
#求出两个模型的具体形式和其对应的AIC值，再根据AIC准则确定我们的模型，
#即选出两个模型中其AIC值较小的一个作为我们确定的模型。

##调用arima函数求出参数的极大似然估计值
m1<-arima(Xt,order=c(2,2,1),method='ML')
m2<-arima(Xt,order=c(4,2,1),method='ML')


#通过对残差进行分析检验模型拟合效果

##画出标准残差的图像
plot(rstandard(m1),ylab="标准残差",type="o")
abline(h=0)
qqnorm(residuals(m1))
qqline(residuals(m1))

##对残差进行正态性检验
shapiro.test(residuals(m1))

##调用tsdiag函数对残差进行诊断
tsdiag(m1,gof=15,initial=F)

