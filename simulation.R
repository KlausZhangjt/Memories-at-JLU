library(MASS)


#n=250
#n=500
#n=1000


##n=250
n1 = 250
MC = 1000
result_1 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
result_2 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
error_1 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
error_2 = matrix(rep(0,5000),nrow = 1000,ncol = 5)

var_eta_x = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_eta_x=matrix(rep(0,1000),nrow = 1000,ncol = 1)

var_eta_z = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_eta_z=matrix(rep(0,1000),nrow = 1000,ncol = 1)

cor_x_z = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_cor_x_z=matrix(rep(0,1000),nrow = 1000,ncol = 1)

for(i in 1:1000){
  set.seed(7+i)
  x1_n1 = rnorm(n1, 0, 1)
  z1_n1 = rnorm(n1, 0, 1)
  w1_n1 = rnorm(n1, 0, 1)
  w2_n1 = rnorm(n1, 0, 1)
  mean_n1<-c(0, 0) #指定均值向量
  sigma_n1<-matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2) #指定协方差矩阵
  eta_n1 <- mvrnorm(n1, mean_n1, sigma_n1) #生成1000个二元正态分布的随机数：1000行
  eta_x_n1 = eta_n1[,1]
  eta_z_n1 = eta_n1[,2]
  
  x2_n1 = 0 + 0.316*x1_n1 + 0.316*z1_n1 + 0.316*w1_n1 + 0.316*w2_n1 + eta_x_n1
  z2_n1 = 0 + 0.316*x1_n1 + 0.316*z1_n1 + 0.316*w1_n1 + 0.316*w2_n1 + eta_z_n1
  df1 <- data.frame(
    x2_n1,
    x1_n1,
    z1_n1,
    w1_n1,
    w2_n1
  )
  df2 <- data.frame(
    z2_n1,
    x1_n1,
    z1_n1,
    w1_n1,
    w2_n1
  )
  lm.model1 <- lm(x2_n1~x1_n1+z1_n1+w1_n1+ w2_n1, data = df1)
  lm.model2 <- lm(z2_n1~x1_n1+z1_n1+w1_n1+ w2_n1, data = df2)
  result_1[i,1]=coef(lm.model1)[[1]]
  error_1[i,1]= (0-coef(lm.model1)[[1]])^2
  
  result_1[i,2]=coef(lm.model1)[[2]]
  error_1[i,2]= (0.316-coef(lm.model1)[[2]])^2
  
  result_1[i,3]=coef(lm.model1)[[3]]
  error_1[i,3]= (0.316-coef(lm.model1)[[3]])^2
  
  result_1[i,4]=coef(lm.model1)[[4]]
  error_1[i,4]= (0.316-coef(lm.model1)[[4]])^2
  
  result_1[i,5]=coef(lm.model1)[[5]]
  error_1[i,5]= (0.316-coef(lm.model1)[[5]])^2
  
  
  result_2[i,1]=coef(lm.model2)[[1]]
  error_2[i,1]= (0-coef(lm.model2)[[1]])^2
  
  result_2[i,2]=coef(lm.model2)[[2]]
  error_2[i,2]= (0.316-coef(lm.model2)[[2]])^2
  
  result_2[i,3]=coef(lm.model2)[[3]]
  error_2[i,3]= (0.316-coef(lm.model2)[[3]])^2
  
  result_2[i,4]=coef(lm.model2)[[4]]
  error_2[i,4]= (0.316-coef(lm.model2)[[4]])^2
  
  result_2[i,5]=coef(lm.model2)[[5]]
  error_2[i,5]= (0.316-coef(lm.model2)[[5]])^2
  
  var_eta_x[i] = var(eta_x_n1)
  error_eta_x[i] = (1-var_eta_x[i])^2
  
  var_eta_z[i] = var(eta_z_n1)
  error_eta_z[i] = (1-var_eta_z[i])^2
  
  cor_x_z[i] = cor(eta_x_n1,eta_z_n1)
  error_cor_x_z = (0.5 - cor_x_z[i] )^2
}

apply(result_1,2,mean)
apply(result_1,2,sd)
apply(error_1,2,mean)

apply(result_2,2,mean)
apply(result_2,2,sd)
apply(error_2,2,mean)

mean(var_eta_x)
sd(var_eta_x)
mean(error_eta_x)

mean(var_eta_z)
sd(var_eta_z)
mean(error_eta_z)

mean(cor_x_z)
sd(cor_x_z)
mean(error_cor_x_z)


##n=500
n1 = 50
MC = 1000
result_1 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
result_2 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
error_1 = matrix(rep(0,5000),nrow = 1000,ncol = 5)
error_2 = matrix(rep(0,5000),nrow = 1000,ncol = 5)

var_eta_x = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_eta_x=matrix(rep(0,1000),nrow = 1000,ncol = 1)

var_eta_z = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_eta_z=matrix(rep(0,1000),nrow = 1000,ncol = 1)

cor_x_z = matrix(rep(0,1000),nrow = 1000,ncol = 1)
error_cor_x_z=matrix(rep(0,1000),nrow = 1000,ncol = 1)

for(i in 1:1000){
  set.seed(7+i)
  x1_n1 = rnorm(n1, 0, 1)
  z1_n1 = rnorm(n1, 0, 1)
  w1_n1 = rnorm(n1, 0, 1)
  w2_n1 = rnorm(n1, 0, 1)
  mean_n1<-c(0, 0) #指定均值向量
  sigma_n1<-matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2) #指定协方差矩阵
  eta_n1 <- mvrnorm(n1, mean_n1, sigma_n1) #生成1000个二元正态分布的随机数：1000行
  eta_x_n1 = eta_n1[,1]
  eta_z_n1 = eta_n1[,2]
  
  x2_n1 = 0 + 0.316*x1_n1 + 0.316*z1_n1 + 0.316*w1_n1 + 0.316*w2_n1 + eta_x_n1
  z2_n1 = 0 + 0.316*x1_n1 + 0.316*z1_n1 + 0.316*w1_n1 + 0.316*w2_n1 + eta_z_n1
  df1 <- data.frame(
    x2_n1,
    x1_n1,
    z1_n1,
    w1_n1,
    w2_n1
  )
  df2 <- data.frame(
    z2_n1,
    x1_n1,
    z1_n1,
    w1_n1,
    w2_n1
  )
  lm.model1 <- lm(x2_n1~x1_n1+z1_n1+w1_n1+ w2_n1, data = df1)
  lm.model2 <- lm(z2_n1~x1_n1+z1_n1+w1_n1+ w2_n1, data = df2)
  result_1[i,1]=coef(lm.model1)[[1]]
  error_1[i,1]= (0-coef(lm.model1)[[1]])^2
  
  result_1[i,2]=coef(lm.model1)[[2]]
  error_1[i,2]= (0.316-coef(lm.model1)[[2]])^2
  
  result_1[i,3]=coef(lm.model1)[[3]]
  error_1[i,3]= (0.316-coef(lm.model1)[[3]])^2
  
  result_1[i,4]=coef(lm.model1)[[4]]
  error_1[i,4]= (0.316-coef(lm.model1)[[4]])^2
  
  result_1[i,5]=coef(lm.model1)[[5]]
  error_1[i,5]= (0.316-coef(lm.model1)[[5]])^2
  
  
  result_2[i,1]=coef(lm.model2)[[1]]
  error_2[i,1]= (0-coef(lm.model2)[[1]])^2
  
  result_2[i,2]=coef(lm.model2)[[2]]
  error_2[i,2]= (0.316-coef(lm.model2)[[2]])^2
  
  result_2[i,3]=coef(lm.model2)[[3]]
  error_2[i,3]= (0.316-coef(lm.model2)[[3]])^2
  
  result_2[i,4]=coef(lm.model2)[[4]]
  error_2[i,4]= (0.316-coef(lm.model2)[[4]])^2
  
  result_2[i,5]=coef(lm.model2)[[5]]
  error_2[i,5]= (0.316-coef(lm.model2)[[5]])^2
  
  var_eta_x[i] = var(eta_x_n1)
  error_eta_x[i] = (1-var_eta_x[i])^2
  
  var_eta_z[i] = var(eta_z_n1)
  error_eta_z[i] = (1-var_eta_z[i])^2
  
  cor_x_z[i] = cor(eta_x_n1,eta_z_n1)
  error_cor_x_z = (0.5 - cor_x_z[i] )^2
}

apply(result_1,2,mean)
apply(result_1,2,sd)
apply(error_1,2,mean)

apply(result_2,2,mean)
apply(result_2,2,sd)
apply(error_2,2,mean)

mean(var_eta_x)
sd(var_eta_x)
mean(error_eta_x)

mean(var_eta_z)
sd(var_eta_z)
mean(error_eta_z)

mean(cor_x_z)
sd(cor_x_z)
mean(error_cor_x_z)

