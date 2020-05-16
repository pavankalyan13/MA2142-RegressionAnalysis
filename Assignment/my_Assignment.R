#the octane rating of a particular petrol was measured as a
#    function of the 3 raw materials,
#    and a variable that characterized the manufacturing conditions

#There are 82 rows of data.  The data include:
#
#      I,  the index;
#      A1, amount of material 1;
#      A2, amount of material 2;
#      A3, amount of material 3;
#      A4, manufacturing condition rating;
#      B,  the octane rating;
#

#We seek a model of the form:
#      B = A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4.

library(readxl)

read_excel("C:/Users/padig/Desktop/Book1.xlsx") -> octane_data

OD<-as.data.frame(octane_data)


#----------------------------
#library("writexl")


#=================================SPlit the data into train and validation into 3:2 ratio============================
#library(caTools)

#sample.split(OD,SplitRatio = 0.60) -> split_index

# this spilliting produces a random split each time I run. so this is creating a problem when I am extracting values after cleaning the data
# and storing into a 1D array. 
# so I am storing the data from data frame it produces into a excel sheel

#OD_train <- subset(OD,split_index == T)
#OD_valid <- subset(OD,split_index == F)

#colnames(OD_train) <- c("A1","A2","A3","A4","y")
#colnames(OD_valid) <- c("A1","A2","A3","A4","y")

#write_xlsx(OD_train,"C:/Users/padig/Desktop/assig/train.xlsx")
#write_xlsx(OD_valid,"C:/Users/padig/Desktop/assig/valid.xlsx")

#========================================================================================================================


read_excel("C:/Users/padig/Desktop/assig/Assignment/train.xlsx") -> octane_train
read_excel("C:/Users/padig/Desktop/assig/Assignment/valid.xlsx") -> octane_valid


# convert them into data frames 

OD_train<-as.data.frame(octane_train)
OD_valid<-as.data.frame(octane_valid)


#====================================================================================================================

OD_Matrix_train <- as.matrix(OD_train)

y <- OD_Matrix_train[,5]
x1 <- OD_Matrix_train[,1]
x2 <- OD_Matrix_train[,2]
x3 <- OD_Matrix_train[,3]
x4 <- OD_Matrix_train[,4]

colnames(OD_Matrix_train) <- c("A1","A2","A3","A4","y")

#-----------Normalize data--------------
sA1 = (x1 - mean(x1))/sd(x1)
sA2 = (x2 - mean(x2))/sd(x2)
sA3 = (x3 - mean(x3))/sd(x3)
sA4 = (x4 - mean(x4))/sd(x4)

sy = (y - mean(y))/sd(y)

s2A1 = sA1 * sA1;
s2A2 = sA2 * sA2;
s2A3 = sA3 * sA3;
s2A4 = sA4 * sA4;


OD_train <- cbind(OD_train, sy)
OD_train <- cbind(OD_train, sA1)
OD_train <- cbind(OD_train, sA2)
OD_train <- cbind(OD_train, sA3)
OD_train <- cbind(OD_train, sA4)
OD_train <- cbind(OD_train, s2A1)
OD_train <- cbind(OD_train, s2A2)
OD_train <- cbind(OD_train, s2A3)
OD_train <- cbind(OD_train, s2A4)


model1 <- lm(y~ A1 + A2 + A3 + A4,data=OD_train)
summary(model1)


#-----------------------checking for multicollinearity by checking for interaction between regressors and their significance----------
model_1<-lm(y~sA1+sA2+sA3+sA4+s2A1+s2A2+s2A3+s2A4+ sA1*sA2+sA1*sA3+sA1*sA4+sA2*sA3+sA2*sA4+sA3*sA4, data = OD_train)
summary(model_1)


model_2 <- lm(y~sA1+sA2+sA3+sA4, data = OD_train)
summary(model_2)
plot(x1,y)

# plot(sA1,sy)

# cor matrix for multicollinearlity. corelation between A_i's



#-------------------------------------------------------------------------------

#Confidence interval for the mean response
predict(model1,OD_valid,interval = "confidence")

# Confidence interval for the new observation
predict(model1,OD_valid,interval = "prediction")


#-------------------------------------------------------------------------------



# studentized residuals 
rstandardized <- rstandard(model1)

# R-Student residuals 

res <- rstudent(model1)

# press residuals 

pr <- residuals(model1)/(1-lm.influence(model1)$hat)


# ---------------------outlier analysis------------------------


fy <- fitted(model1)
plot(fy,res)



#-----------------------------plot qq norm to check for the regression asumptions e ~ N(0,sigma^2) follows normal probability plot
qqnorm(residuals(model1))


#Leverage
lev<-lm.influence(model1)$hat

levthres<-(2*5)/49

# COOK'S Distance
cd<-cooks.distance(model1)

#DFBETA
dfb<-dfbeta(model1)

#DFFITS
dfft<-dffits(model1)

#COVRATIO
covr<-covratio(model1)

All<-as.matrix(cbind(OD_Matrix_train, lev, cd, dfb, dfft, covr))

# Data cleaning 
Allm <- All[All[, 6]<0.12195 & All[, 7]< 1]



# after cleaning the train data set we get the following 

All_final <- data.frame(A1 = Allm[c(1:41)],A2=Allm[c(42:82)],A3=Allm[c(83:123)],A4=Allm[c(124:164)],y=Allm[c(165:205)])

# manualling deleting the outlier from the plot it turns out 
# row 29 and 39 are outliers
# deleting them

All_final1 <- All_final[-c(29,39),]

x1new <- All_final1[,1]
x2new <- All_final1[,2]
x3new <- All_final1[,3]
x4new <- All_final1[,4]
ynew <- All_final1[,5]






#-----------------------------plot qq norm to check for the regression asumptions e ~ N(0,sigma^2)------------
qqnorm(residuals(model1))

#-----------------------------plot qq norm afte cleaning the data(outlier analysis) and build a model linear model------


# from model1 we know the significant regressors are A1,A2,A4 so build a model based on that 

model_after <- lm(y~ A1 + A2 + A4,data=All_final1)
summary(model_after)

qqnorm(residuals(model_after))

plot(x1,y)
plot(x2,y)
plot(x3,y)
plot(x4,y)


plot(x1new,ynew)
plot(x2new,ynew)
plot(x3new,ynew)
plot(x4new,ynew)


# studentized residuals 
rstandardized1 <- rstandard(model_after)

# R-Student residuals 

res1 <- rstudent(model_after)

# press residuals 

pr1 <- residuals(model_after)/(1-lm.influence(model_after)$hat)

fy1 <- fitted(model_after)
plot(fy1,res1)


#Confidence interval for the mean response
predict(model_after,OD_valid,interval = "confidence")

# Confidence interval for the new observation
predict(model_after,OD_valid,interval = "prediction")


#========================================================================================================================

##--multicollinearity___ploting trace plot to find a min MSE at a K



OD1 <- All_final1[,c(1:4)]
cor(OD1)

# AS the cor is symmetric matrix and there is no value whose corelation is greater than 0.5(except diagonal) which says that there is no multicollinearlity
# in the data 

library(lmridge)

mod <- lmridge(y~ A1 + A2 + A3 + A4,data=All_final, K=seq(0,0.05,0.005))
vif(mod)
plot(mod,type="ridge")

plot(mod,type="vif")



##----------Weighted least squares-----------------------------------------------



IX<-as.matrix(All_final1)

Iy<-IX[,5]

Ix1<-IX[,1]
Ix2<-IX[,2]
Ix3<-IX[,3]
Ix4<-IX[,4]

# weighted 
xw<-1/Ix1
modwet<-lm(Iy~Ix1+Ix2+Ix3+Ix4, weights = xw)

summary(modwet)
res1<-residuals(modwet)
py1<-predict(modwet)
plot(py1,res1)


#this is actually enough as you can see from the residual plot.
# AS the residuals are scattered around 0


#but still trying out weighted square


wts1 <- 1/fitted(lm(abs(residuals(model_after)) ~ fitted(model_after)))^2

Wmod<-lm(Iy~Ix1+Ix2+Ix3+Ix4, weights = wts1)
summary(Wmod)
wres2<-rstudent(Wmod)
wfy<-fitted(Wmod)
plot(wfy,wres2)

# the output shows a small difference in the residuals vs fitted plot

#I couldn't find the library for boxcox function 



library("geoR")
library('MASS')
library('car')

#Box_cox Transformation
lambda<-boxCox(Iy~Ix1+Ix2+Ix3+Ix4, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
Iy.tr<-bcPower(Iy, lambda = lambda.max)
modt<-lm(Iy.tr~Ix1+Ix2+Ix3+Ix4)
summary(modt)
rest<-residuals(modt)
pyt<-predict(modt)
plot(pyt,rest)

# in boxcox the residuals are scattered but their residuals is very large 

#----------------------------------------------------------------------------------------------------------------



library(olsrr) 


ols_step_all_possible(model1)

ols_step_forward_p(model1,prem = 0.05)

ols_step_backward_p(model1,prem = 0.05)

ols_step_both_p(model1, details = TRUE)


ols_step_backward_aic(model1,details = TRUE)

ols_step_forward_aic(model1,details = TRUE)
AIC1<-AIC(lm(y~A1,data = All_final1))
AIC2<-AIC(lm(y~A2,data = All_final1))
AIC3<-AIC(lm(y~A3,data = All_final1))
AIC4<-AIC(lm(y~A4,data = All_final1))

AIC12<-AIC(lm(y~A1+A2,data = All_final1))
AIC13<-AIC(lm(y~A1+A3,data = All_final1))
AIC14<-AIC(lm(y~A1+A4,data = All_final1))
AIC23<-AIC(lm(y~A2+A3,data = All_final1))
AIC24<-AIC(lm(y~A2+A4,data = All_final1))
AIC34<-AIC(lm(y~A3+A4,data = All_final1))



AIC123<-AIC(lm(y~A1+A2+A3,data = All_final1))
AIC124<-AIC(lm(y~A1+A2+A4,data = All_final1))
AIC134<-AIC(lm(y~A1+A3+A4,data = All_final1))
AIC234<-AIC(lm(y~A2+A3+A4,data = All_final1))

AIC1234<-AIC(lm(y~A1+A2+A3+A4,data = All_final1))

AIC_Vals<-c(AIC1, AIC2, AIC3, AIC4, AIC12, AIC13, AIC14, AIC23, AIC24, AIC34, AIC123, AIC124, AIC134, AIC234, AIC1234)

BIC1<-BIC(lm(y~A1,data = All_final1))
BIC2<-BIC(lm(y~A2,data = All_final1))
BIC3<-BIC(lm(y~A3,data = All_final1))
BIC4<-BIC(lm(y~A4,data = All_final1))

BIC12<-BIC(lm(y~A1+A2,data = All_final1))
BIC13<-BIC(lm(y~A1+A3,data = All_final1))
BIC14<-BIC(lm(y~A1+A4,data = All_final1))
BIC23<-BIC(lm(y~A2+A3,data = All_final1))
BIC24<-BIC(lm(y~A2+A4,data = All_final1))
BIC34<-BIC(lm(y~A3+A4,data = All_final1))

BIC123<-BIC(lm(y~A1+A2+A3,data = All_final1))
BIC124<-BIC(lm(y~A1+A2+A4,data = All_final1))
BIC134<-BIC(lm(y~A1+A3+A4,data = All_final1))
BIC234<-BIC(lm(y~A2+A3+A4,data = All_final1))

BIC1234<-BIC(lm(y~A1+A2+A3+A4,data = All_final1))

BIC_Vals<-c(BIC1, BIC2, BIC3, BIC4, BIC12, BIC13, BIC14, BIC23, BIC24, BIC34, BIC123, BIC124, BIC134, BIC234, BIC1234)

name<-c("1","2","3","4","12","13","14","23","24","34","123","124","134","234","1234")
AIC_BIC<-data.frame(name, AIC_Vals, BIC_Vals)

AIC_BIC



####################################################--RESULTS---#######################################

#  based on R^2 we should choose a model with ( A1,A2,A4) , (A1,A3,A4) , (A1,A4)

#  based on  R^2_adj we should choose a model with (A1,A2,A4) , (A1,A3,A4) , (A1,A4)

# based on mallow cp we should choose a model wth (A1,A2,A4)

# based on AIC we should choose (A1,A2,A4)

# based on BIC we should choose (A1,A4)

#########################################################################################################

#-----finally from the results we have 3 models which are good ( with decreasing order of preference)
# they are model with  
# 1. regressors A1,A2,A4
# 2. regressors A1,A3,A4
# 3. regressors A1, A4


#------------------------------------------------------
M_1 <- lm(y~A1+A2+A4, data = OD_train)
summary(M_1)

M_valid_1 <- lm(y~A1+A2+A4, data = OD_valid)
summary(M_valid_1)

#-------------------------------------------------------
M_2 <- lm(y~A1+A3+A4, data = OD_train)
summary(M_2)

M_valid_2 <- lm(y~A1+A3+A4, data = OD_valid)
summary(M_valid_2)
#------------------------------------------------------
M_3 <- lm(y~A1+A4, data = OD_train)
summary(M_3)

M_valid_3 <- lm(y~A1+A4, data = OD_valid)
summary(M_valid_3)

# Comparing all the values of R^2, R^2_adj and residual error the best model to be taken is the one which includes the regressors 
# A1 and A4


