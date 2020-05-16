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



#=================================SPlit the data into train and validation into 3:2 ratio============================
library(caTools)

sample.split(OD,SplitRatio = 0.60) -> split_index

OD_train <- subset(OD,split_index == T)
OD_valid <- subset(OD,split_index == F)
colnames(OD_train) <- c("A1","A2","A3","A4","y")
colnames(OD_valid) <- c("A1","A2","A3","A4","y")

#====================================================================================================================

OD_Matrix_train <- as.matrix(OD_train)
y <- OD_Matrix_train[,5]
x1 <- OD_Matrix_train[,1]
x2 <- OD_Matrix_train[,2]
x3 <- OD_Matrix_train[,3]
x4 <- OD_Matrix_train[,4]

colnames(OD_Matrix_train) <- c("A1","A2","A3","A4","y")

OD1 <- OD_Matrix_train[,c(1:4)]
OD2 <- as.data.frame(OD1)

cor(OD2)

model1 <- lm(y~ A1 + A2 + A3 + A4,data=OD_train)
summary(model1)


#========================================================================================================================

##---Q-5__(b)___multicollinearity___ploting trace plot to find a min MSE at a K

library(lmridge)

mod <- lmridge(y~ A1 + A2 + A3 + A4,data=OD_train, K=seq(0,0.05,0.005))
vif(mod)
plot(mod,type="ridge")

plot(mod,type="vif")

# studentized residuals 
rstandardized <- rstandard(model1)

# R-Student residuals 

res <- rstudent(model1)

# press residuals 

pr <- residuals(model1)/(1-lm.influence(model1)$hat)



# ---------------------outlier analysis------------------------


fy <- fitted(model1)
plot(fy,res)



#-----------------------------plot qq norm to check for the regression asumptions e ~ N(0,sigma^2)------------
qqnorm(residuals(model1))


plot(x1,y)
plot(x2,y)
plot(x3,y)


#Leverage
lev<-lm.influence(model1)$hat

levthres<-(2*5)/82


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
Allm <- All[All[, 7]<0.12195 & All[, 8]< 1]



# after cleaning the train data set we get the following 

All_final <- data.frame(A1 = Allm[c(1:46)],A2=Allm[c(47:92)],A3=Allm[c(93:138)],A4=Allm[c(139:184)],A5=Allm[c(185:230)])


#----------------------------------------------------------------------------------------------------------------


library(olsrr) 


ols_step_all_possible(model1)

ols_step_forward_p(model1,prem = 0.05)

ols_step_backward_p(model1,prem = 0.05)

ols_step_both_p(model1, details = TRUE)


ols_step_backward_aic(model1,details = TRUE)

ols_step_forward_aic(model1,details = TRUE)
AIC1<-AIC(lm(y~x1,data = OD_train))
AIC2<-AIC(lm(y~x2,data = OD_train))
AIC3<-AIC(lm(y~x3,data = OD_train))
AIC4<-AIC(lm(y~x2,data = OD_train))

AIC12<-AIC(lm(y~x1+x2,data = OD_train))
AIC13<-AIC(lm(y~x1+x3,data = OD_train))
AIC14<-AIC(lm(y~x1+x4,data = OD_train))
AIC23<-AIC(lm(y~x2+x3,data = OD_train))
AIC24<-AIC(lm(y~x2+x4,data = OD_train))
AIC34<-AIC(lm(y~x3+x4,data = OD_train))



AIC123<-AIC(lm(y~x1+x2+x3,data = OD_train))
AIC124<-AIC(lm(y~x1+x2+x4,data = OD_train))
AIC134<-AIC(lm(y~x1+x3+x4,data = OD_train))
AIC234<-AIC(lm(y~x2+x3+x4,data = OD_train))

AIC1234<-AIC(lm(y~x1+x2+x3+x4,data = OD_train))

AIC_Vals<-c(AIC1, AIC2, AIC3, AIC4, AIC12, AIC13, AIC14, AIC23, AIC24, AIC34, AIC123, AIC124, AIC134, AIC234, AIC1234)

BIC1<-BIC(lm(y~x1,data = OD_train))
BIC2<-BIC(lm(y~x2,data = OD_train))
BIC3<-BIC(lm(y~x3,data = OD_train))
BIC4<-BIC(lm(y~x2,data = OD_train))

BIC12<-BIC(lm(y~x1+x2,data = OD_train))
BIC13<-BIC(lm(y~x1+x3,data = OD_train))
BIC14<-BIC(lm(y~x1+x4,data = OD_train))
BIC23<-BIC(lm(y~x2+x3,data = OD_train))
BIC24<-BIC(lm(y~x2+x4,data = OD_train))
BIC34<-BIC(lm(y~x3+x4,data = OD_train))

BIC123<-BIC(lm(y~x1+x2+x3,data = OD_train))
BIC124<-BIC(lm(y~x1+x2+x4,data = OD_train))
BIC134<-BIC(lm(y~x1+x3+x4,data = OD_train))
BIC234<-BIC(lm(y~x2+x3+x4,data = OD_train))

BIC1234<-BIC(lm(y~x1+x2+x3+x4,data = OD_train))

BIC_Vals<-c(BIC1, BIC2, BIC3, BIC4, BIC12, BIC13, BIC14, BIC23, BIC24, BIC34, BIC123, BIC124, BIC134, BIC234, BIC1234)

name<-c("1","2","3","4","12","13","14","23","24","34","123","124","134","234","1234")
AIC_BIC<-data.frame(name, AIC_Vals, BIC_Vals)

AIC_BIC




