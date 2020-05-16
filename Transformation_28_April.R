
ID_Data <- read_excel('C:/Users/padig/Downloads/Residuals_Transformation_Outlier_Analysis/Residuals_Transformation_Outlier_Analysis/Industrial_Data.xlsx')

IX<-as.matrix(ID_Data)

Iy<-IX[,2]

Ix<-IX[,1]

plot(Ix,Iy)

mod<-lm(Iy~Ix)
summary(mod)

res<-residuals(mod)
py<-fitted(mod)
plot(py,res)

# weighted 
xw<-1/Ix
modwet<-lm(Iy~Ix, weights = xw)

summary(modwet)
res1<-residuals(modwet)
py1<-predict(modwet)
plot(py1,res1)

wts1 <- 1/fitted(lm(abs(residuals(mod)) ~ fitted(mod)))^2

Wmod<-lm(Iy~Ix, weights = wts1)
wres2<-rstudent(Wmod)
wfy<-fitted(Wmod)
plot(wfy,wres2)



#Box_cox Transformation
lambda<-boxCox(Iy~Ix, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
Iy.tr<-bcPower(Iy, lambda = lambda.max)
modt<-lm(Iy.tr~Ix)
summary(modt)
rest<-residuals(modt)
pyt<-predict(modt)
plot(pyt,rest)


library(readxl)
ELCT_Data <- read_excel("/Users/satya/Desktop/Regression Analysis/R_Codes/Transformation/Electricity_Data.xlsx")

X<-as.matrix(ELCT_Data)
y<-X[,2]
x<-X[,1]

mod2<-lm(y~x)
summary(mod2)

res2<-rstandard(mod2)
fy<-fitted(mod2)
plot(fy,res2)

sy<-sqrt(y)

mod4<-lm(sy~x)

res4<-rstudent(mod4)
fy2<-fitted(mod4)
plot(fy2,res4)

#plot(mod1)
#required package car
# Box-Cox transformation
lambda<-boxCox(y~x, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
y.tr<-bcPower(y, lambda = lambda.max)
mod3<-lm(y.tr~x)
summary(mod3)
res3<-rstandard(mod3)
fy3<-fitted(mod3)
plot(fy3,res3)



WM_Data <- read_excel("/Users/satya/Desktop/Regression Analysis/R_Codes/Transformation/Wind_Mill_Data.xlsx")
WX<-as.matrix(WM_Data)
wy<-WX[,2]
wx<-WX[,1]
plot(wy,wx)

wmod<-lm(wy~wx)
summary(wmod)
wres<-rstudent(wmod)
fwy<-fitted(wmod)
plot(fwy,wres)


xlambda<-boxTidwell(wy~wx)

wx.tr<-yjPower(wx, -0.8333)
wmodn<-lm(wy~wx.tr)
summary(wmodn)
wnres<-residuals(wmodn)

plot(fwy,wnres)

# Restaurant Data

library(readxl)
Rest_Data <- read_excel("/Users/satya/Desktop/Regression Analysis/R_Codes/Transformation/Restaurant_Data.xlsx")

RX<-as.matrix(Rest_Data)
Ry<-RX[,1]
Rx<-RX[,2]
Rmod.lm<-lm(Ry~Rx)
summary(Rmod.lm)

Rres<-residuals(Rmod.lm)
Py<-predict(Rmod.lm)
Fy<-fitted(Rmod.lm)
plot(Py,abs(Rres))

wts <- 1/fitted(lm(abs(residuals(Rmod.lm)) ~ fitted(Rmod.lm)))^2

WRmod.lm<-lm(Ry~Rx, weights = wts)
summary(WRmod.lm)
res5<-rstudent(WRmod.lm)
fy5<-fitted(WRmod.lm)
plot(fy5,res5)



plot(WRmod.lm)