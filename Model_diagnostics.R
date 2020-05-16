library(readxl)
Delivery_Data <- read_excel("C:/Users/padig/Downloads/Residuals_Transformation_Outlier_Analysis/Residuals_Transformation_Outlier_Analysis/TimeDeliveryData.xlsx")
#X<-as.data.frame(Delivery_Data)
X<-as.matrix(Delivery_Data)

y<-X[,1]
x1<-X[,2]
x2<-X[,3]
colnames(X) <- c("y","x1","x2")
mod.lm<-lm(y~x1+x2)
summary(mod.lm)


res<-rstudent(mod.lm)
fy<-fitted(mod.lm)
plot(fy,res)

plot(y,x1)
qqnorm(residuals(mod.lm))

# delete the outlier
X1<-X[-c(9),]

y1<-X1[,1]
x11<-X1[,2]
x21<-X1[,3]

plot(y1,x11)
colnames(X1) <- c("y1","x11","x21")
mod.lm1<-lm(y1~x11+x21)
summary(mod.lm1)
res1<-rstudent(mod.lm1)
fy1<-fitted(mod.lm1)
plot(fy1,res1)

qqnorm(residuals(mod.lm1))


#Leverage
lev<-lm.influence(mod.lm)$hat

levthres<-(2*3)/25


# COOK'S Distance
cd<-cooks.distance(mod.lm)


#DFBETA
dfb<-dfbeta(mod.lm)

#DFFITS
dfft<-dffits(mod.lm)

#COVRATIO
covr<-covratio(mod.lm)

All<-as.matrix(cbind(X, lev, cd, dfb, dfft, covr))

# Data cleaning 
Allm <- All[All[, 5]<3 & All[, 4]<0.39]
