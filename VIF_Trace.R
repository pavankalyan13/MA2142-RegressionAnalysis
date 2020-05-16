library(readxl)

#library(carData)

#library(car)

Acetylene_Data<-read_excel('C:/Users/padig/Downloads/Residuals_Transformation_Outlier_Analysis/Residuals_Transformation_Outlier_Analysis/Acetylene_Data.xlsx')

ACD<-as.data.frame(Acetylene_Data)


colnames(ACD) <- c("y","x1","x2","x3","sx1", "s2x1","sx2", "s2x2","sx3", "s2x3")

ACD1<-ACD[,c(5:10)]
ACD2<-as.data.frame(ACD1)

cor(ACD2)

model1<-lm(y~sx1+sx2+sx3+s2x1+s2x2+s2x3+sx1*sx2+sx1*sx3+sx2*sx3, data = ACD)

summary(model1)



library(lmridge)

mod <-lmridge(y~sx1+sx2+sx3+s2x1+s2x2+s2x3+sx1*sx2+sx1*sx3+sx2*sx3, data = ACD, K = seq(0, 0.05, 0.005))
## Ridge trace
vif(mod)
plot(mod)
plot(mod, type = "ridge")

plot(mod, type = "vif")

summary(mod)
