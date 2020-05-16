library(readxl)

Time_Delivery_Data<-read_excel('C:/Users/padig/Downloads/Residuals_Transformation_Outlier_Analysis/Residuals_Transformation_Outlier_Analysis/Electricity_Data.xlsx')

TDD<-as.data.frame(Time_Delivery_Data)

colnames(TDD) <- c("y","x1","x2")

model1<-lm(y~x1+x2, data = TDD)

summary(model1)

residuals_p<-residuals(model1)

# Studentized residuals
rstandardized<-rstandard(model1)

# R-Student residuals
rstudent<-rstudent(model1)

# PRESS residuals
pr <- residuals(model1)/(1-lm.influence(model1)$hat)

