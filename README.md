# MA2142-RegressionAnalysis
R codes for performing Regression analysis 

split the given data into train and validation data  sets

calculated residuals (Studentized residuals) 
performed outlier analysis using various criteria like
i) cook's distance
ii) DF BETAS
iii) COV ratio


checked for multicollinearity from cov matrix

result no multicollinearity 


ploted ridge plot 

After we fit the model, number of significant regressors are 
A1,A4,A2 and intercept 

overall model is significant 
and in case of individual significance A1,A2,A4

This means octane rating depends on amount of material 1,2 and 4

summary(model) tells that they are 4 variables and 44 degrees of freedom

sum of squares of residuals( residual standard error) is 0.4395
