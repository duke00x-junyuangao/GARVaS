########Implement the algorithm on the R built-in dataset:mtcars#######
mtcars

mod = lm(mpg~., data = mtcars) #Full model
result = select(mtcars, generations = 30)
mod_new = result[[4]]
#Updated model. Use 30 generations to guarantee convergence.

chosen = result[[2]]
chosen
#Take a look at chosen variables. Many replicates show good convergence, three variables were singled out.
chosen_ind = c(which(chosen[,1] == 1))
colnames(mtcars[,chosen_ind + 1])
#Only 3 variables out of 10 were chosen, dimension got reduced significantly.
#The three chosen predictors are "wt", "qsec" and "am".

AIC(mod)
AIC(mod_new)

prediction = predict(mod)
MSE = sum((prediction-mtcars[,1])^2)/nrow(mtcars)
MSE

prediction = predict(mod_new)
MSE_new = sum((prediction-mtcars[,1])^2)/nrow(mtcars)
MSE_new
#We managed to decrease the AIC at the cost of sacrificing prediction accuracy (MSE became slightly bigger.)

###Now plot to check normality
par(mfrow=c(1,2), mar=rep(4,4))
plot(fitted(mod), residuals(mod),xlab="Fitted Value", ylab="Residual(Error)",
     main="Residual Plot (full)")
plot(fitted(mod_new), residuals(mod_new),xlab="Fitted Value", ylab="Residual(Error)",
     main="Residual Plot (updated)")

qqnorm(residuals(mod), main="Normal Q-Q Plot (full)")
qqline(residuals(mod))
qqnorm(residuals(mod_new), main="Normal Q-Q Plot (updated)")
qqline(residuals(mod_new))
#Applying genetic algorithm impairs normality of the model by introducing patterns in residuals.
#This is reasonable because by throwing away most of the predictors (kept 3 out of 10), we are trading completeness for speed.

