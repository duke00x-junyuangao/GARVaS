implement = function(dat, generations=10){
  ###First the full model:
  mod = lm(as.formula(paste(colnames(dat)[1], "~.", sep="")), data=dat)
  ###Next the updated model using genetic algorithm:
  ga = update(dat, generations)[[2]]
  chosen = c(which(ga[,1] == 1))
  
  mod_new = lm(as.formula(paste(colnames(dat)[1], "~", paste(colnames(dat)[chosen+1], collapse = "+"),
                                sep = "")), data=dat)
  return(list(mod, mod_new))
}

########Implement the algorithm on the R built-in dataset:mtcars#######
mtcars

imp = implement(mtcars, generations=30)#Update with up to 30 generations to guarantee convergence.
mod = imp[[1]]
mod_new = imp[[2]]

AIC(mod)
AIC(mod_new)

prediction = predict(mod)
MSE = sum((prediction-mtcars[,1])^2)/nrow(mtcars)
MSE

prediction = predict(mod_new)
MSE_new = sum((prediction-mtcars[,1])^2)/nrow(mtcars)
MSE_new
#We managed to decrease the AIC at the cost of sacrificing prediction accuracy(MSE became slightly bigger.)

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

