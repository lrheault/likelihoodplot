#!/usr/bin/Rscript

# Illustrating the objective functions of OLS and MLE
# for a simple linear regression model.

library(plotly)

# Sum of squared residuals function.
SSE = function(alpha, beta, X, y) {
  b = as.matrix(c(alpha,beta))
  yhat = X %*% b
  return(sum((y - yhat)^2))
}

# Log-likelihood function.
LL = function(alpha, beta, X, y){
  b = as.matrix(c(alpha,beta))
  yhat = X %*% b
  ll = sum(dnorm(y - yhat, mean = 0, sd = 2, log = TRUE))
  # Fixing sd at 2 to facilitate visualization.
  return(ll)
}

# Sample data.
X = as.matrix(cbind(1,rnorm(1000,0,3)))
y = as.matrix(2*X[,2] + rnorm(1000, 0, 2))

# Parameter values to build the plots.
alphas = seq(from=-3,to=3,length.out=100)
betas = seq(from=0,to=4,length.out=100)

# Values of SSE for surface plot.
zols = matrix(nrow=100, ncol=100)
for (i in 1:length(alphas)){
  for (j in 1:length(betas)){
     zols[i,j] = SSE(alphas[i],betas[j],X,y)
  }
}
# Values of LL for surface plot.
zmle = matrix(nrow=100, ncol=100)
for (i in 1:length(alphas)){
  for (j in 1:length(betas)){
    zmle[i,j] = LL(alphas[i],betas[j],X,y)
  }
}

# OLS plot.
p = plot_ly(x=alphas, y=betas, z=zols) %>% add_surface() %>%
  layout(
    title = "Ordinary Least Squares",
    scene = list(
      xaxis = list(title = "Intercept"),
      yaxis = list(title = "Slope"),
      zaxis = list(title = "Sum of Squared Residuals")
    ))
p

# MLE plot.
p = plot_ly(x=alphas, y=betas, z=zmle) %>% add_surface() %>%
  layout(
    title = "Maximum Likelihood Estimation",
    scene = list(
      xaxis = list(title = "Intercept"),
      yaxis = list(title = "Slope"),
      zaxis = list(title = "Log-Likelihood")
    ))
p
