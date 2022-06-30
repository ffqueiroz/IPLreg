
IPLreg <- function(formula, data, family, zeta = 2, type = "pML", c){
  require("PLreg")
  require("Formula")
  oformula <- Formula(formula)
  
  mf <- model.frame(oformula, data = data)
  
  y <- model.response(mf)
  ind <- ifelse(y == c, 1, 0)
  
  data.PL  <- data[ind == 0, ]
  data.glm <- cbind(data, ind)
  
  formula.PL <- formula(oformula, lhs = 1, rhs = -3)
  formula.glm <- update(formula(oformula, lhs = 1, rhs = 3), ind ~ .)
  
  fit.PL  <- PLreg(formula.PL, data = data.PL, family = family, zeta = zeta, type = type)
  fit.glm <- glm(formula.glm, family = binomial(), data = data.glm)
  
  cat("#********************************************************************************#\n")
  cat("#                         Fit for the discrete component                         #\n")
  cat("#********************************************************************************#\n")
  print(summary(fit.glm))
  cat("#********************************************************************************#\n")
  cat("#                        Fit for the continuous component                        #\n")
  cat("#********************************************************************************#\n")
  print(summary(fit.PL))
  
  X = model.matrix(oformula, data = mf, lhs = 1, rhs = -c(2,3))
  S = model.matrix(oformula, data = mf, lhs = 1, rhs = -c(1,3))
  
  alpha  <- fit.glm$fitted.values
  mu     <- fit.PL$link$median$linkinv(X%*%fit.PL$coefficients$median)
  sigma  <- fit.PL$link$dispersion$linkinv(S%*%fit.PL$coefficients$dispersion)
  lambda <- fit.PL$coefficients$skewness
  
  cdf <- alpha*as.numeric(y >= c) + (1 - alpha)*pPL(y, mu, sigma, lambda, family = family, zeta = zeta)
  if(c == 0){
    res <- ifelse(y == 0, qnorm(runif(length(y),0, alpha)), qnorm(cdf))
  }else{
    res <- ifelse(y == 1, qnorm(runif(length(y),1 - alpha, 1)), qnorm(cdf))
  }
  
  return(list(fitPL = fit.PL, fitglm = fit.glm, residual = res))
}


