install.packages("Matching")
library(Matching)
## 
##  Matching (Version 4.9-2, Build Date: 2015-12-25)
##  See http://sekhon.berkeley.edu/matching for additional documentation.
##  Please cite software as:
##   Jasjeet S. Sekhon. 2011. ``Multivariate and Propensity Score Matching
##   Software with Automated Balance Optimization: The Matching package for R.''
##   Journal of Statistical Software, 42(7): 1-52. 
##
data(lalonde)

model <- glm(treat~age+educ+black+hisp+married+nodegr,lalonde,family=binomial)
ps <- model$fitted.values
summary(model)

re.model <- lm(lalonde$re78~lalonde$treat+ps)
summary(re.model)

# IPW
y <- lalonde$re78
z1 <- lalonde$treat
(ipwe1 <- sum((z1*y)/ps)/sum(z1/ps))
# [1] 6195.964
(ipwe0 <- sum(((1-z1)*y)/(1-ps))/sum((1-z1)/(1-ps)))
# [1] 4563.501
ipwe1 - ipwe0
# [1] 1632.463

# DR
# SAMさんの例にならって関数化してあります
dre <- function(data, target, treat, ps, formula) {
       n       <- nrow(data)
       y       <- data[target]
       data1   <- data[data[treat]==1,]
       data0   <- data[data[treat]==0,]
       model1  <- lm(formula=formula, data=data1)
       model0  <- lm(formula=formula, data=data0)
       fitted1 <- predict(model1, data)
       fitted0 <- predict(model0, data)
       dre1    <- (1/n)*sum(y+((z1-ps)/ps)*(y-fitted1))
       dre0    <- (1/n)*sum(((1-z1)*y)/(1-ps)+(1-(1-z1)/(1-ps))*fitted0)
       return(c(dre1, dre0))
   }
 
   ret <- dre(lalonde, "re78", "treat", ps,
               +            re78~age+educ+black+hisp+married+nodegr)
ret
# [1] 6185.915 4566.401
ret[1] - ret[2]
# [1] 1619.514