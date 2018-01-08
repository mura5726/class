dataSet = read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")

head(dataSet)

table(dataSet[,c("swang1", "death")])

1486/(698+1486) - 2236/(1315+2236)

propensityScoreForAllVal <- glm(swang1 ~ age + sex + race + edu + income + ninsclas + cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 + bili1 + alb1 + resp + card + neuro + gastr + renal + meta + hema + seps + trauma + ortho + cardiohx + chfhx + dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx + malighx + immunhx + transhx + amihx,
                                family  = binomial(link = "logit"),
                                data    = dataSet)
step(propensityScoreForAllVal)

propensityScoreModel = glm(swang1 ~ age + edu + ninsclas + cat1 + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + hema1 + sod1 + pot1 + crea1 + alb1 + resp + card + neuro + gastr + renal + hema + seps + trauma + dementhx + psychhx + renalhx + transhx, 
                           family = binomial(link = "logit"), 
                           data = dataSet)
summary(propensityScoreModel)

propensityScores = propensityScoreModel$fitted.values

library(Matching) 
propensityScoreMatching0.1 = Match(Y = as.integer(dataSet$death)-1 , Tr = (dataSet$swang1=="RHC"), X = propensityScores, M=1,caliper = 0.1, ties=FALSE, replace = FALSE)
summary(propensityScoreMatching0.1)

Y  <- as.integer(dataSet$death)-1
z1 <- dataSet$swang1=="RHC"
ipwe1 <- sum((z1*Y/propensityScores)/sum(z1/propensityScores))
ipwe0 <- sum(((1-z1)*Y)/(1-propensityScores))/sum((1-z1)/(1-propensityScores))

ipwe1 - ipwe0
