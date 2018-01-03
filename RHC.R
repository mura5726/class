install.packages("data.table")
library(data.table)



dataSet = fread("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")
dat = dataSet[,-1] 
dat
table(dat[,c("swang1", "death")])
attach(dat)
# detach(2)
dat[,"swang1"] = (dat[,"swang1"] == "RHC")*1
dat[,"sex"] = (dat[,"sex"] == "Male")*1
dat[,"race"] = (swang1 == "black")*1

propensityScoreForAllVal <- glm(swang1 ~ age + sex + race + edu + income + ninsclas + cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 + bili1 + alb1 + resp + card + neuro + gastr + renal + meta + hema + seps + trauma + ortho + cardiohx + chfhx + dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx + malighx + immunhx + transhx + amihx,
                                family  = binomial(link = "logit"),
                                data    = dataSet)
step(propensityScoreForAllVal)
      
write.csv(str(dat),"str.csv")
getwd()
?str
str(dat)