## plotting
# install.packages("ggplot2")
library(ggplot2)
## ROC
# install.packages("pROC")
library(pROC)
## Load nonrandom for PS matching
# install.packages("nonrandom")
library(nonrandom)
## Load Matching for PS matching
# install.packages("Matching")
library(Matching)
## Load sandwich package for robust sandwich covariance matrix estimators
# install.packages("sandwich")
library(sandwich)
## Load lmtest package for coeftest
# install.packages("lmtest")
library(lmtest)
## Load geepack for robust sandwich covariance matrix estimators
# install.packages("geepack")
library(geepack)

## Show outcome (death) and exposure (swang1)
addmargins(table(rhc[,c("swang1", "death")]))

## Create weights for plotting
rhc$one <- 1

## Define function to obtain ORs with 95% confidence intervals from a result matrix
GetConfInt <- function(obj) {
  
  logitsticModel <- FALSE
  
  if (identical(class(obj), c("glm", "lm")) == TRUE) {
    
    mat <- coef(summary(obj))
    logitsticModel <- TRUE
    
  } else if (identical(class(obj), c("geeglm", "gee", "glm")) == TRUE) {
    
    mat <- coef(summary(obj))
    
  } else if (identical(class(obj), c("coeftest")) == TRUE) {
    
    mat <- obj
    
  } else if (identical(class(obj), c("matrix")) == TRUE) {
    
    mat <- obj
    
  } else {
    
    stop("Not a supported object")
  }
  
  ## Add point estimates
  matRes <- mat[, 1, drop = F]
  
  ## 1.96 * SE
  matSe <-  mat[, 2, drop = F] * qnorm(0.975)
  
  ## Estimate, lower 95% limit, upper 95% limit
  matRes <- cbind(matRes, (matRes - matSe), (matRes + matSe))
  
  ## Name
  colnames(matRes) <- c("OR","lower","upper")
  
  ## Exponentiate
  matRes <- exp(matRes)
  
  ## Add p-value
  matRes <- cbind(matRes, mat[, 3:4, drop = F])
  
  if (logitsticModel == TRUE) {
    
    matRes[, c("lower","upper")] <- exp(suppressMessages(confint(obj)))
  }
  
  ## Show
  matRes
}

#「通常の」ロジスティック回帰を実行する

## Crude analysis (confounded!)
glmCrude <- glm(formula = death ~ swang1,
                family  = binomial(link = "logit"),
                data    = rhc)
GetConfInt(glmCrude)

## Fit a "regular" fully adjusted model
glmFull <- glm(formula =  death ~ swang1 + age + sex + race + edu + income + ninsclas + cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 + bili1 + alb1 + resp + card + neuro + gastr + renal + meta + hema + seps + trauma + ortho + cardiohx + chfhx + dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx + malighx + immunhx + transhx + amihx,
               family  = binomial(link = "logit"),
               data    = rhc)
## Show result
GetConfInt(coef(summary(glmFull)))

#傾向スコアモデルを構築する
## PS model
psModel <- glm(formula = swang1 ~ age + sex + race + edu + income + ninsclas + cat1 + das2d3pc + dnr1 + ca + surv2md1 + aps1 + scoma1 + wtkilo1 + temp1 + meanbp1 + resp1 + hrt1 + pafi1 + paco21 + ph1 + wblc1 + hema1 + sod1 + pot1 + crea1 + bili1 + alb1 + resp + card + neuro + gastr + renal + meta + hema + seps + trauma + ortho + cardiohx + chfhx + dementhx + psychhx + chrpulhx + renalhx + liverhx + gibledhx + malighx + immunhx + transhx + amihx,
               family  = binomial(link = "logit"),
               data    = rhc)

## PS (predicted probability of treatment)
rhc$ps <- predict(psModel, type = "response")

## PS model diagnostic by ROC
rocPsModel <- roc(swang1 ~ ps, data = rhc)
plot(rocPsModel, legacy.axes = TRUE)

## Define a function to create density data (x = ps, y = density) frame given weights
CreateDataset <- function(data = rhc, ps = "ps", weights = "one") {
  ## Density for overall cohort
  dfDataAll <- suppressWarnings(density(x = data[,ps], weights = data[,weights]))
  dfDataAll <- data.frame(dfDataAll[c("x","y")])
  dfDataAll$group <- "All"
  
  ## Density for RHC
  dfDataRhc <- suppressWarnings(density(x = subset(data, swang1 == "RHC")[,ps], weights = subset(data, swang1 == "RHC")[,weights]))
  dfDataRhc <- data.frame(dfDataRhc[c("x","y")])
  dfDataRhc$group <- "RHC"
  
  ## Density for No RHC
  dfDataNoRhc <- suppressWarnings(density(x = subset(data, swang1 == "No RHC")[,ps], weights = subset(data, swang1 != "RHC")[,weights]))
  dfDataNoRhc <- data.frame(dfDataNoRhc[c("x","y")])
  dfDataNoRhc$group <- "No RHC"
  
  ## combine datasets
  dfData <- rbind(dfDataAll, dfDataRhc, dfDataNoRhc)
  
  ## Return
  dfData
}

## Create dataset with weight = 1 for every subject
dfData <- CreateDataset(data = rhc, ps = "ps", weights = "one")

## Framewrok only (no layer)
plotBase <- ggplot(data = dfData,
                   mapping = aes(x = x, y = y, color = group)) +
  scale_x_continuous(limit = c(-0.1,1.1), breaks = c(0,0.5,1.0)) +
  scale_y_continuous(breaks = NULL) +
  ## scale_color_manual(values = c("All" = "black", "RHC" = "red", "No RHC" = "blue")) +
  scale_color_manual(values = c("All" = "#999999", "RHC" = "#D55E00", "No RHC" = "#0072B2")) +
  labs(title = "PS distribution by treatment", x = "Propensity score", y = "Distribution") +
  theme_bw() +
  theme(legend.key = element_blank())

## Plot with total
plotOrig <- layer(geom = "line")
plotBase + plotOrig
