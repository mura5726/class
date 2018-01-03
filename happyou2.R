screeplot(maker.pca, family="mono")

quartzFonts()

# ネットが繋がっていれば
rhc <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")
rhc1 <- rhc[, c(2, 4, 9:58, 61:62)] # NA とか変なカテゴリパラメタなどを削除したとして

# 生存日数
# たぶんこれが違うから、今回の結果がおかしいのだと思う。
# sadmdte	study admission date	
# dschdte	hospital discharge date	
# dthdte	date of death	
# lstctdte	date of last contact
# となっていて、sadmdte から換算して死んでいれば dthdte, 生きていれば lstctdte までの日にちを取ったわけだが…
day <- numeric(nrow(rhc))
for(i in seq(day)){
  day[i] <- c(rhc$lstctdte[i], rhc$dthdte[i])[as.integer(rhc$death[i])] - rhc$sadmdte[i]
}

# 生存解析
library(survival)
event <- as.integer(rhc$death)
group <- rhc$swang1
s1 <- survfit(Surv(time=day, event=event) ~ group)
plot(s1, col=c("blue", "red"), lwd=2)
legend("topright", legend=c("No RHC", "RHC"), col=c("blue", "red"), lwd=3, bty="n", cex=2)

# PSの計算
# RHC, すなわち Swan-Ganz カテーテルを行うか否かを各パラメータからロジスティック回帰で定量化する。
swg <- glm(swang1 ~ . - death, family = binomial(logit), data = rhc1)
D <- as.integer(rhc1$swang1)-1 # 治療する
Y <- as.integer(rhc1$death)%%2 # 結果
X <- swg$fitted.values         # PS
dens <- mapply(function(x) density(X[D==x]), 0:1, SIMPLIFY=FALSE)
xl <- c(-1, 1)*max(dens[[1]]$y, dens[[2]]$y)
yl <- c(0, 1)
par(mar=c(5, 4.5, 2, 2))
plot(0, type="n", xlim=xl, ylim=yl, xlab="", ylab="Propensity score (PS)", cex.lab=1.6, cex.axis=1.6)
abline(v=0, lty=2)
points(dens[[2]]$y, dens[[2]]$x, type="l", col="red", lwd=2)
points(-dens[[1]]$y, dens[[1]]$x, type="l", col="blue", lwd=2)
text(mean(c(0, par()$usr[1])), par()$usr[3], "No RHC", xpd=TRUE, adj=c(NA, 4), cex=1.5, col="blue")
text(mean(c(0, par()$usr[2])), par()$usr[3], "RHC", xpd=TRUE, adj=c(NA, 4), cex=1.5, col="red")
warning()
