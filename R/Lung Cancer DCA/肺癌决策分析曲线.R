library(rmda)
library(ggDCA)
library(ggplot2)
library(rms)
library(caret)
library(survival)  
#导入数据 
data(LIRI)
head(LIRI)
ddist <- datadist(LIRI)
options(datadist = "ddist")

#构建逻辑回归模型
lrm1 <- lrm(status ~ ANLN, LIRI)
lrm2 <- lrm(status ~ ANLN + CENPA, LIRI)
lrm3 <- lrm(status ~ ANLN + CENPA + GPR182, LIRI)
lrm4 <- lrm(status ~ ANLN + CENPA + GPR182 + BCO2, LIRI)
dca_lrm <- dca(lrm1, lrm2, lrm3, lrm4, model.names = c("ANLN", "ANLN+CENPA", "ANLN+CENPA+GPR182",
                                                       "ANLN+CENPA+GPR182+BCO2"))
head(dca_lrm)
ggplot(dca_lrm, lwd = 0.5)

#构建cox回归模型
cph1 <- cph(Surv(time, status) ~ ANLN, LIRI)
cph2 <- cph(Surv(time, status) ~ ANLN + CENPA, LIRI)
cph3 <- cph(Surv(time, status) ~ ANLN + CENPA + GPR182, LIRI)
cph4 <- cph(Surv(time, status) ~ ANLN + CENPA + GPR182 + BCO2, LIRI)
dca_cph <- dca(cph1, cph2, cph3, cph4, model.names = c("ANLN", "ANLN+CENPA", "ANLN+CENPA+GPR182",
                                                       "ANLN+CENPA+GPR182+BCO2"))
head(dca_cph)
ggplot(dca_cph, lwd = 0.5)


##多模型多时间点
dca_cph <- dca(cph1, cph2, cph3, cph4, model.names = c("ANLN", "ANLN+CENPA", "ANLN+CENPA+GPR182",
                                                       "ANLN+CENPA+GPR182+BCO2"), times = c(1.5, 2, 3))
ggplot(dca_cph)
###计算AUDC
AUDC_dca <- function(x,y){
  dca_cph = dca(x,times=y)
  return(list(dca_cph,AUDC(dca_cph)，rFP.p100(dca_cph)))
  }

AUDC_dca(cph1,2)[2]
AUDC_dca(cph2,2)[2]
AUDC_dca(cph3,2)[2]
AUDC_dca(cph4,2)[2]
