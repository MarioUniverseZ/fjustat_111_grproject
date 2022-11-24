library(psych)
library(ppcor)
library(parallel)
library(nFactors)
library(GPArotation)
library(readxl)
library(dplyr)

g1 <- read.csv("D:\\g1.csv")
g2 <- read.csv("D:\\g2.csv")
g3 <- read.csv("C:\\Users\\Administrator\\OneDrive\\文件\\人格特質分群3.csv")
g1 <- g1[4:23]
g2 <- g2[4:23]
g3 <- g3[4:23]

#因素分析
correlation1 <- cov(g1)
correlation2 <- cov(g2)
correlation3 <- cov(g3)
# 
fa.parallel(correlation1, n.obs = nrow(g1), fa = "fa", main = "碎石圖", sim = TRUE)
fa.parallel(correlation2, n.obs = nrow(g2), fa = "fa", main = "碎石圖", sim = TRUE)
fa.parallel(correlation3, fa = "fa", main = "碎石圖")
# 
fa1 <- fa(correlation1, nfactors = 5, n.obs = 326, rotate = "varimax", fm = "pa")
fa2 <- fa(correlation2, nfactors = 5, n.obs = 36, rotate = "geominT", fm = "pa")
fa1a <- fa(correlation1, nfactors = 4, n.obs = 326, rotate = "varimax", fm = "pa")
fa2a <- fa(correlation2, nfactors = 4, n.obs = 36, rotate = "varimax", fm = "pa")
fa3 <- fa(correlation3, nfactors = 5, rotate = "varimax", fm = "pa")
fa1
fa2
fa1a
fa2a
fa3

View(factor.stats(correlation1, fa1, n.obs = 326, alpha = .05))
View(factor.stats(correlation1, fa1a, n.obs = 326, alpha = .05))
View(factor.stats(correlation1, fa2, n.obs = 36, alpha = .05))
View(factor.stats(correlation1, fa2a, n.obs = 36, alpha = .05))

fa.diagram(fa1, digits = 3)
fa.diagram(fa2, digits = 3)
fa.diagram(fa2a, digits = 3)
fa.diagram(fa3, digits = 3)

#SEM
library(lavaan)
library(semPlot)

model1 <- '
#measurement model
        親和性 =~ Q5 + Q6 + Q7 + Q8
        神經質 =~ Q13 + Q14 + Q15 + Q16
        外向性 =~ Q1 + Q2 + Q3 + Q4
        開放性 =~ Q17 + Q18 + Q19 + Q20
        謹慎性 =~ Q9 + Q10 + Q11 + Q12
        #covariances
        親和性 ~~ 神經質 + 外向性 + 開放性 + 謹慎性
        神經質 ~~ 外向性 + 開放性 + 謹慎性
        外向性 ~~ 開放性 + 謹慎性
        開放性 ~~ 謹慎性
        Q5 ~~ Q6
        Q9 ~~ Q10
        Q12 ~~ Q20
        Q13 ~~ Q14
        Q1 ~~ Q2 + Q3 + Q4
        '

model2 <- '
        #measurement model
        開放性 =~ Q1 + Q3 + Q4 + Q17 + Q18 + Q19 + Q20
        謹慎性 =~ Q9 + Q10 + Q11 + Q12
        外向性 =~ Q2 + Q6 + Q8
        神經質 =~ Q13 + Q14 + Q15 + Q16
        親和性 =~ Q5 + Q7
        #covariances
        親和性 ~~ 神經質 + 外向性 + 開放性 + 謹慎性
        神經質 ~~ 外向性 + 開放性 + 謹慎性
        外向性 ~~ 開放性 + 謹慎性
        開放性 ~~ 謹慎性


'

fit1 <- sem(model1, data = g1)
summary(fit1, standardized = T)
fitmeasures(fit1, c("cfi", "rmsea", "srmr"))
semPaths(fit1, what = "stand", layout = 'circle2')

fit2 <- sem(model2, data = g2)
summary(fit2, standardized = T)
fitmeasures(fit2, c("cfi", "rmsea", "srmr"))
semPaths(fit2, what = "stand", layout = 'circle')

resid(fit1, type="standardized")
resid(fit2, type="standardized")
