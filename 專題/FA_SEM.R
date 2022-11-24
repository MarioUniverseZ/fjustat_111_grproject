library(psych)
library(ppcor)
library(parallel)
library(nFactors)
library(GPArotation)
library(readxl)
library(dplyr)

pj <- read.csv("D:\\data2.csv")
pj <- pj[3:22]
pj$嚜熹1 = NULL

#因素分析
correlation0 <- cov(pj)
# 
fa.parallel(correlation0, n.obs = nrow(pj), fa = "fa", main = "碎石圖", use = "pairwise.complete.obs")
# 
fa <- fa(correlation0, nfactors = 5, rotate = "varimax", fm = "pa")
fa

fa.diagram(fa, digits = 3)

#SEM
library(lavaan)
library(semPlot)

# m1 <- lm(q17 + q18 + q19 + q20 ~ ., data = pj)

model <- '
        #measurement model
        PA1 =~ q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12
        PA2 =~ q13 + q14 + q15 + q16
        PA3 =~ q1 + q2 + q3 + q4
        PA4 =~ q17 + q18 + q19 + q20
'

model2 <- '
        #measurement model
        PA1 =~ 1*q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12
        PA2 =~ 1*q13 + q14 + q15 + q16
        PA3 =~ 1*q1 + q2 + q3 + q4
        PA4 =~ 1*q17 + q18 + q19 + q20
        #covariances
        PA4 ~~ PA1 + PA2 + PA3
        PA1 ~~ PA3
        q17 ~~ q1 + q18 + q19
        q18 ~~ q2 + q3 + q4 + q19
        q19 ~~ q2 + q3 + q20
        q20 ~~ q5 + q12
        q5 ~~ q6 + q7 + q17
        q6 ~~ q2 + q13 + q14
        q7 ~~ q3  + q9 + q11
        q8 ~~ q4 + q5 + q6
        q9 ~~ q1 + q10 + q11 + q12 + q20
        q10 ~~ q11 + q18
        q11 ~~ q12 + q19
        q12 ~~ q4 + q13
        q13 ~~ q1 + q14
        q15 ~~ q3
        q16 ~~ q1 + q13
        q3 ~~ q1 + q4 + q5
        
        '

model3 <- '
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
        Q9 ~~ Q2 + Q3
        Q2 ~~ Q15 + Q16
        Q13 ~~ Q14
        Q5 ~~ Q6
        '

'model3 <- 
#measurement model
PA1 =~ Q5 + Q6 + Q7 + Q8
PA2 =~ Q13 + Q14 + Q15 + Q16
PA3 =~ Q1 + Q2 + Q3 + Q4
PA4 =~ Q17 + Q18 + Q19 + Q20
PA5 =~ Q9 + Q10 + Q11 + Q12
#covariances
PA1 ~~ PA2 + PA3 + PA4 + PA5
PA2 ~~ PA3 + PA4 + PA5
PA3 ~~ PA4 + PA5
PA4 ~~ PA5
Q5 ~~ Q6 + Q7 + Q8 + Q13 + Q14 + Q17
Q6 ~~ Q17
Q7 ~~ Q2 + Q8 + Q9 + Q11
Q8 ~~ Q9 + Q10 + Q11
Q13 ~~ Q11 + Q14
Q14 ~~ Q2 + Q11
Q15 ~~ Q2 + Q9 + Q11 + Q12 + Q19 + Q20
Q16 ~~ Q2 + Q3 + Q9 + Q11 + Q12 + Q19 + Q20
Q1 ~~ Q2 + Q3 + Q9
Q2 ~~ Q9 + Q11 + Q12 + Q20
Q3 ~~ Q9
Q4 ~~ Q9
Q19 ~~ Q9 + Q20
Q20 ~~ Q9 + Q11 + Q12
Q9 ~~ Q11
Q11 ~~ Q12'

resid(fit5, type = 'standardized')
fit5 <- sem(model3, data = pj)
summary(fit5, standardized = T)
fitmeasures(fit5, c("cfi", "rmsea", "srmr"))
semPaths(fit5, what = "stand", layout = 'circle2')

fit <- cfa(model, data = pj)
summary(fit, fit.measure = T)

fit2 <- sem(model, data = pj)
summary(fit2, standardized = T, fit.measures = T)
fitmeasures(fit2, c("cfi", "rmsea", "srmr"))

fit3 <- lavaan(model, data = pj, auto.var = T)
summary(fit3, fit.measure = T)

semPaths(fit2,what = 'est', layout = 'spring') #展示估計值

fit4 <- sem(model2, data = pj)
summary(fit4, standardized = T)

parameterEstimates(fit4)
fitted(fit4)
resid(fit4, type = 'standardized')
fitmeasures(fit4, c("cfi", "rmsea", "srmr"))

semPaths(fit4, what = 'est', layout = 'circle')


