library(lavaan)
library(semPlot)
a <- read.csv("file:///D:/test.csv",header=T)

model3 <- "
r=~R2+R4+R8
i=~I2+I4+I8
a=~A2+A4+A8
s=~S2+S4+S8
e=~E2+E4+E8
c=~C2+C4+C8
r~~i+a+s+e+c
i~~a+s+e+c
a~~s+e+c
s~~e+c
e~~c
R2~~I2+A2+S2+E2+C2
R4~~I4+A4+S4+E4+C4
R8~~I8+S8+E8+C8
I2~~A2+S2+E2+C2
I4~~A4+S4+E4+C4
I8~~A8+S8+E8+C8
A2~~S2+E2+C2
A4~~S4+E4+C4
A8~~S8+E8+C8
S2~~E2+C2
S4~~E4+C4
S8~~E8+C8
E2~~C2
E4~~C4
E8~~C8
"

fit5 <- cfa(model3,data=a)
parameterEstimates(fit5)
fitted(fit5)
resid(fit5, type="standardized")
fitMeasures(fit5)
fitMeasures(fit5, c("cfi","rmsea","srmr"))

semfit <- sem(model=model3, data=a,
              control=list(iter.max=1000),
              estimator = "ML",
              orthogonal=FALSE,
              verbose = FALSE)

summary(semfit, fit.measures=TRUE)
semPaths(semfit, "std", intercepts = FALSE)

  
#         

