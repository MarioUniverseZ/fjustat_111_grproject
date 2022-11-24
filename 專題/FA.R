library(psych)
library(ppcor)
library(parallel)
library(nFactors)
library(GPArotation)
library(readxl)
library(dplyr)

pj <- read.csv("D:\\project.csv")
pj <- pj[,-1]
correlation0 <- cov(pj, use = "pairwise.complete.obs")
# 
fa.parallel(correlation0, fa = "fa", main = "¸H¥Û¹Ï")
# 
fa <- fa(correlation0, nfactors = 4, rotate = "varimax", fm = "pa")
fa0 <- fa(correlation0, nfactors = 4, rotate = "quartimax", fm = "pa")
fa

fa.diagram(fa, digits = 3)
