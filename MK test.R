# Modifikovani Mann-Kendall test za podatke sa serijalnom korelacijom ---

library(modifiedmk)
library(forecast)

setwd("d:/")
dt <- read.csv("tabela.csv", head = TRUE)
head(dt, 3)

# graficki prikaz ---
dtts <- ts(dt)
dtts[,3:8]
plot.ts(dtts[,3:8])

# definisanje sezonske serije ---
dttseas <- ts(dtts[,3:8], frequency = 6, start = c(1979,1))
plot.ts(dttseas[,1:6])

# dekompozicija serija - multiplikativni model ---
decompose(dttseas[,1:6])

# eliminisanje sezonske komponente npr. lokalitet 6 ---
components6 <- decompose(dttseas[,6], type = "mult")
ts.sa6 <- seasadj(components6)
plot.ts(ts.sa6)

# pretvaranje serije u vektor i primena korigovanog MK testa ---
tsv6 <- as.vector(ts.sa6)
mmkh(tsv6, ci = 0.95)

# lokalitet 1 ---
components1 <- decompose(dttseas[,1], type = "mult")
ts.sa1 <- seasadj(components1)
plot.ts(ts.sa1)

# pretvaranje serije u vektor i primena korigovanog MK testa ---
tsv1 <- as.vector(ts.sa1)
mmkh(tsv1, ci = 0.95)