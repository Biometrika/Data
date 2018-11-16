
# Analiza eksperimentalnih podatka sa ponovljenim merenjima ---
rm(list=ls())

# izvor podataka:
# Davidian M., Giltinan D.M. (1995). Nonlinear Models for Repeated
# Measurement Data (Chapman & Hall/CRC Monographs on Statistics &
# Applied Probability)

library(nlme)

soya <- read.table("d:/Biometrika/soya.txt", header = TRUE)
df <- data.frame(soya[soya$year == "1988",])
head(df,3)

# promenljiva "weight" pozitivno asimetricna...

df$plot <- as.factor(df$plot)
df$gen <- as.factor(df$gen)
df$time <- as.factor(df$time)

# ID struktura ---
m1 <- gls(weight ~ gen + time + gen:time,
          correlation = varIdent(form = ~ 1),
          weights = varIdent(form = ~ 1),
          data = df)
anova(m1)
plot(m1)

# CS struktura ---
m2 <- gls(weight ~ gen + time + gen:time,
          correlation = corCompSymm(form = ~ time|plot),
          weights = varIdent(form = ~ 1),
          data = df)
anova(m2)
plot(m2)

# CSH struktura ---
m3 <- gls(weight ~ gen + time + gen:time,
          correlation = corCompSymm(form = ~ time|plot),
          weights = varIdent(form = ~ 1|time),
          data = df)
anova(m3)
plot(m3)

# DIAG struktura ---
m4 <- gls(weight ~ gen + time + gen:time,
          correlation = varIdent(form = ~ 1),
          weights = varIdent(form = ~ 1|time),
          data = df)
anova(m4)
plot(m4)

# AR(1) struktura ---
m5 <- gls(weight ~ gen + time + gen:time,
          correlation = corAR1(form = ~ 1|plot),
          weights = varIdent(form = ~ 1),
          data = df)
anova(m5)
plot(m5)

# ARH(1) struktura ---
m6 <- gls(weight ~ gen + time + gen:time,
          correlation = corAR1(form = ~ 1|plot),
          weights = varIdent(form = ~ 1|time),
          data = df)
anova(m6)
plot(m6)

# US struktura ---
m7 <- gls(weight ~ gen + time + gen:time,
          correlation = corSymm(form = ~ 1|plot),
          weights = varIdent(form = ~ 1|time),
          data = df)
anova(m7)
plot(m7)

# AIC ---
AIC(m1) # ID struktura
AIC(m2) # CS struktura
AIC(m3) # CSH struktura
AIC(m4) # DIAG struktura
AIC(m5) # AR(1) struktura
AIC(m6) # ARH(1) struktura
AIC(m7) # US struktura

