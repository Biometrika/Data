
library(nlme)

soja <- read.table("d:/Biometrika/soja.txt", header = TRUE)
df <- data.frame(soja[soja$year == "1988",])

df$plot <- as.factor(df$plot)
df$gen <- as.factor(df$gen)
df$time <- as.factor(df$time)

df$logweight <- log(df$weight)

# ID error structure ---
rstruct <- varIdent(form = ~ 1)
rheter <- varIdent(form = ~ 1)
m1 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m1)
anova(m1)
plot(m1)

# CS error structure ---
rstruct <- corCompSymm(form = ~ time|plot)
rheter <- varIdent(form = ~ 1)
m2 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m2)
anova(m2)
plot(m2)

# CSH error structure ---
rstruct <- corCompSymm(form = ~ time|plot)
rheter <- varIdent(form = ~ 1|time)
m3 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m3)
anova(m3)
plot(m3)

# DIAG error structure ---
rstruct <- varIdent(form = ~ 1)
rheter <- varIdent(form = ~ 1|time)
m4 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m4)
anova(m4)
plot(m4)

# AR(1) error structure ---
rstruct <- corAR1(form = ~ 1|plot)
rheter <- varIdent(form = ~ 1)
m5 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m5)
anova(m5)
plot(m5)

# ARH(1) error structure ---
rstruct <- corAR1(form = ~ 1|plot)
rheter <- varIdent(form = ~ 1|time)
m6 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m6)
anova(m6)
plot(m6)

# US error structure ---
rstruct <- corSymm(form = ~ 1|plot)
rheter <- varIdent(form = ~ 1|time)
m7 <- gls(logweight ~ gen + time + gen:time,
          correlation = rstruct,
          weights = rheter,
          data = df)
summary(m7)
anova(m7)
plot(m7)

# AIC ---
AIC(m1) # ID error structure
AIC(m2) # CS error structure
AIC(m3) # CSH error structure
AIC(m4) # DIAG error structure
AIC(m5) # AR(1) error structure
AIC(m6) # ARH(1) error structure
AIC(m7) # US error structure
