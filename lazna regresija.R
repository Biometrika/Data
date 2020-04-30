# lazna regresija ---

set.seed(12345)

# generisanje podataka
df <- matrix(nrow = 30, ncol = 1001, rnorm(30*1001))
korelacija <- numeric(1000)

# izracunavanje korelacije izmedju zavisne i nezavisnih promenljivih
for(i in 1:1000) korelacija[i] <- cor(df[,1], df[,1+i])

# graficko predstavljanje korelacije
par(mfrow=c(1,2))
hist(korelacija)
rug(korelacija, col = "tomato")
qqnorm(korelacija)
qqline(korelacija, col = "tomato")
range(korelacija)

# indeks promenljivih koje su korelisane sa zavisnom promenljivom
indeks <- which(abs(korelacija)> 0.5)

# model
m1 <- lm(df[,1] ~ df[,1 + indeks])
summary(m1)