set.seed(12345)
rep <- 1000
tabela <- matrix(NA, nrow = rep, ncol = 2)

intercept <- 3.0
b1 <- 0.5

n <- 1000
X <- runif(n, -1, 1)

for(i in 1:rep){
Y <- intercept + b1*X + rnorm(n, 0, 1)
m <- lm(Y ~ X)
tabela[i, 1] <- m$coef[1]
tabela[i, 2] <- m$coef[2]
}

# raspodela intercepta
par(mfrow = c(1, 3))
hist(tabela[,1], col = "gold", main = "")
box()
rug(tabela[,1], col = "tomato")
qqnorm(tabela[,1], col = "gold", main = "")
qqline(tabela[,1], col = "tomato")
plot(ecdf(tabela[,1]), col = "tomato", main = "")


# raspodela b1 koeficijenta
par(mfrow = c(1, 3))
hist(tabela[,2], col = "gold", main = "")
box()
rug(tabela[,2], col = "tomato")
qqnorm(tabela[,2], col = "gold", main = "")
qqline(tabela[,2], col = "tomato")
plot(ecdf(tabela[,2]), col = "tomato", main = "")