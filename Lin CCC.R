# Lin koeficijent konkordancije ---
set.seed(seed = 12345)
x <- rnorm(n = 100, mean = 0, sd = 1)
y <- x + runif(n = 100, min = 0, max = 1)
plot(x, y)

lin.ccc <- function(x, y) {
    cov.xy <- cov(x, y)
    var.x <- var(x)
    var.y <- var(y)
    mean.x <- mean(x)
    mean.y <- mean(y)
    2*cov.xy / (var.x + var.y + (mean.x - mean.y)^2)
    }

lin.ccc(x, y)
cor.test(x, y, method = "spearman")