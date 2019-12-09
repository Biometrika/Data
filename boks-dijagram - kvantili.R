
library(qboxplot)

par(mfrow = c(3,3))
for (i in c(1:9)){
x <- runif(100)

qboxplot(data.frame(x), probs = c(0.25,0.5,0.75), qtype = i, range = 1.5)
}

quantAll <- function(x, prob, ...)
  t(vapply(1:9, function(typ) quantile(x, prob = prob, type = typ, ...), quantile(x, prob, type = 1)))
p <- c( 25, 50, 75) / 100
quantAll(x, p)