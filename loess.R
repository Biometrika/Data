# LOESS

loess <- loess(Y ~ X)
summary(loess)
hat <- predict(loess)
plot(Y ~ X, xlab = expression(X[3]), ylab = "Y", mgp = c(2.4,0.7,0), cex = 1.0, cex.axis = 0.9)
lines(X[order(X)], hat[order(hat)], col = "red", lwd = 2)
r2_loess <- cor(Y, hat)
r2_loess

r2 <- bquote(italic(r)^2 == .(format(r2_loess, digits = 3)))
text(x = 19.2, y = 5.9, labels = r2)