# funkcije rasta ---

library(ggplot2)

# 2015 ---

# cetveroparametarska logisticka funkcija ---
# f(x) = c + (d - c) / (1 + exp(b*(x - e)))
fun1 <- function(xvar){0.000 + (0.311 - 0.000) / (1 + exp(-0.015*(xvar - 286.650)))
}

# cetveroparametarska log-logisticka funkcija ---
# f(x) = c + (d – c) / (1 + exp(b*(log(x) - log(e)))
fun2 <- function(xvar){0.002 + (0.329 - 0.002) / (1 + exp(-3.875*(log(xvar) - log(288.043))))
}

# cetveroparametarska Weibul funkcija ---
# f(x) = c + (d - c)* exp(-exp(b*(log(x) - log(e))))
fun3 <- function(xvar){0.003 + (0.361 - 0.003)* exp(-exp(-2.251*(log(xvar) - log(253.206))))
}

# cetveroparametarska Gompertz funkcija ---
# f(x) = c + (d - c)*(exp(-exp(b*(x - e))))
fun4 <- function(xvar){0.001 + (0.321 - 0.001)*(exp(-exp(-0.010*(xvar - 246.200))))
}

ggplot(NULL, aes(x = x, colour = Curves)) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(1)), fun = fun1, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(2)), fun = fun2, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(3)), fun = fun3, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(4)), fun = fun4, lwd = 1.2) +
  scale_colour_manual(values = c("#BD0026","#E31A1C","#FC4E2A","#FD8D3C"),
                      labels = c("Logisticka funkcija","Log-logisticka funkcija","Weibul funkcija","Gompertz funkcija")) +
  scale_x_continuous(breaks = seq(from = 0, to = 1000, by = 100)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.05)) +
  labs(x = "GDD",
       y = "Masa ulja (g)") +
  theme(legend.position = c(0.84,0.14))


# 2016 ---

# cetveroparametarska logisticka funkcija ---
# f(x) = c + (d - c) / (1 + exp(b*(x - e)))
fun5 <- function(xvar){-0.018 + (0.467 + 0.018) / (1 + exp(-0.017*(xvar - 220.530)))
}

# cetveroparametarska log-logisticka funkcija ---
# f(x) = c + (d – c) / (1 + exp(b*(log(x) - log(e)))
fun6 <- function(xvar){0.001 + (0.490 - 0.001)/(1 + exp(-3.638*(log(xvar) - log(225.265))))
}

# cetveroparametarska Weibul funkcija ---
# f(x) = c + (d - c)* exp(-exp(b*(log(x) - log(e))))
fun7 <- function(xvar){0.008 + (0.523 - 0.008)* exp(-exp(-2.285*(log(xvar) - log(197.374))))
}

# cetveroparametarska Gompertz funkcija ---
# f(x) = c + (d - c)*(exp(-exp(b*(x - e))))
fun8 <- function(xvar){0.002 + (0.476 - 0.002)*(exp(-exp(-0.012*(xvar - 192.280))))
}

ggplot(NULL, aes(x = x, colour = Curves)) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(1)), fun = fun5, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(2)), fun = fun6, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(3)), fun = fun7, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(4)), fun = fun8, lwd = 1.2) +
  scale_colour_manual(values = c("#006D2C","#238B45","#41AB5D","#74C476"),
                      labels = c("Logisticka funkcija", "Log-logisticka funkcija", "Weibul funkcija","Gompertz funkcija")) +
  scale_x_continuous(breaks = seq(from = 0, to = 1000, by = 100)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.05)) +
  labs(x = "GDD",
       y = "Masa ulja (g)") +
  theme(legend.position = c(0.84,0.14))


# 2015 & 2016 ---

ggplot(NULL, aes(x = x, colour = Curves)) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(1)), fun = fun1, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(2)), fun = fun2, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(3)), fun = fun3, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(4)), fun = fun4, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(5)), fun = fun5, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(6)), fun = fun6, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(7)), fun = fun7, lwd = 1.2) +
  stat_function(data = data.frame(x = 0:1000, Curves = factor(8)), fun = fun8, lwd = 1.2) +
  scale_colour_manual(values = c("#BD0026","#E31A1C","#FC4E2A","#FD8D3C","#006D2C","#238B45","#41AB5D","#74C476"),
  labels = c("Logisticka funkcija (2015)","Log-logisticka funkcija (2015)","Weibul funkcija (2015)","Gompertz funkcija (2015)",
             "Logisticka funkcija (2016)","Log-logisticka funkcija (2016)","Weibul funkcija (2016)","Gompertz funkcija (2016)")) +
  scale_x_continuous(breaks = seq(from = 0, to = 1000, by = 100)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.05)) +
  labs(x = "GDD",
       y = "Masa ulja (g)") +
  theme(legend.position = c(0.80,0.21))