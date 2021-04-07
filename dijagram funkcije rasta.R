# dijagram funkcije rasta ----

library(ggplot2)

funkcija1 <- function(xvar){1.27940 + (57.155 - 1.27940) / (1 + exp(-0.009043*(xvar - 493.70)))
}
funkcija2 <- function(xvar){1.05720 + (47.580 - 1.05720) / (1 + exp(-0.008860*(xvar - 470.20)))
}
funkcija3 <- function(xvar){3.80240 + (52.533 - 3.80240) / (1 + exp(-0.009188*(xvar - 505.60)))
}
funkcija4 <- function(xvar){4.14140 + (42.650 - 4.14140) / (1 + exp(-0.011743*(xvar - 465.91)))
}
funkcija5 <- function(xvar){2.16680 + (50.722 - 2.16680) / (1 + exp(-0.009698*(xvar - 479.75)))
}
funkcija6 <- function(xvar){0.94721 + (42.199 - 0.94721) / (1 + exp(-0.009023*(xvar - 436.51)))
}
funkcija7 <- function(xvar){3.21080 + (45.794 - 3.21080) / (1 + exp(-0.009270*(xvar - 476.89)))
}
funkcija8 <- function(xvar){4.29940 + (36.136 - 4.29940) / (1 + exp(-0.011934*(xvar - 456.15)))
}

ggplot(NULL,
       aes(x = x,
       colour = Genotip)) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(1)), fun = funkcija1, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(2)), fun = funkcija2, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(3)), fun = funkcija3, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(4)), fun = funkcija4, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(5)), fun = funkcija5, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(6)), fun = funkcija6, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(7)), fun = funkcija7, lwd = 1.5) +
  stat_function(data = data.frame(x = 100:1000, Genotip = factor(8)), fun = funkcija8, lwd = 1.5) +
  scale_colour_manual(values = c("#800026","#BD0026","#E31A1C","#FC4E2A","#FD8D3C","#FEB24C","#FED976","#FFEDA0"),
                      labels = c("G1","G2","G3","G4","G5","G6","G7","G8")) +
  scale_x_continuous(breaks = seq(from = 0, to = 1000, by = 100)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  labs(x = "GDD",
       y = "Masa semena (g)") +
 theme(text = element_text(size = 11),
       axis.text.x = element_text(size = 12, hjust = 0.5),
       axis.title.x = element_text(size = 15, face = "bold"),
       axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5),
       axis.title.y = element_text(size = 15, face = "bold"))