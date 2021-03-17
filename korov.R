# analiza brojivih podataka ---
rm(list=ls(all=TRUE))

library(dplyr)
library(ggplot2)
library(patchwork)
library(MASS)
library(car)
library(pscl)
library(multcompView)
library(AER)
library(emmeans)
library(hnp)

trt <- c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E","F","F","F","F")
blk <- c("1","2","3","4","1","2","3","4","1","2","3","4","1","2","3","4","1","2","3","4","1","2","3","4")
korov <- c(538,422,377,315,438,442,319,380,77,61,157,52,115,57,100,45,17,31,87,16,18,26,77,20)
dt <- data.frame(trt,blk,korov)
head(dt, 3)

dt$trt <- as.factor(dt$trt)
dt$blk <- as.factor(dt$blk)

# odnos srednje vrednost i varijanse ---
dt.var.mean <- dt %>%
 group_by(trt) %>%
 summarize(mean.korov = mean(korov),
           var.korov = var(korov)) %>%
 mutate(var.mean = var.korov / mean.korov)

ggplot(data = dt.var.mean,
       aes(x = var.korov,
           y = mean.korov)) +
  geom_point() +
  geom_label(aes(label = trt,
                 fill = trt),
             size = 5) +
  labs(x = "Varijansa",
       y = "Srednja vrednost",
       title = "Odnos srednje vrednosti i varijanse") +
  theme(legend.position = "none")

ggplot(data = dt.var.mean,
       aes(x = trt,
           y = var.mean)) +
  geom_bar(stat = "identity",
           fill = "gold") +
  geom_text(aes(label = round(var.mean)),
            vjust = 1.5,
            colour = "black",
            size = 4) +
  geom_hline(yintercept = 1.0,
             color = "black",
             size = 0.7,
             linetype = "dashed") +
  annotate(geom = "text", x = 1.2, y = 2.0,
           label = "Indeks disperzije = 1.0",
           color = "black", size = 4) +
  labs(x = "Tretman",
       y = "Varijansa / Srednja vrednost",
       title = "Odnos varijanse i srednje vrednosti",
       subtitle = "Indeks disperzije")


# modeli za brojive podatke ---

# Poasonov model ---
m1 <- glm(korov ~ trt, family = "poisson", data = dt)
m1
summary(m1)
Anova(m1, test = "LR")
m1$deviance / m1$df.residual
dispersiontest(m1)

exp(m1$coefficients[2])
exp(m1$coefficients[3])
exp(m1$coefficients[4])
exp(m1$coefficients[5])
exp(m1$coefficients[6])


# kvazi-Poasonov model ---
m2 <- glm(korov ~ trt, family = "quasipoisson", data = dt)
m2
summary(m2)
Anova(m2, test = "LR")
m2$deviance / m2$df.residual

exp(m2$coefficients[2])
exp(m2$coefficients[3])
exp(m2$coefficients[4])
exp(m2$coefficients[5])
exp(m2$coefficients[6])


# negativni binomni (NB) model ---
m3 <- glm.nb(korov ~ trt, control = glm.control(maxit=10000), data = dt)
m3
summary(m3)
Anova(m3, test = "LR")
m3$deviance / m3$df.residual
summary(m3)$theta
odTest(m3)

exp(m3$coefficients[2])
exp(m3$coefficients[3])
exp(m3$coefficients[4])
exp(m3$coefficients[5])
exp(m3$coefficients[6])


# uporedjenje i izbor modela ---

# dijagram polu-normalnih skorova ---
par(mfrow = c(1,3))
hnp(m1, pch = 16, cex = 1,
    main = "Poasonov model",
    ylab = "Reziduali odstupanja",
    xlab = "Polu-normalni skorovi",
    paint.out = TRUE, print = TRUE)
hnp(m2, pch = 16, cex = 1,
    main = "kvazi-Poasonov model",
    ylab = "Reziduali odstupanja",
    xlab = "Polu-normalni skorovi",
    paint.out = TRUE, print = TRUE)
hnp(m3, pch = 16, cex = 1,
    main = "NB model",
    ylab = "Reziduali odstupanja",
    xlab = "Polu-normalni skorovi",
    paint.out = TRUE, print = TRUE)


# AIC kriterijum ---
AIC <- c(AIC(m1), AIC(m2), AIC(m3))
model <- c("Poasonov model",
           "kvazi-poasonov model",
           "NB model")
AIC.tab <- data.frame(model = model, AIC = AIC)
AIC.tab


# visestruka uporedjenja ---

# Poasonov model ---

emm.m1 <- emmeans(m1, pairwise ~ trt, type = "response")
emm.m1 <- multcomp::cld(emm.m1$emmeans, Letters = letters)
emm.m1$.group <- gsub(" ", "", emm.m1$.group, fixed = TRUE)
emm.m1

# kvazi-Poasonov model ---
emm.m2 <- emmeans(m2, pairwise ~ trt, type = "response")
emm.m2 <- multcomp::cld(emm.m2$emmeans, Letters = letters)
emm.m2$.group <- gsub(" ", "", emm.m2$.group, fixed = TRUE)
emm.m2

# Negativni binomni model ---
emm.m3 <- emmeans(m3, pairwise ~ trt, type = "response")
emm.m3 <- multcomp::cld(emm.m3$emmeans, Letters = letters)
emm.m3$.group <- gsub(" ", "", emm.m3$.group, fixed = TRUE)
emm.m3


# dijagram predikcija ---

g1 <- ggplot() +
        geom_boxplot(data = dt, aes(x = trt, y = korov),
                     outlier.shape = NA, width = 0.4, fill = "gold") +
        geom_jitter(data = dt, aes(x = trt, y = korov),
                    width = 0.1, shape = 1, size = 2) +
        geom_point(data = emm.m1, aes(x = as.numeric(trt) + 0.4, y = rate),
                   col = "red", shape = 15, size = 2) +
        geom_errorbar(data = emm.m1, aes(x = as.numeric(trt) + 0.4,
                      ymin = asymp.LCL, ymax = asymp.UCL),
                      col = "red", width = 0.2, size = 1) +
        scale_y_continuous(breaks = seq(0, 500, 100)) +
        labs(x = "Tretman",
             y = "Broj korova po parceli",
             title = "Poasonov model") +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(hjust = 0.5, size = 10),
              axis.text.y = element_text(size = 10, hjust = 0.5))

g2 <- ggplot() +
        geom_boxplot(data = dt, aes(x = trt, y = korov),
                     outlier.shape = NA, width = 0.4, fill = "gold") +
        geom_jitter(data = dt, aes(x = trt, y = korov),
                    width = 0.1, shape = 1, size = 2) +
        geom_point(data = emm.m2, aes(x = as.numeric(trt) + 0.4, y = rate),
                   col = "red", shape = 15, size = 2) +
        geom_errorbar(data = emm.m2, aes(x = as.numeric(trt) + 0.4,
                      ymin = asymp.LCL, ymax = asymp.UCL),
                      col = "red", width = 0.2, size = 1) +
        scale_y_continuous(breaks = seq(0, 500, 100)) +
        labs(x = "Tretman",
             y = "Broj korova po parceli",
             title = "kvazi-Poasonov model") +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(hjust = 0.5, size = 10),
              axis.text.y = element_text(size = 10, hjust = 0.5))

g3 <- ggplot() +
        geom_boxplot(data = dt, aes(x = trt, y = korov),
                     outlier.shape = NA, width = 0.4, fill = "gold") +
        geom_jitter(data = dt, aes(x = trt, y = korov),
                    width = 0.1, shape = 1, size = 2) +
        geom_point(data = emm.m3, aes(x = as.numeric(trt) + 0.4, y = response),
                   col = "red", shape = 15, size = 2) +
        geom_errorbar(data = emm.m3, aes(x = as.numeric(trt) + 0.4,
                      ymin = asymp.LCL, ymax = asymp.UCL),
                      col = "red", width = 0.2, size = 1) +
        scale_y_continuous(breaks = seq(0, 500, 100)) +
        labs(x = "Tretman",
             y = "Broj korova po parceli",
             title = "NB model") +
        theme(plot.title = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(hjust = 0.5, size = 10),
              axis.text.y = element_text(size = 10, hjust = 0.5))

sl <- g1|g2|g3
sl + plot_annotation(tag_levels = "A")