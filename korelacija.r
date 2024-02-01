# korelacija

library(pdfCluster)
library(Hmisc)
library(corrplot)
library(qgraph)
library(dplyr)
library(magrittr)
library(tibble)
library(psych)
library(DescTools)
library(ellipse)
library(PerformanceAnalytics)

data(oliveoil)
as_tibble(oliveoil)

# Pearson korelacija ---
round(rcorr(as.matrix(oliveoil[,3:10]))$r,3)
round(rcorr(as.matrix(oliveoil[,3:10]))$P,3)

# grafikon 1 ---
oliveoil %>%
  select(3:10) %>%
  cor(method = "pearson") %>%
  corrplot(method = "number",
           # c("circle","square","ellipse","number","shade","color","pie")
           order = "alphabet", # c("AOE","FPC","hclust","alphabet")
           type = "lower",
           # c("full","upper","lower")
           col = colorRampPalette(c("#4DAF4A","white","#E41A1C"))(100),
           addgrid.col = "gray85",
           outline = TRUE,
           tl.cex = 0.85,
           tl.col = "black",
           tl.srt = 90,
           mar = c(0,0,1,1))

# grafikon 2 ---
chart.Correlation(oliveoil[,3:10],
                  method = "pearson")

# grafikon 3 ---
oliveoil %>%
  select(3:10) %>%
  cor(method = "pearson") %>%
  heatmap(symm = TRUE, col = colorRampPalette(c("#4DAF4A","white","#E41A1C"))(100))

# grafikon 4 ---
oliveoil %>%
  select(3:10) %>%
  cor(method = "pearson") %>%
  cor.plot(numbers = TRUE, colors = TRUE)

# grafikon 5 ---
oliveoil %>%
  select(3:10) %>%
  cor(method = "pearson") %>%
  PlotWeb(col = c("#4DAF4A","#E41A1C"))

# grafikon 6 ---
oliveoil %>%
  select(3:10) %>%
  cor(method = "pearson") %>%
  plotcorr(type = "lower", diag = FALSE, col = "#E41A1C")

# grafikon 7 ---
qgraph(cor(oliveoil[3:10], method = "pearson"),
       graph = "cor",
       layout = "spring",
       shape = "circle",
       # shape = c("square", "triangle", "diamond", "heart", "circle")
       posCol = "#E41A1C",
       negCol = "#4DAF4A",
       edge.labels = TRUE,
       edge.label.cex = 0.9,
       label.cex = 1.0,
       vsize = 12,
       vTrans = 150,
       labels = colnames(oliveoil[3:10]),
       label.scale = TRUE,
       label.cex = 1.5,
       details = TRUE,
       minimum = "sig",
       alpha = 0.05,
       bonf = TRUE,
       sampleSize = 572,
       esize = 15)