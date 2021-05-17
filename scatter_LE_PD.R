# This code includes the programming of the description
# of the underlying data

# For England & Wales, City of London and Isle of Scilly will be 
# excluded from the analysis ("E09000001", "E06000053")

datENW <- 
  datENW %>% 
  filter(!Geo %in% c("E09000001", "E06000053"))

# Function lifetable for calculation of e60

lifetable <- function(mx){
  n <- rep(5, length(mx))
  ax <- rep(2.5, length(mx))
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[length(qx)] <- 1
  px <- 1-qx
  lx <- c(100000, cumprod(px)*100000)[-(length(mx)+1)]
  dx <- c(-diff(lx), lx[length(lx)])
  Lx1 <- n[-length(n)]*lx[-1]+ax[-length(ax)]*dx[-length(dx)]
  Lx2 <- dx[length(dx)]/mx[length(mx)]
  Lx <- c(Lx1, Lx2)
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  out <- ex[1]
  # out <- data.frame(lx=lx, Lx=Lx, Tx=Tx, ex=ex)
  return(out)
}



e60_PD_ENW <- 
  datENW %>% 
  group_by(Age, Sex, Geo, PD, PDcat) %>% 
  summarise(Nx = sum(Pop), Dx = sum(Death)) %>%
  group_by(Sex, Geo, PD, PDcat) %>% 
  mutate(mx = Dx/Nx) %>% 
  mutate(e60 = lifetable(mx)) %>%
  distinct(Geo, Sex, PD, e60)

e60_PD_GER <- 
  datGER %>% 
  group_by(Age, Sex, Geo, PD, PDcat) %>% 
  summarise(Nx = sum(Pop), Dx = sum(Death)) %>%
  group_by(Sex, Geo, PD, PDcat) %>% 
  mutate(mx = Dx/Nx) %>% 
  mutate(e60 = lifetable(mx)) %>%
  distinct(Geo, Sex, PD, e60)

# Scatterplots

dev.off()
pdf("scatterplot_LE_PD.pdf", family = "Times", width = 14, height = 14)
profile <- layout(rbind(matrix(1:4, ncol = 2, byrow = FALSE), rep(5,2)),
                  heights = c(0.45, 0.45,0.1))
par(xpd = TRUE, mar = c(3,5,3,2))

# Females Germany
# Extract data and calculate tercile of e60
plotDat <- 
  e60_PD_GER %>% 
  filter(Sex == 2)
tercile <- quantile(plotDat$e60, probs = c(0, 1/3, 2/3, 1))
plotDat$e60Cat <- cut(plotDat$e60, breaks = tercile, 
                      labels = 1:3, include.lowest = TRUE)

# Label pch and color
colLE <- c('#41b6c4','#2c7fb8','#253494')
plotDat$e60Col <- mapvalues(plotDat$e60Cat, from = 1:3, to = colLE)
plotDat$pch <- mapvalues(plotDat$PDcat, from = 1:3, to = c(15,1,16))

plot(x=1:7, y=1:7, bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs
     = "i", typ = "n", xlab = NA, ylab = NA, log = "x", xlim = c(20,
                                                                 17000),
     ylim = c(23, 30))

segments(x0=300, y0=23, x1=300, y1=30, col = "gray", lty = 2, lwd = 2)
segments(x0=1000, y0=23, x1=1000, y1=30, col = "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[2], x1=17000, y1=tercile[2], col =
           "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[3], x1=17000, y1=tercile[3], col =
           "gray", lty = 2, lwd = 2)
text(x=20, y=tercile[2], round(tercile[2],2), cex = 1.4, pos = 4)
text(x=20, y=tercile[3], round(tercile[3],2), cex = 1.4, pos = 4)

axis(1, at = c(20, 50, 100, 300, 1000, 2000, 4000, 8000, 17000),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 2)
axis(2, at = seq(23, 30, by = 1),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 0)
lines(x=plotDat$PD, y=plotDat$e60, pch = as.numeric(plotDat$pch), typ = "p", cex = 1.3, col =
        as.character(plotDat$e60Col))
mtext("Life expectancy at age 60", 2, line = 3, cex = 1.4)
mtext("Population density (log-scale)", 1, line = 3, cex = 1.4)
mtext("Germany", 3, line = 1, cex = 1.6)
mtext("Females", 3, line = -1, cex = 1.6, adj = 0.02)


################################################################################

# Males Germany
# Extract data and calculate tercile of e60
plotDat <- 
  e60_PD_GER %>% 
  filter(Sex == 1)
tercile <- quantile(plotDat$e60, probs = c(0, 1/3, 2/3, 1))
plotDat$e60Cat <- cut(plotDat$e60, breaks = tercile, 
                      labels = 1:3, include.lowest = TRUE)

# Label pch and color
colLE <- c('#41b6c4','#2c7fb8','#253494')
plotDat$e60Col <- mapvalues(plotDat$e60Cat, from = 1:3, to = colLE)
plotDat$pch <- mapvalues(plotDat$PDcat, from = 1:3, to = c(15,1,16))

plot(x=1:7, y=1:7, bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs
     = "i", typ = "n", xlab = NA, ylab = NA, log = "x", xlim = c(20,
                                                                 17000),
     ylim = c(19, 28))

segments(x0=300, y0=19, x1=300, y1=28, col = "gray", lty = 2, lwd = 2)
segments(x0=1000, y0=19, x1=1000, y1=28, col = "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[2], x1=17000, y1=tercile[2], col =
           "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[3], x1=17000, y1=tercile[3], col =
           "gray", lty = 2, lwd = 2)
text(x=20, y=tercile[2], round(tercile[2],2), cex = 1.4, pos = 4)
text(x=20, y=tercile[3], round(tercile[3],2), cex = 1.4, pos = 4)

axis(1, at = c(20, 50, 100, 300, 1000, 2000, 4000, 8000, 17000),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 2)
axis(2, at = seq(19, 28, by = 1),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 0)
lines(x=plotDat$PD, y=plotDat$e60, pch = as.numeric(plotDat$pch), typ = "p", cex = 1.3, col =
        as.character(plotDat$e60Col))
mtext("Life expectancy at age 60", 2, line = 3, cex = 1.4)
mtext("Population density (log-scale)", 1, line = 3, cex = 1.4)
mtext("Males", 3, line = -1, cex = 1.6, adj = 0.02)

# Females ENW
# Extract data and calculate tercile of e60
plotDat <- 
  e60_PD_ENW %>% 
  filter(Sex == 2)
tercile <- quantile(plotDat$e60, probs = c(0, 1/3, 2/3, 1))
plotDat$e60Cat <- cut(plotDat$e60, breaks = tercile, 
                      labels = 1:3, include.lowest = TRUE)

# Label pch and color
colLE <- c('#41b6c4','#2c7fb8','#253494')
plotDat$e60Col <- mapvalues(plotDat$e60Cat, from = 1:3, to = colLE)
plotDat$pch <- mapvalues(plotDat$PDcat, from = 1:3, to = c(15,1,16))

plot(x=1:7, y=1:7, bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs
     = "i", typ = "n", xlab = NA, ylab = NA, log = "x", xlim = c(20,
                                                                 17000),
     ylim = c(23, 30))

segments(x0=300, y0=23, x1=300, y1=30, col = "gray", lty = 2, lwd = 2)
segments(x0=1000, y0=23, x1=1000, y1=30, col = "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[2], x1=17000, y1=tercile[2], col =
           "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[3], x1=17000, y1=tercile[3], col =
           "gray", lty = 2, lwd = 2)
text(x=20, y=tercile[2], round(tercile[2],2), cex = 1.4, pos = 4)
text(x=20, y=tercile[3], round(tercile[3],2), cex = 1.4, pos = 4)

axis(1, at = c(20, 50, 100, 300, 1000, 2000, 4000, 8000, 17000),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 2)
axis(2, at = seq(23, 30, by = 1),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 0)
lines(x=plotDat$PD, y=plotDat$e60, pch = as.numeric(plotDat$pch), typ = "p", cex = 1.3, col =
        as.character(plotDat$e60Col))
mtext("Life expectancy at age 60", 2, line = 3, cex = 1.4)
mtext("Population density (log-scale)", 1, line = 3, cex = 1.4)
mtext("England & Wales", 3, line = 1, cex = 1.6)

# Males ENW
# Extract data and calculate tercile of e60
plotDat <- 
  e60_PD_ENW %>% 
  filter(Sex == 1)
tercile <- quantile(plotDat$e60, probs = c(0, 1/3, 2/3, 1))
plotDat$e60Cat <- cut(plotDat$e60, breaks = tercile, 
                      labels = 1:3, include.lowest = TRUE)

# Label pch and color
colLE <- c('#41b6c4','#2c7fb8','#253494')
plotDat$e60Col <- mapvalues(plotDat$e60Cat, from = 1:3, to = colLE)
plotDat$pch <- mapvalues(plotDat$PDcat, from = 1:3, to = c(15,1,16))

plot(x=1:7, y=1:7, bty = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs
     = "i", typ = "n", xlab = NA, ylab = NA, log = "x", xlim = c(20,
                                                                 17000),
     ylim = c(19, 28))

segments(x0=300, y0=19, x1=300, y1=28, col = "gray", lty = 2, lwd = 2)
segments(x0=1000, y0=19, x1=1000, y1=28, col = "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[2], x1=17000, y1=tercile[2], col =
           "gray", lty = 2, lwd = 2)

segments(x0=35, y0=tercile[3], x1=17000, y1=tercile[3], col =
           "gray", lty = 2, lwd = 2)
text(x=20, y=tercile[2], round(tercile[2],2), cex = 1.4, pos = 4)
text(x=20, y=tercile[3], round(tercile[3],2), cex = 1.4, pos = 4)

axis(1, at = c(20, 50, 100, 300, 1000, 2000, 4000, 8000, 17000),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 2)
axis(2, at = seq(19, 28, by = 1),
     labels = TRUE, lwd = 3, cex.axis = 1.5)
par(las = 0)
lines(x=plotDat$PD, y=plotDat$e60, pch = as.numeric(plotDat$pch), typ = "p", cex = 1.3, col =
        as.character(plotDat$e60Col))
mtext("Life expectancy at age 60", 2, line = 3, cex = 1.4)
mtext("Population density (log-scale)", 1, line = 3, cex = 1.4)

# Legend (Plot 5)
## Legend
par(xpd = TRUE, mar = c(1,1,1,1))
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
       c(1,6))

legend(x=2, y=5.5, legend =c("Predominantly rural"), col =
         "black", bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=3, y=5.5, legend =c("Intermediate"), col =
         "black", bty = "n", cex = 2, pch = 1, horiz = TRUE)

legend(x=4, y=5.5, legend =c("Predominantly urban"), col =
         "black", bty = "n", cex = 2, pch = 16, horiz = TRUE)


legend(x=2, y=3.5, legend =c("Lower tertile of LE"), col =
         c(colLE[1]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=3, y=3.5, legend =c("Middle tertile of LE"), col =
         c(colLE[2]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=4, y=3.5, legend =c("Upper tertile of LE"), col =
         c(colLE[3]), bty = "n", cex = 2, pch = 15, horiz = TRUE)
dev.off()

