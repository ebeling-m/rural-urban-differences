# This code calculates and plots the unadjusted rural-urban mortality differences

# Summarize years
# Reduce to variables necessary and bind GER and ENW

ENW <- 
  datENW %>% 
  group_by(Age, Sex, Geo, PDcat) %>% 
  summarize(Nx = sum(Pop), Dx = sum(Death)) %>% 
  mutate(Country = "ENW")

GER <- 
  datGER %>% 
  group_by(Age, Sex, Geo, PDcat) %>% 
  summarize(Nx = sum(Pop), Dx = sum(Death)) %>% 
  mutate(Country = "GER")

GER$Geo <- as.character(GER$Geo)
ENW_GER <- bind_rows(ENW, GER)


# Function to calculate rural-urban differences 
# with statistical inference 

unadjusted_diff <- function(Dx, Nx, PDcat){
  # Extract rural, bootstrap 
  # and calculate differences
  ruDx <- Dx[PDcat == 1]
  ruNx <- Nx[PDcat == 1]
  meanRu <- unlist(Map(1:10000, f = function(x) mean(rpois(n = length(ruDx), lambda = ruDx)/ruNx)))
  
  # Extract urban, bootstrap 
  # and calculate differences
  urDx <- Dx[PDcat == 3]
  urNx <- Nx[PDcat == 3]
  meanUr <- unlist(Map(1:10000, f = function(x) mean(rpois(n = length(urDx), lambda = urDx)/urNx)))
  
  relDif <- (meanRu/meanUr)-1
  distDiff <- quantile(relDif, probs = c(0.025, 0.5, 0.975))
  # out <- data.frame(med = distDiff[2])
  out <- data.frame(low=distDiff[1], med = distDiff[2], up = distDiff[3])
  return(out)
  
}

mort_diff <- 
  ENW_GER %>% 
  group_by(Age, Sex, Country) %>%
  do(unadjusted_diff(Dx=.$Dx, Nx =.$Nx, PDcat = .$PDcat))


##################################################
##################################################
# Plot for total unadjusted differences
##################################################
##################################################

dev.off()
pdf("totals_unadjusted.pdf", family = "Times", width = 14)
profile <- layout(rbind(matrix(1:2, ncol = 2, byrow = FALSE), rep(3,2)),
                  heights = c(0.9,0.1))
par(xpd = TRUE, mar = c(3,5,3,1))

## ## Germany
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
       c(1,16), log = "x")

segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
         col = "gray", lwd = 1, lty = 1)

segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=0, y0=1, x1=0, y1=8,
         col = "gray", lwd = 2, lty = 1)

segments(x0=0.75, y0=seq(1, 8, by = 1)+8, x1=1.25, y1=seq(1, 8, by = 1)+8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=seq(0.75, 1.25, by = 0.05), y0=1+8, x1=seq(0.75, 1.25, by = 0.05), y1=8+8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=0, y0=1+8, x1=0, y1=8+8,
         col = "gray", lwd = 2, lty = 1)



par(las = 2)
axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                               "70-74", "75-79", "80-84",
                                               "85-89", "90+"), lwd = 0,
     cex.axis = 1.4)

axis(2, at = seq(9.5, 15.5, by = 1), labels = c("60-64", "65-69",
                                                "70-74", "75-79", "80-84",
                                                "85-89", "90+"), lwd = 0,
     cex.axis = 1.4)

axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                  0.25, by = 0.05)*100, 0),
                                                        "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)


addFac <- 1:7
names(addFac) <- seq(60, 90, by = 5)

for(i in paste(seq(60, 90, by = 5))){
  plotDat <- 
    mort_diff %>%
    filter(Sex == 1 & Country == "GER" & Age == i)
  xx <- c(rep(1, 2), rep(plotDat$med+1, 2))
  yy <- c(0,1, 1, 0)+addFac[i]
  polygon(x=xx,y=yy, col = "darkgray", border = "white")
  
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        pch = 16, typ = "p")
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        typ = "l")
}

for(i in paste(seq(60, 90, by = 5))){
  plotDat <- 
    mort_diff %>%
    filter(Sex == 2 & Country == "GER" & Age == i)
  xx <- c(rep(1, 2), rep(plotDat$med+1, 2))
  yy <- c(0,1, 1, 0)+addFac[i]+8
  polygon(x=xx,y=yy, col = "darkgray", border = "white")
  
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        pch = 16, typ = "p")
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        typ = "l")
}
par(las = 0)
mtext("Germany", 3, line = 1, cex = 2)
mtext("Age group", 3, line = 0.2, cex = 1, adj = -0.15)
text(x=0.75, y=7.5, "Males" , pos = 4, cex = 1.8)
text(x=0.75, y=7.5+8, "Females" , pos = 4, cex = 1.8)
mtext("Relative mortality difference (rural-urban)", 1, line = 4.5, cex = 1.4)

## ## England & Wales
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
       c(1,16), log = "x")

segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
         col = "gray", lwd = 1, lty = 1)

segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=0, y0=1, x1=0, y1=8,
         col = "gray", lwd = 2, lty = 1)

segments(x0=0.75, y0=seq(1, 8, by = 1)+8, x1=1.25, y1=seq(1, 8, by = 1)+8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=seq(0.75, 1.25, by = 0.05), y0=1+8, x1=seq(0.75, 1.25, by = 0.05), y1=8+8,
         col = "gray", lwd = 1, lty = 1)

segments(x0=0, y0=1+8, x1=0, y1=8+8,
         col = "gray", lwd = 2, lty = 1)



par(las = 2)

axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                  0.25, by = 0.05)*100, 0),
                                                        "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)


addFac <- 1:7
names(addFac) <- seq(60, 90, by = 5)

for(i in paste(seq(60, 90, by = 5))){
  plotDat <- 
    mort_diff %>%
    filter(Sex == 1 & Country == "ENW" & Age == i)
  xx <- c(rep(1, 2), rep(plotDat$med+1, 2))
  yy <- c(0,1, 1, 0)+addFac[i]
  polygon(x=xx,y=yy, col = "darkgray", border = "white")
  
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        pch = 16, typ = "p")
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        typ = "l")
}

for(i in paste(seq(60, 90, by = 5))){
  plotDat <- 
    mort_diff %>%
    filter(Sex == 2 & Country == "ENW" & Age == i)
  
  xx <- c(rep(1, 2), rep(plotDat$med+1, 2))
  yy <- c(0,1, 1, 0)+addFac[i]+8
  polygon(x=xx,y=yy, col = "darkgray", border = "white")
  
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        pch = 16, typ = "p")
  lines(x=c(plotDat$low, plotDat$up)+1, y=rep(yy[1]+0.5, 2),
        typ = "l")
}
par(las = 0)
mtext("England & Wales", 3, line = 1, cex = 2)
## mtext("Age group", 3, line = 0.2, cex = 1, adj = -0.15)
## text(x=-0.3, y=7.5, "Germany" , pos = 4, cex = 1.8)
## text(x=-0.3, y=7.5+8, "England & Wales" , pos = 4, cex = 1.8)
mtext("Relative mortality difference (rural-urban)", 1, line = 4.5, cex = 1.4)

dev.off()

