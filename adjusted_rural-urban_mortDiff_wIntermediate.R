# This code calculates and plots the adjusted rural-urban mortality differences

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

# Functions to calculate rural-urban differences 
# using bootstrapping 

# Function for bootstrap Dx, LE calc and rural-urban diff
bootstrap <- function(iteration){
  x1 <- 
    input %>% 
    mutate(mxSim = rpois(n = length(Dx), lambda = Dx)/Nx) %>% 
    group_by(Geo) %>% 
    mutate(e60 = lifetable(mxSim))
  
  e60 <- x1 %>% distinct(Geo, e60)
  e60breaks <- quantile(e60$e60, probs = c(0, 1/3, 2/3, 1))
  x1$e60Cat <- cut(x1$e60, breaks = e60breaks, labels = 1:3, 
                   include.lowest = TRUE)
  
  meanMx <- 
    x1 %>% 
    group_by(Age, PDcat, e60Cat) %>% 
    mutate(mean_mx = mean(mxSim)) %>% 
    distinct(PDcat, e60Cat, mean_mx) %>% 
    # filter(PDcat %in% c(1,3)) %>% 
    pivot_wider(id_cols = c(Age, e60Cat), values_from = "mean_mx", names_from = "PDcat") %>% 
    mutate(mx_diff_RU = (`1`/`3`)-1,
           mx_diff_RI = (`1`/`2`)-1)
  return(meanMx)
}

# Set up cluster to speed up bootstrap estimation
numCore <- 8
cl <- makeCluster(numCore)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(tidyverse)
})
clusterExport(cl, c("bootstrap", "lifetable"))

# getDoParWorkers()
# stopCluster(cl)

# Function to perform bootstrap estimation 
boot_est <- function(input){
  # clusterExport(cl, "input")
  
  boot_mx <- foreach(i = 1:10000) %dopar% bootstrap(iteration = i) 
  
  quant_fct <- function(diffs_RU, diffs_RI){
    values_RU <-  quantile(diffs_RU, probs = c(0.025, 0.5, 0.975))
    values_RI <-  quantile(diffs_RI, probs = c(0.025, 0.5, 0.975))
    out <- data.frame(low_RU = values_RU[1], med_RU = values_RU[2], up_RU = values_RU[3],
                      low_RI = values_RI[1], med_RI = values_RI[2], up_RI = values_RI[3])
    return(out)
  }
  
  est <- 
    boot_mx %>% 
    bind_rows() %>%  
    group_by(Age, e60Cat) %>% 
    do(quant_fct(diffs_RU=.$mx_diff_RU, diffs_RI=.$mx_diff_RI))
  return(est)
  
} 

# Perform bootsrap estimation
sex_country <- expand.grid(1:2, c("GER", "ENW"))
mort_diff_adj <- list()

for(i in 1:4){
  input <- ENW_GER %>% filter(Sex == sex_country[i,1] & Country == 
                                sex_country[i,2])
  clusterExport(cl, "input")
  mort_diff_adj[[i]] <- boot_est(input)
  mort_diff_adj[[i]]$Sex <- sex_country[i,1]
  mort_diff_adj[[i]]$Country <- sex_country[i,2]
  
}

stopCluster(cl) 

# Combine list to one dataset
mort_diff_adj <- mort_diff_adj %>% bind_rows()

# Plot results
colLE <- c('#41b6c4','#2c7fb8','#253494')


pdf("totals_adjusted.pdf", family = "Times", width = 24, height
    = 10, pointsize = 17)
profile <- layout(rbind(1:6, 7:12, rep(13, 6)),
                  heights = c(0.45,0.45,0.1))

par(xpd = TRUE, mar = c(3,5,3,1))

## ## ## ## ## ## Females
for(country in c("GER", "ENW")){
  for(le in 1:3){
    plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
           c(1,8), log = "x")
    
    segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=1, y0=1, x1=1, y1=8,
             col = "lightgray", lwd = 2, lty = 2)
    
    par(las = 2)
    if(le == 1 & country == "GER"){
      axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                     "70-74", "75-79", "80-84",
                                                     "85-89", "90+"), lwd = 0,
           cex.axis = 1.4)
      par(las = 0)
      mtext("Females", 3, line = -1.5, cex = 1.2, adj = 0)
    }
    
    par(las = 0)
    if(le == 1 & country == "GER"){
      mtext("Germany", 3, line = 0.5, cex = 1.4, adj = 0)
    }
    
    if(le == 1 & country == "ENW"){
      mtext("England & Wales", 3, line = 0.5, cex = 1.4, adj = 0)
    }
    
    par(las = 2)
    axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                      0.25, by = 0.05)*100, 0),
                                                            "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
    
    
    addFac <- 1:7
    names(addFac) <- seq(60, 90, by = 5)
    
    for(age in paste(seq(60, 90, by = 5))){
      plotDat <- 
        mort_diff_adj %>% 
        filter(Sex == 2 & Age == age & e60Cat == le & Country == country)
      xx <- c(rep(1, 2), rep(plotDat$med_RU+1, 2))
      yy <- c(0,1, 1, 0)+addFac[age]
      polygon(x=xx,y=yy, col = colLE[le], border = "white")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            pch = 16, typ = "p", cex = 1, col = "darkgray")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            typ = "l", lwd =1, col = "darkgray")
    }
  }
}

## ## ## ## ## Males
for(country in c("GER", "ENW")){
  for(le in 1:3){
    plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
           c(1,8), log = "x")
    
    segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=1, y0=1, x1=1, y1=8,
             col = "lightgray", lwd = 2, lty = 2)
    
    par(las = 2)
    if(le == 1 & country == "GER"){
      axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                     "70-74", "75-79", "80-84",
                                                     "85-89", "90+"), lwd = 0,
           cex.axis = 1.4)
      par(las = 0)
      mtext("Males", 3, line = -1.5, cex = 1.2, adj = 0)
    }
    
    par(las = 2)
    axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                      0.25, by = 0.05)*100, 0),
                                                            "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
    
    
    addFac <- 1:7
    names(addFac) <- seq(60, 90, by = 5)
    
    for(age in paste(seq(60, 90, by = 5))){
      plotDat <- 
        mort_diff_adj %>% 
        filter(Sex == 1 & Age == age & e60Cat == le & Country == country)
      
      xx <- c(rep(1, 2), rep(plotDat$med_RU+1, 2))
      yy <- c(0,1, 1, 0)+addFac[age]
      polygon(x=xx,y=yy, col = colLE[le], border = "white")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            pch = 16, typ = "p", cex = 1, col = "darkgray")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            typ = "l", lwd =1, col = "darkgray")
    }
    if(le == 3 & country == "GER"){
      par(las = 0)
      mtext("Relative mortality difference (rural-urban)", 1, line = 4.5, cex
            = 1.4, adj = -0.25)
    }
    
  }
}

## Legend
par(xpd = TRUE, mar = c(1,1,1,1))
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
       c(1,6))

legend(x=2.25, y=5, legend =c("lower tertile of LE"), col =
         c(colLE[1]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=3.25, y=5, legend =c("middle tertile of LE"), col =
         c(colLE[2]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=4.25, y=5, legend =c("upper tertile of LE"), col =
         c(colLE[3]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

dev.off()

############################################
############################################
############################################
# Intermediate group versus rural
############################################
############################################
############################################


pdf("totals_adjusted_RuralIntermediate.pdf", family = "Times", width = 24, height
    = 10, pointsize = 17)
profile <- layout(rbind(1:6, 7:12, rep(13, 6)),
                  heights = c(0.45,0.45,0.1))

par(xpd = TRUE, mar = c(3,5,3,1))

## ## ## ## ## ## Females
for(country in c("GER", "ENW")){
  for(le in 1:3){
    plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
           c(1,8), log = "x")
    
    segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=1, y0=1, x1=1, y1=8,
             col = "lightgray", lwd = 2, lty = 2)
    
    par(las = 2)
    if(le == 1 & country == "GER"){
      axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                     "70-74", "75-79", "80-84",
                                                     "85-89", "90+"), lwd = 0,
           cex.axis = 1.4)
      par(las = 0)
      mtext("Females", 3, line = -1.5, cex = 1.2, adj = 0)
    }
    
    par(las = 0)
    if(le == 1 & country == "GER"){
      mtext("Germany", 3, line = 0.5, cex = 1.4, adj = 0)
    }
    
    if(le == 1 & country == "ENW"){
      mtext("England & Wales", 3, line = 0.5, cex = 1.4, adj = 0)
    }
    
    par(las = 2)
    axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                      0.25, by = 0.05)*100, 0),
                                                            "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
    
    
    addFac <- 1:7
    names(addFac) <- seq(60, 90, by = 5)
    
    for(age in paste(seq(60, 90, by = 5))){
      plotDat <- 
        mort_diff_adj %>% 
        filter(Sex == 2 & Age == age & e60Cat == le & Country == country)
      xx <- c(rep(1, 2), rep(plotDat$med_RI+1, 2))
      yy <- c(0,1, 1, 0)+addFac[age]
      polygon(x=xx,y=yy, col = colLE[le], border = "white")
      lines(x=c(plotDat$low_RI, plotDat$up_RI)+1, y=rep(yy[1]+0.5, 2),
            pch = 16, typ = "p", cex = 1, col = "darkgray")
      lines(x=c(plotDat$low_RI, plotDat$up_RI)+1, y=rep(yy[1]+0.5, 2),
            typ = "l", lwd =1, col = "darkgray")
      lines(x=plotDat$med_RU+1, y=yy[1]+0.5,
            pch = 15, typ = "p", cex = 1, col = "black")
    }
  }
}

## ## ## ## ## Males
for(country in c("GER", "ENW")){
  for(le in 1:3){
    plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         bty = "n", xlab = NA, ylab = NA, typ = "n", xlim = c(0.75,1.25), ylim =
           c(1,8), log = "x")
    
    segments(x0=0.75, y0=seq(1, 8, by = 1), x1=1.25, y1=seq(1, 8, by = 1),
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=seq(0.75, 1.25, by = 0.05), y0=1, x1=seq(0.75, 1.25, by = 0.05), y1=8,
             col = "lightgray", lwd = 1, lty = 2)
    
    segments(x0=1, y0=1, x1=1, y1=8,
             col = "lightgray", lwd = 2, lty = 2)
    
    par(las = 2)
    if(le == 1 & country == "GER"){
      axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                     "70-74", "75-79", "80-84",
                                                     "85-89", "90+"), lwd = 0,
           cex.axis = 1.4)
      par(las = 0)
      mtext("Males", 3, line = -1.5, cex = 1.2, adj = 0)
    }
    
    par(las = 2)
    axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                      0.25, by = 0.05)*100, 0),
                                                            "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
    
    
    addFac <- 1:7
    names(addFac) <- seq(60, 90, by = 5)
    
    for(age in paste(seq(60, 90, by = 5))){
      plotDat <- 
        mort_diff_adj %>% 
        filter(Sex == 1 & Age == age & e60Cat == le & Country == country)
      
      xx <- c(rep(1, 2), rep(plotDat$med_RI+1, 2))
      yy <- c(0,1, 1, 0)+addFac[age]
      polygon(x=xx,y=yy, col = colLE[le], border = "white")
      lines(x=c(plotDat$low_RI, plotDat$up_RI)+1, y=rep(yy[1]+0.5, 2),
            pch = 16, typ = "p", cex = 1, col = "darkgray")
      lines(x=c(plotDat$low_RI, plotDat$up_RI)+1, y=rep(yy[1]+0.5, 2),
            typ = "l", lwd =1, col = "darkgray")
      lines(x=plotDat$med_RU+1, y=yy[1]+0.5,
            pch = 15, typ = "p", cex = 1, col = "black")
    }
    if(le == 3 & country == "GER"){
      par(las = 0)
      mtext("Relative mortality difference (rural-intermediate)", 1, line = 4.5, cex
            = 1.4, adj = -0.25)
    }
    
  }
}

## Legend
par(xpd = TRUE, mar = c(1,1,1,1))
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
       c(1,6))
legend(x=1, y=5, legend =c("median rural-urban difference"), col =
         "black", bty = "n", cex = 1.5, pch = 15, horiz = TRUE)

legend(x=2.25, y=5, legend =c("lower tertile of LE"), col =
         c(colLE[1]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=3.25, y=5, legend =c("middle tertile of LE"), col =
         c(colLE[2]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=4.25, y=5, legend =c("upper tertile of LE"), col =
         c(colLE[3]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

dev.off()

