# This code calculates rural-urban mortality differences for alternative definitons of rural-urban

####################################
########### England (Alternative Rural Urban defintion only available for England)
####################################

EN <- 
  datENW %>%
  filter(!is.na(RUR_EN)) %>% 
  group_by(Age, Sex, Geo, RUR_EN) %>% 
  summarize(Nx = sum(Pop), Dx = sum(Death))

EN$RUR_NUM <- mapvalues(EN$RUR_EN, from = unique(EN$RUR_EN), to = 3:1)
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
    group_by(Age, RUR_NUM, e60Cat) %>% 
    mutate(mean_mx = mean(mxSim)) %>% 
    distinct(RUR_NUM, e60Cat, mean_mx) %>% 
    # filter(PDcat %in% c(1,3)) %>% 
    pivot_wider(id_cols = c(Age, e60Cat), values_from = "mean_mx", names_from = "RUR_NUM") %>% 
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
mort_diff_adj_EN <- list()

for(i in 1:2){
  input <- EN %>% filter(Sex == i)
  clusterExport(cl, "input")
  mort_diff_adj_EN[[i]] <- boot_est(input)
  mort_diff_adj_EN[[i]]$Sex <- i
}

stopCluster(cl) 

# Combine list to one dataset
mort_diff_adj_EN <- mort_diff_adj_EN %>% bind_rows()

# Create Plot for England 

colLE <- c('#41b6c4','#2c7fb8','#253494')


pdf("totals_adjusted_England_alternativeRuralUrban.pdf", family = "Times", width = 24, height
    = 10, pointsize = 17)
profile <- layout(rbind(1:3, 4:6, rep(7, 3)),
                  heights = c(0.45,0.45,0.1))

par(xpd = TRUE, mar = c(3,5,3,1))

## ## ## ## ## ## Females
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
    if(le == 1){
      axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                     "70-74", "75-79", "80-84",
                                                     "85-89", "90+"), lwd = 0,
           cex.axis = 1.4)
      par(las = 0)
      mtext("Females", 3, line = -1.5, cex = 1.2, adj = 0)
    }
    
    par(las = 0)
    if(le == 1){
      mtext("England", 3, line = 0.5, cex = 1.4, adj = 0)
    }
    
    par(las = 2)
    axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                      0.25, by = 0.05)*100, 0),
                                                            "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
    
    
    addFac <- 1:7
    names(addFac) <- seq(60, 90, by = 5)
    
    for(age in paste(seq(60, 90, by = 5))){
      plotDat <- 
        mort_diff_adj_EN %>% 
        filter(Sex == 2 & Age == age & e60Cat == le)
      xx <- c(rep(1, 2), rep(plotDat$med_RU+1, 2))
      yy <- c(0,1, 1, 0)+addFac[age]
      polygon(x=xx,y=yy, col = colLE[le], border = "white")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            pch = 16, typ = "p", cex = 1, col = "darkgray")
      lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
            typ = "l", lwd =1, col = "darkgray")
      lines(x=plotDat$med_RI+1, y=yy[1]+0.5, pch = 15, typ = "p")
    }
  }


## ## ## ## ## Males
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
  if(le == 1){
    axis(2, at = seq(1.5, 7.5, by = 1), labels = c("60-64", "65-69",
                                                   "70-74", "75-79", "80-84",
                                                   "85-89", "90+"), lwd = 0,
         cex.axis = 1.4)
    par(las = 0)
    mtext("Males", 3, line = -1.5, cex = 1.2, adj = 0)
  }
  
  par(las = 0)

  if(le == 2){
    par(las = 0)
    mtext("Relative mortality difference (rural-urban)", 1, line = 4.5, cex
          = 1.4)
  }
  
  
  par(las = 2)
  axis(1, at = seq(0.75, 1.25, by = 0.05), labels = paste(round(seq(-0.25,
                                                                    0.25, by = 0.05)*100, 0),
                                                          "%", sep =""), lwd = 3, cex.axis = 1.2, pos = 0.95)
  
  
  addFac <- 1:7
  names(addFac) <- seq(60, 90, by = 5)
  
  for(age in paste(seq(60, 90, by = 5))){
    plotDat <- 
      mort_diff_adj_EN %>% 
      filter(Sex == 1 & Age == age & e60Cat == le)
    xx <- c(rep(1, 2), rep(plotDat$med_RU+1, 2))
    yy <- c(0,1, 1, 0)+addFac[age]
    polygon(x=xx,y=yy, col = colLE[le], border = "white")
    lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
          pch = 16, typ = "p", cex = 1, col = "darkgray")
    lines(x=c(plotDat$low_RU, plotDat$up_RU)+1, y=rep(yy[1]+0.5, 2),
          typ = "l", lwd =1, col = "darkgray")
    lines(x=plotDat$med_RI+1, y=yy[1]+0.5, pch = 15, typ = "p")
  }
}

## Legend
par(xpd = TRUE, mar = c(1,1,1,1))
plot(x=1:7, y=1:7, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
     bty = "n", xlab = NA, ylab = NA, typ = "n", xlim =
       c(1,6))

legend(x=1, y=5, legend =c("median rural-intermediate difference"), col =
         "black", bty = "n", cex = 1.5, pch = 15, horiz = TRUE)

legend(x=2.25, y=5, legend =c("lower tertile of LE"), col =
         c(colLE[1]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=3.25, y=5, legend =c("middle tertile of LE"), col =
         c(colLE[2]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

legend(x=4.25, y=5, legend =c("upper tertile of LE"), col =
         c(colLE[3]), bty = "n", cex = 2, pch = 15, horiz = TRUE)

dev.off()



