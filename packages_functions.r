## Turn off exponential writing and increase 
options(scipen = 999, java.parameters = "-Xmx8000m")

# Packages
library(xlsx)
library(tidyverse)
library(doParallel)

# Additional Function borrowed from plyr package

mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}

# Function for calculating life expectancy 

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