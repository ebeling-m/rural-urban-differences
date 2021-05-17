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
  out <- data.frame(med = distDiff[2])
  out <- data.frame(low=distDiff[1], med = distDiff[2], up = distDiff[3])
  return(out)
  
}

  
test <- 
  ENW_GER %>% 
  group_by(Age, Sex, Country, PDcat) %>%
  
  do(unadjusted_diff(Dx=.$Dx, Nx =.$Nx, PDcat = .$PDcat))
