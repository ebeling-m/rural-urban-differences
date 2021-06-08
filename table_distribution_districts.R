# Descriptive table of 

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

ENW_GER <- 
  ENW_GER %>% 
  mutate(mx = Dx/Nx) %>% 
  group_by(Geo, Sex) %>% 
  mutate(e60 = lifetable(mx))

ENW_GER %>% group_by(Sex, Country) %>% group_map(~quantile(.$e60, probs = c(1/3, 2/3)), .keep = TRUE)

quant_Fct <- function(Sex, Geo, Country, e60){
  breaks <- quantile(e60, probs = c(0, 1/3, 2/3, 1))
  e60Cat <- cut(e60, breaks = breaks, labels = 1:3, include.lowest = TRUE)
  out <- data.frame(Sex = Sex, Country = Country, Geo = Geo, e60Cat = e60Cat)
  return(out)
}

ENW_GER <- 
  ENW_GER %>% 
  distinct(Geo, e60, Country) %>% 
  group_by(Country, Sex) %>%
  group_map(~quant_Fct(Sex=.$Sex, Country = .$Country, Geo = .$Geo, e60 = .$e60), .keep = TRUE) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  left_join(ENW_GER) %>% 
  distinct(Country, Sex, Geo, PDcat, e60Cat)

table(ENW_GER$PDcat, ENW_GER$e60Cat, ENW_GER$Country, ENW_GER$Sex)