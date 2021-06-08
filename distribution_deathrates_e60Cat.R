# Variability within life expectancy groups

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
  left_join(ENW_GER)

# Plot distributions of mx by age and country 

dev.off()
pdf("hist_DeathRates_Males.pdf", width = 20, height = 10, pointsize = 15)
theme_set(theme_minimal())
ENW_GER1 <- ENW_GER %>% filter(Sex == 1)
  ggplot(ENW_GER1, aes(x = mx, fill = e60Cat, colour = e60Cat)) +
  geom_histogram(alpha = 0.2, aes(y=..density..)) + 
  geom_density(alpha = 0.3, size = 1) +
  scale_fill_manual(values=c('#41b6c4','#2c7fb8','#253494'), name = "Life expectancy tercile") +
  scale_color_manual(values=c('#41b6c4','#2c7fb8','#253494'), name = "Life expectancy tercile") +
  labs(x ="Death rates", y = "Density", fill = "LE Terciles")+
  facet_wrap(vars(Country, Age), nrow = 4, ncol = 7, scales = "free") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key=element_rect())
dev.off()


dev.off()
pdf("hist_DeathRates_Females.pdf", width = 20, height = 10, pointsize = 15)
theme_set(theme_minimal())
ENW_GER1 <- ENW_GER %>% filter(Sex == 2)
ggplot(ENW_GER1, aes(x = mx, fill = e60Cat, colour = e60Cat)) +
  geom_histogram(alpha = 0.2, aes(y=..density..)) + 
  geom_density(alpha = 0.3, size = 1) +
  scale_fill_manual(values=c('#41b6c4','#2c7fb8','#253494'), name = "Life expectancy tercile") +
  scale_color_manual(values=c('#41b6c4','#2c7fb8','#253494'), name = "Life expectancy tercile") +
  labs(x ="Death rates", y = "Density", fill = "LE Terciles")+
  facet_wrap(vars(Country, Age), nrow = 4, ncol = 7, scales = "free") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key=element_rect())
dev.off()

