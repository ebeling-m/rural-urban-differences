## Load the list of districts
listDist <-
  read.csv("Local_Authority_Districts_(April_2020)_Names_and_Codes_in_the_United_Kingdom.csv",
           header = TRUE,
           sep =",", stringsAsFactors = FALSE)

## Load Death data, This data contains also deaths by CoD but 
# this will be omitted, Data source: https://www.nomisweb.co.uk/datasets/
death <- read.table("death_dataENW_20201019.tsv", header = TRUE,
                    sep ="", stringsAsFactors = FALSE)

# Reduce data to values necessary, Year >= 2017, all causes
death1 <- 
  death %>% 
  filter(Date > 2016 & 
           cause.of.death == "A00-R99,U00-Y89 All causes, all ages" &
           !Age == "total (all ages)") %>% 
  as_tibble() %>% 
  select(Date, Gender, Age, geogcode, value) %>% 
  rename(Year = Date, Sex = Gender, Geo = geogcode, Death = value) %>% 
  mutate(Sex = mapvalues(Sex, from = c("Male", "Female"), to = 1:2))
 
# Recode Age
ageRecode <- cbind(unique(death1$Age), substr(unique(death1$Age), 6, 7))
death1$Age <- mapvalues(death1$Age, from = ageRecode[,1], to = ageRecode[,2])

## ## Check included districts with list of districts in UK
unique(death1$Geo[!death1$Geo %in% listDist$LAD20CD])
# Data contains an entry for Total death "Column Total", Delete this 
death2 <- 
  death1 %>% 
  filter(!Geo == "Column Total")


## Load Population data. This data contains already mid-year estimates 
pop <- read.table("population_dataENW_20201019.tsv", header = TRUE,
                  sep ="", stringsAsFactors = FALSE)

## Check coverage with death in both directions
unique(pop$geogcode[!pop$geogcode %in% death2$Geo])
unique(death2$Geo[!death2$Geo %in% pop$geogcode])
 
# Pop Data contains Counts for districts in Scotland and Norther Ireland

# 
# ## ## Drop Population estimates for Scotland and Northern Ireland 
# pop$Country <- substr(pop$geogcode,1 ,1)
# head(pop)
# pop1 <- pop[pop$Country %in% c("E", "W"),]
# table(substr(unique(pop1$geogcode), 1, 1))
# 
# ## Recode age variable
# 
# ageLabels <- cbind(unique(pop1$Age), c(seq(60, 80, by = 5), rep(85,
#                                                                 5), 90))
# 
# pop1$AgeNew <- mapvalues(pop1$Age, from = ageLabels[,1], to = ageLabels[,2])
# ## pop1$AgeNew <- as.numeric(pop1$AgeNew)
# head(pop1)
# ## Make array to summarize 
# ## dim(popArr)
# ## dim(deathArr)
# 
# ## Merge population and death data
# popArr <- as.data.frame.table(tapply(pop1$value, INDEX =
#                                        list(pop1$AgeNew, pop1$Date, pop1$geogcode,
#                                             pop1$Gender), FUN = sum),
#                               stringsAsFactors = FALSE)
# 
# head(popArr)
# names(popArr) <- c("Age", "Year", "Geo", "Sex", "Expo")
# popArr <- popArr[popArr$Year > 2012,]
# 
# popArr$ID <- paste(popArr$Year, popArr$Sex, popArr$Geo, popArr$Age,
#                    sep = "_")
# 
# head(deathTotal1)
# deathTotal1$ID <- paste(deathTotal1$Year, deathTotal1$Sex,
#                         deathTotal1$geocode, deathTotal1$AgeNew, sep = "_")
# 
# head(deathCoD1)
# deathCoD1$ID <- paste(deathCoD1$Year, deathCoD1$Sex,
#                       deathCoD1$geocode, deathCoD1$AgeNew, sep = "_")
# 
# head(popArr)
# popArrSel <- popArr[, c("Expo", "ID")]
# head(popArrSel)
# 
# deathTotal2 <- merge(x=deathTotal1, y=popArrSel, by = "ID")
# dim(deathTotal1)
# dim(deathTotal2)
# 
# deathCoD2 <- merge(x=deathCoD1, y=popArrSel, by = "ID")
# dim(deathCoD1)
# dim(deathCoD2)
# 
# head(deathTotal2)
# head(deathCoD2)
# 
# 
# ## Load Density Data
# census <- read.csv("Census2011populationDensityDistricts20200107.tsv", header = TRUE,
#                    sep ="", stringsAsFactors = FALSE)
# names(census)
# head(census)
# 
# unique(census$Units)
# unique(census$Area.Population.Densit)
# unique(census$Rural.Urban)
# 
# head(census)
# census$Country <- substr(census$geogcode, 1, 1)
# censusEnW <- census[census$Country %in% c("E", "W"),]
# head(censusEnW)
# 
# unique(censusEnW$Area.Population.Densit)
# 
# ## Check geocodes
# ## There are severals no matches
# head(deathTotal2)
# 
# unique(deathTotal2$geocode[!deathTotal2$geocode %in% census$geogcode])
# unique(censusEnW$geogcode[!censusEnW$geogcode %in% deathTotal2$geocode])
# ## ## Load geodata
# 
# listDeath <- read.csv("deathGeoCode_data20201019.tsv", header = TRUE,
#                       sep ="", stringsAsFactors = FALSE)
# 
# listCensus <- read.csv("districtCodesCensusData20200107.tsv", header = TRUE,
#                        sep ="", stringsAsFactors = FALSE)
# 
# head(listDeath)
# 
# noMatchDx <- listDeath[listDeath$geogcode %in%
#                          unique(deathTotal2$geocode[!deathTotal2$geocode %in% census$geogcode]),]
# 
# noMatchCensus <- listCensus[listCensus$geogcode %in%
#                               unique(censusEnW$geogcode[!censusEnW$geogcode
#                                                         %in%
#                                                           deathTotal2$geocode]),]
# 
# ## Relabel districts
# head(censusEnW)
# censusEnW$geoNew <- censusEnW$geogcode
# 
# ## Forest Heath (E07000201) and St Edmundsbury (E07000204) merged to
# ## West Suffolk (E07000245)
# censusEnW$geoNew[censusEnW$geoNew %in% c("E07000201", "E07000204")] <-
#   "E07000245"
# ## Waveney (E07000206) and Suffolk Coastal (E07000205) merged to East
# ## Suffolk (E07000244)
# 
# censusEnW$geoNew[censusEnW$geoNew %in% c("E07000206", "E07000205")] <-
#   "E07000244"
# 
# ## Bournemouth (E06000028), Christchurch (E07000048) and Poole
# ## (E06000029) merged to "Bournemouth, Christchurch and Poole" (E06000058)
# censusEnW$geoNew[censusEnW$geoNew %in% c("E06000028", "E07000048", "E06000029")] <-
#   "E06000058"
# 
# 
# ## Taunton Deane (E07000190) and West Somerset (E07000191) merged to
# ## Somerset West and Taunton (E07000246)
# censusEnW$geoNew[censusEnW$geoNew %in% c("E07000190", "E07000191")] <-
#   "E07000246"
# 
# ## East Dorset (E07000049), North Dorset (E07000050), Purbeck
# ## (E07000051), West Dorset (E07000052), Weymouth and Portland (E07000053)
# ## merged to Dorset (E06000059)
# censusEnW$geoNew[censusEnW$geoNew %in% c("E07000049",
#                                          "E07000050", "E07000051",
#                                          "E07000052", "E07000053")] <-
#   "E06000059"
# 
# ## E07000004 Aylesbury Vale, E07000005 Chiltern, E07000006 South Bucks
# ## E07000007 Wycombe merged to Buckinghamshire "E06000060"
# 
# censusEnW$geoNew[censusEnW$geoNew %in% c("E07000004",
#                                          "E07000005", "E07000006",
#                                          "E07000007")] <-
#   "E06000060"
# 
# 
# ## Check codes that are left
# unique(deathTotal2$geocode[!deathTotal2$geocode %in%
#                              censusEnW$geoNew])
# 
# listCensus[listCensus$geogcode %in%
#              unique(censusEnW$geoNew[!censusEnW$geoNew %in%
#                                        deathTotal2$geocode]),]
# 
# ## Check for different variables an summarize accordingly
# names(censusEnW)
# unique(censusEnW$Area.Population.Densit)
# unique(censusEnW$Rural.Urban)
# 
# ## ## Only area is usable but this is splitted into specific groups 
# 
# area <- censusEnW[censusEnW$Area.Population.Densit %in% "Area Hectares" & censusEnW$Rural.Urban %in%
#                     c("Urban (total)", "Rural (total)"),]
# 
# head(area)
# str(area)
# area$value <- as.numeric(area$value)
# areaArr <- as.data.frame.table(tapply(area$value, INDEX =
#                                         list(area$geoNew), FUN = sum))
# names(areaArr) <- c("geocode", "AreaHec")
# 
# head(areaArr)
# head(deathTotal2)
# head(deathCoD2)
# deathTotal2Area <- merge(deathTotal2, areaArr, by = "geocode")
# deathCoD2Area <- merge(deathCoD2, areaArr, by = "geocode")
# 
# dim(deathTotal2)
# dim(deathTotal2Area)
# 
# dim(deathCoD2)
# dim(deathCoD2Area)
# 
# dim(areaArr)
# length(unique(deathCoD2Area$geocode))
# 
# ## ## Calculation total population 
# pop1 <- read.table("TotalPopulation_data20201019.tsv", header = TRUE,
#                    sep ="", stringsAsFactors = FALSE)
# head(pop1)
# 
# totPop <- as.data.frame.table(tapply(pop1$value, INDEX =
#                                        list(pop1$Date,
#                                             pop1$geogcode),
#                                      FUN = sum), stringsAsFactors = FALSE)
# 
# head(totPop)
# names(totPop) <- c("Year", "geocode", "TotPop")
# 
# totPop$ID <- paste(totPop$Year, totPop$geocode, sep ="_")
# totPop$Year <- totPop$geocode <- NULL
# 
# deathTotal2Area$ID <- paste(deathTotal2Area$Year, deathTotal2Area$geocode, sep = "_")
# deathCoD2Area$ID <- paste(deathCoD2Area$Year, deathCoD2Area$geocode, sep = "_")
# 
# deathTotal2Area1 <- merge(deathTotal2Area, totPop, by = "ID")
# deathCoD2Area1 <- merge(deathCoD2Area, totPop, by = "ID")
# 
# dim(deathCoD2Area1)
# dim(deathCoD2Area)
# head(deathCoD2Area1)
# head(deathTotal2Area1)
# 
# deathTotal2Area1$AreaKm2 <- deathTotal2Area1$AreaHec/100
# deathCoD2Area1$AreaKm2 <- deathCoD2Area1$AreaHec/100
# 
# ## ## Take out data
# 
# write.table(deathTotal2Area1, file = "ENWdata20201021.txt", row.names
#             = FALSE)
# 
# write.table(deathCoD2Area1, file = "ENWdataCOD20201021.txt", row.names
#             = FALSE)
