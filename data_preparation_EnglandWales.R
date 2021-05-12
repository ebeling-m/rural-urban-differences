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
# Source: https://www.nomisweb.co.uk/sources
pop <- read.table("population_dataENW_20201019.tsv", header = TRUE,
                  sep ="", stringsAsFactors = FALSE)

## Check coverage with death in both directions
unique(pop$geogcode[!pop$geogcode %in% death2$Geo])
unique(death2$Geo[!death2$Geo %in% pop$geogcode])

# Pop Data contains Counts for districts in Scotland and Norther Ireland
# Drop these counts
# Additionally rename and recode variables + select necessary
# Select also only necessary years (2017-2019)


pop1 <- 
  pop %>%
  as_tibble() %>% 
  filter(substr(geogcode, 1, 1) %in% c("W", "E")) %>% 
  rename(Year = Date, Sex = Gender, Geo = geogcode, Pop = value) %>% 
  select(Year, Age, Sex, Geo, Pop) %>% 
  filter(Year > 2016) %>% 
  mutate(Sex = mapvalues(Sex, from = c("Male", "Female"), to = c(1,2)))
 
# Recode age
ageCode <- ifelse(nchar(unique(pop1$Age)) == 6, 
                  substr(unique(pop1$Age), 5,6), 
                  substr(unique(pop1$Age), 6,7))
ageCode[ageCode %in% 85:89] <- 85
pop1$Age <- mapvalues(pop1$Age, from = unique(pop1$Age), to = ageCode)
 
# Collapse Age 85-89 since pop estimates for this group are in single years

pop2 <- 
  pop1 %>% 
  group_by(Year, Age, Sex, Geo) %>% 
  summarise(Pop = sum(Pop))

# Merge population and death data
finalDat <- 
  pop2 %>% 
  right_join(death2)


## Load Density Data, Based on Census 2011
census <- read.csv("Census2011populationDensityDistricts20200107.tsv", header = TRUE,
                   sep ="", stringsAsFactors = FALSE)

## Check geocodes
unique(census$geogcode[!census$geogcode %in% finalDat$Geo])
unique(finalDat$Geo[!finalDat$Geo %in% census$geogcode])

# There are several districts that don't match due to 
# a restructuring of areas
# Relabel districts
census$GeoNew <- census$geogcode
## Forest Heath (E07000201) and St Edmundsbury (E07000204) merged to
## West Suffolk (E07000245)
census$GeoNew[census$GeoNew %in% c("E07000201", "E07000204")] <- "E07000245"

## Waveney (E07000206) and Suffolk Coastal (E07000205) merged to East
## Suffolk (E07000244)
census$GeoNew[census$GeoNew %in% c("E07000206", "E07000205")] <-
  "E07000244"

## Bournemouth (E06000028), Christchurch (E07000048) and Poole
## (E06000029) merged to "Bournemouth, Christchurch and Poole" (E06000058)
census$GeoNew[census$GeoNew %in% c("E06000028", "E07000048", "E06000029")] <-
  "E06000058"

## Taunton Deane (E07000190) and West Somerset (E07000191) merged to
## Somerset West and Taunton (E07000246)
census$GeoNew[census$GeoNew %in% c("E07000190", "E07000191")] <-
  "E07000246"

## East Dorset (E07000049), North Dorset (E07000050), Purbeck
## (E07000051), West Dorset (E07000052), Weymouth and Portland (E07000053)
## merged to Dorset (E06000059)
census$GeoNew[census$GeoNew %in% c("E07000049",
                                         "E07000050", "E07000051",
                                         "E07000052", "E07000053")] <-
  "E06000059"

## E07000004 Aylesbury Vale, E07000005 Chiltern, E07000006 South Bucks
## E07000007 Wycombe merged to Buckinghamshire "E06000060"
census$GeoNew[census$GeoNew %in% c("E07000004",
                                         "E07000005", "E07000006",
                                         "E07000007")] <-
  "E06000060"

# Check matching again 
unique(census$GeoNew[!census$GeoNew %in% finalDat$Geo])
unique(finalDat$Geo[!finalDat$Geo %in% census$GeoNew])
# Codes match

# Prepare dataset: select neccesary information (Area size, etc)
# Also transform hectares to km^2

head(census)
unique(census$Area.Population.Densit)

census1 <- 
  census %>%
  as_tibble() %>% 
  filter(Area.Population.Densit == "Area Hectares") %>% 
  select(GeoNew, value) %>% 
  rename(Geo = GeoNew, Area = value) %>% 
  group_by(Geo) %>% 
  summarise(Area = sum(as.numeric(Area))) %>% 
  mutate(Area = Area*0.01) 



## Calculation total population, Source similar to other Pop Data
totPop <- read.table("TotalPopulation_data20201019.tsv", header = TRUE,
                   sep ="", stringsAsFactors = FALSE)

totPop1 <- 
  totPop %>%
  as_tibble() %>% 
  filter(substr(geogcode, 1, 1) %in% c("W", "E")) %>% 
  rename(Year = Date, Sex = Gender, Geo = geogcode, Pop = value) %>% 
  select(Year, Age, Sex, Geo, Pop) %>% 
  filter(Year > 2016) %>% 
  mutate(Sex = mapvalues(Sex, from = c("Male", "Female"), to = c(1,2))) %>% 
  group_by(Year, Geo) %>% 
  summarise(Pop = sum(Pop)) %>% 
  group_by(Geo) %>% 
  summarise(aveTot = mean(Pop))

# Merge datasets
densENW <- 
  right_join(totPop1, census1) %>% 
  mutate(PD = aveTot/Area) %>% 
  mutate(PDcat = case_when(PD <= 100 ~ 1,
                           PD > 100 & PD <= 1000 ~ 2, 
                           PD > 1000 ~ 3)) 

# Load alternative rural urban definition, Source: Census 2011
rur <- read.csv("RUC_LAD_2011_EN_LU.csv", header = TRUE, stringsAsFactors = FALSE)
dim(rur)


