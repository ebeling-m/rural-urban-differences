# source("packages_functions.r")
## Turn off exponential writing
options(scipen = 999, java.parameters = "-Xmx8000m")

# File with NUTS 3 codes for Germany
## Corrected Version for NUTS for Börde and Jerichower Land
link <- read.xlsx("Referenz Gemeinden-GVB-Kreise_NUTSCorrected20201103.xlsx",
                  rowIndex = c(1, 3:403), stringsAsFactors = FALSE, sheetIndex= 4,
                  header = TRUE,colIndex=c(1,2,3,23, 24))

## Make only numeric AGS
link$GeoNew <- ifelse(nchar(link$krs17) == 7,
                      substr(link$krs17, 1,4),
                      substr(link$krs17, 1,5))

## Adjust for changes in Cochem-Zell (DEB16 changed to DEB1C)
## Adjust for changes in Rhein-Hunsrück Kreis (DEB19 changed to DEB1D)

link$NUTS3new <- link$NUTS3

link$NUTS3new[link$NUTS3new == "DEB16"] <- "DEB1C"
link$NUTS3new[link$NUTS3new == "DEB19"] <- "DEB1D"

# Load death data
## read data for 2018 (Cause of death data (restricted access))
dat18 <- read.xlsx("Sterbefälle_AlterG60-90_Geschl_Kreis_2018.xlsx",
                   startRow = 1, stringsAsFactors = FALSE, sheetIndex= 1,
                   header = TRUE,colIndex=1:5)
head(dat18)
names(dat18) <- c("Year", "Age", "Geo", "Sex", "Death")
dat18$GeoNum <- as.numeric(dat18$Geo)


## read data for 2016 and 2017 (Eurostat)

dat16 <- read.xlsx("Eurostat_Gestorbene_Geschl_Ag_90+_Kreise_2013_2018_Spalten.xlsx",
                   startRow = 9, stringsAsFactors = FALSE, sheetIndex= 4,
                   header = TRUE,colIndex=1:5) 

dat17 <- read.xlsx("Eurostat_Gestorbene_Geschl_Ag_90+_Kreise_2013_2018_Spalten.xlsx",
                   startRow = 9, stringsAsFactors = FALSE, sheetIndex= 5,
                   header = TRUE,colIndex=1:5)

## Last two rows in both datasets contain info footnotes: delete them
tail(dat16)
tail(dat17)

dat16 <- dat16[-c(nrow(dat16), nrow(dat16)-1),]
dat17 <- dat17[-c(nrow(dat17), nrow(dat17)-1),]

tail(dat17)
tail(dat16)

## Check availiability AGS in 2018
miss18 <- dat18$GeoNum[!dat18$GeoNum %in% link$GeoNew]

## Check availiability NUTS-3 in 2016 and 2017
miss17 <- dat17$NUTS.3[!dat17$NUTS.3 %in% link$NUTS3new]

## Check availiability NUTS-3 in 2016 and 2017
miss16 <- dat16$NUTS.3[!dat16$NUTS.3 %in% link$NUTS3new]

# Respective codes all included in the data in the link file (Object: "link")

## Merge AGS to data for 2016 and 2017
dat16$GeoNew <- mapvalues(dat16$NUTS.3, from = link$NUTS3new, to = link$GeoNew)
dat17$GeoNew <- mapvalues(dat17$NUTS.3, from = link$NUTS3new, to = link$GeoNew)

## Recode Age and selection of ages needed
ageRecode <- cbind(unique(dat16$Alter), c(rep(1, 13), seq(60, 90, by =
                                                            5), 1))
ageRecode

dat16$AgeNew <- mapvalues(dat16$Alter, from = ageRecode[,1], to = ageRecode[,2])
dat17$AgeNew <- mapvalues(dat17$Alter, from = ageRecode[,1], to = ageRecode[,2])

# Select only Ages 60+
dat16.1 <- dat16[dat16$AgeNew > 1,]
dat17.1 <- dat17[dat17$AgeNew > 1,]

# Age recode for 2018 data
ageRecode18 <- cbind(unique(dat18$Age), seq(60,90, by = 5))
ageRecode18

dat18$AgeNew <- mapvalues(dat18$Age, from = ageRecode18[,1], to = ageRecode18[,2])
head(dat18)

## Modify format of data for 2016 and 2017 to match 2018
dat16.lm <- 
        dat16.1 %>% 
        select(AgeNew, GeoNew, Männer, Frauen) %>% 
        pivot_longer(cols = c(Frauen, Männer), 
                     names_to = "Sex1", 
                     values_to = "Death") %>%
            mutate(Sex = mapvalues(Sex1, 
                                   from = c("Frauen" , "Männer"), 
                                   to = c(2,1)), Year = 2016) %>% 
            select(Year, AgeNew, GeoNew, Sex, Death) %>% 
        rename(Age = AgeNew, Geo = GeoNew) %>% 
        as_tibble()

dat17.lm <- 
        dat17.1 %>% 
        select(AgeNew, GeoNew, Männer, Frauen) %>% 
        pivot_longer(cols = c(Frauen, Männer), 
                     names_to = "Sex1", 
                     values_to = "Death") %>%
            mutate(Sex = mapvalues(Sex1, 
                                   from = c("Frauen" , "Männer"), 
                                   to = c(2,1)), Year = 2017) %>% 
            select(Year, AgeNew, GeoNew, Sex, Death) %>% 
        rename(Age = AgeNew, Geo = GeoNew) %>% 
        as_tibble()

## Modify format for 2018 
head(dat18)

dat18.lm <- dat18 %>% as_tibble() %>% mutate(Sex = mapvalues(Sex, from = c("m", "w"), to = c(1,2))) %>% 
            select(Year, AgeNew, Sex, GeoNum, Death) %>% rename(Age = AgeNew, Geo = GeoNum)

## Create Final death data set
datLong <- rbind(dat16.lm, dat17.lm, dat18.lm)


##############################################################################
##############################################################################
##############################################################################
## Load Population Data (obtained from Genesis)
pop2015 <- read.csv("pop2015.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
pop2015$Year <- 2015

pop2016 <- read.csv("pop2016.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
pop2016$Year <- 2016

pop2017 <- read.csv("pop2017.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
pop2017$Year <- 2017

pop2018 <- read.csv("pop2018.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
pop2018$Year <- 2018

names(pop2015) <- names(pop2016) <- names(pop2017) <- names(pop2018) <- c("Geo", "Name", "Age", "Total", "Male", "Female", "Year")

# Merge data together
pop <- rbind(pop2015, pop2016, pop2017, pop2018)

# Edit data to include only NUTS 3 and match NUTS 3 Format and 
# to match modified districts (Osterode (03156) and Göettingen (03152)
## to newly created Kreis Göttingen (3159) applies for 2015)

pop1 <- 
        pop %>% 
        as_tibble() %>% 
        mutate(GeoNew = as.numeric(Geo)) %>% 
        filter(!is.na(as.numeric(GeoNew))) %>% 
        mutate(GeoNew = ifelse(GeoNew %in% c(3156, 3152, 2, 11), 
               mapvalues(GeoNew, 
                         from = c(3156, 3152, 2, 11), 
                         to = c(3159, 3159, 2000, 11000)), GeoNew)) %>% 
        filter(GeoNew %in% link$GeoNew) %>% 
        pivot_longer(cols = c(Male, Female), names_to = "Sex", values_to = "Pop") %>% 
        mutate(Sex = mapvalues(Sex, from =c("Male", "Female"), to = c(1,2))) %>%
        select(Age, Year, GeoNew, Sex, Pop) %>% 
        rename(Geo = GeoNew)

# # Check if coding worked correct
# test <- table(pop1$Geo, pop1$Year)
# dim(test)

## Recode Age 
ageRecode <- cbind(unique(pop1$Age), c(rep(1, 60), rep(60, 5),
                                          rep(65, 5), rep(70, 5), 75, 80, 85, 90, 2))
pop1$AgeNew <- mapvalues(pop1$Age, from = ageRecode[,1], to = ageRecode[,2])

# Make Table long format
# Entries for "Landkreis" with modification are marked with "-", delete those entries
pop2 <- 
        pop1 %>% 
        filter(AgeNew >= 60) %>% 
        mutate(Pop = as.numeric(Pop)) %>% 
        select(Year, Geo, AgeNew, Sex, Pop) %>% 
        rename(Age = AgeNew) %>% 
        filter(!is.na(Pop)) %>% 
        group_by(Year, Geo, Age, Sex) %>% 
        summarise(Pop = sum(Pop))

## write datafile for Total population (potentially for PD estimation)
write.table(pop2, file = "totalPopulation2015to18Germany_20210510.txt",
            row.names = FALSE)
 
# Calculate Average population
pop3 <- 
        pop2 %>% 
        pivot_wider(id_cols = c(Sex, Age, Geo), names_from = Year, values_from = Pop) %>%
        mutate(ave2016=(`2015`+`2016`)/2,
               ave2017=(`2016`+`2017`)/2,
               ave2018=(`2017`+`2018`)/2) %>% 
        pivot_longer(cols = c(ave2016, ave2017, ave2018), names_to = "Year", values_to = "Pop") %>% 
        select(Age, Sex, Geo, Year, Pop) %>% 
        mutate(Year = mapvalues(Year, from =c("ave2016", "ave2017", "ave2018"), to = 2016:2018))

## Merge Population Data with Death Data
datLong$Geo <- as.numeric(datLong$Geo)
pop3$Year <- as.numeric(pop3$Year)
popDx <- pop3 %>% 
        ungroup() %>%  
        left_join(datLong)

## Link Area Size to Data
link$GeoNew <- as.numeric(link$GeoNew)
popDx1 <- 
        link %>% 
        select(GeoNew, fl17) %>% 
        as_tibble() %>%  
        rename(Geo = GeoNew, Area=fl17) %>% 
        right_join(popDx)

## Calculate Population Density
# Total Population also including ages below 60

totPop <- 
        pop1 %>%
        filter(AgeNew == 2) %>% 
        mutate(PopNum = as.numeric(Pop)) %>% 
        group_by(Geo, Year) %>%
        summarise(TotPop = sum(PopNum)) %>% 
        pivot_wider(id_cols = Geo, names_from = Year, values_from = TotPop) %>% 
        mutate(aveTot = (`2015`+`2016`+`2016`+`2017`+`2017`+`2018`)/6) %>% 
        select(Geo, aveTot) %>% 
        right_join(popDx1) %>% 
        mutate(PD = aveTot/Area) %>% 
        mutate(PDcat = case_when(PD <= 300 ~ 1,
                                 PD > 300 & PD <= 1000 ~ 2, 
                                 PD > 1000 ~ 3)) 

# alternative definiton rural urban
altDef <- read.table("AlternativeRuralUrban_DefGER2017_INKAR20210512.csv", 
                     header = FALSE, skip = 2, sep = ";", stringsAsFactors = FALSE)

datGER <- 
        altDef %>% 
        select(V1, V4, V5) %>% 
        rename(Geo = V1, distTyp = V4, rurUrb = V5) %>% 
        as_tibble() %>% 
        right_join(totPop)

