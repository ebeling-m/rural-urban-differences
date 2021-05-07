source("packages_functions.r")
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
library(plyr)
head(dat16)

dat16$GeoNew <- mapvalues(dat16$NUTS.3, from = link$NUTS3new, to = link$GeoNew)
dat17$GeoNew <- mapvalues(dat17$NUTS.3, from = link$NUTS3new, to = link$GeoNew)

head(dat17)
head(dat18)

dat17[dat17$NUTS.3 == "DEE06",]
dat17[dat17$GeoNew == "15086",]
dat18[dat18$GeoNum == "15086",]
dat17[dat17$GeoNew == "15083",]


## Recode Age and selection of ages needed

unique(dat16$Alter)==unique(dat17$Alter)

ageRecode <- cbind(unique(dat16$Alter), c(rep(1, 13), seq(60, 90, by =
                                                            5), 1))
ageRecode

dat16$AgeNew <- mapvalues(dat16$Alter, from = ageRecode[,1], to = ageRecode[,2])
dat17$AgeNew <- mapvalues(dat17$Alter, from = ageRecode[,1], to = ageRecode[,2])

dat16.1 <- dat16[dat16$AgeNew > 1,]
dat17.1 <- dat17[dat17$AgeNew > 1,]

head(dat16.1)
head(dat18)

ageRecode <- cbind(unique(dat18$Age), seq(60,90, by = 5))
ageRecode

dat18$AgeNew <- mapvalues(dat18$Age, from = ageRecode[,1], to = ageRecode[,2])
head(dat18)

## Modify format of data for 2016 and 2017 to match 2018

## 2016
head(dat16.1)
dat16.1m <- dat16.1[, c("AgeNew", "GeoNew", "Männer")]
names(dat16.1m)[3] <- "Death"
dat16.1m$Year <- 2016
dat16.1m$Sex <- 1

dat16.1f <- dat16.1[, c("AgeNew", "GeoNew", "Frauen")]
names(dat16.1f)[3] <- "Death"
dat16.1f$Year <- 2016
dat16.1f$Sex <- 2

dat16Long <- rbind(dat16.1m, dat16.1f)
head(dat16Long)

## 2017
dat17.1m <- dat17.1[, c("AgeNew", "GeoNew", "Männer")]
names(dat17.1m)[3] <- "Death"
dat17.1m$Year <- 2017
dat17.1m$Sex <- 1

dat17.1f <- dat17.1[, c("AgeNew", "GeoNew", "Frauen")]
names(dat17.1f)[3] <- "Death"
dat17.1f$Year <- 2017
dat17.1f$Sex <- 2

dat17Long <- rbind(dat17.1m, dat17.1f)
head(dat17Long)

## Put 2018 also into format
head(dat18)

## Recode sex
sexRecode <- cbind(c("m", "w"), c(1, 2))
sexRecode

dat18$SexNew <- mapvalues(dat18$Sex, from = sexRecode[,1], to = sexRecode[,2])
head(dat18)
head(dat17Long)

dat18Long <- dat18[, c("AgeNew", "GeoNum", "Death", "Year" , "SexNew")]
names(dat18Long) <- names(dat17Long)
head(dat18Long)


## Create Final death data set
datLong <- rbind(dat16Long, dat17Long, dat18Long)
head(datLong)
length(unique(datLong$GeoNew))


## Load Population Data

pop2015 <- read.csv("pop2015.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
head(pop2015)
tail(pop2015)

pop2016 <- read.csv("pop2016.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
head(pop2016)
tail(pop2016)

pop2017 <- read.csv("pop2017.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
head(pop2017)
tail(pop2017)

pop2018 <- read.csv("pop2018.csv", skip = 7, header = FALSE,
                    stringsAsFactors = FALSE, sep = ";", nrows=length(8:43047))
head(pop2018)
tail(pop2018)

names(pop2015) <- names(pop2016) <- names(pop2017) <- names(pop2018) <- c("Geo", "Name", "Age", "Total", "Male", "Female")

## Reduce to Kreise 
pop2015$GeoNew <- as.numeric(pop2015$Geo)
pop2015.1 <- pop2015[!is.na(pop2015$GeoNew),]
head(pop2015.1)

## Edit Hamburg and Berlin
pop2015.1$GeoNew[pop2015.1$GeoNew == 2] <- 2000
pop2015.1$GeoNew[pop2015.1$GeoNew == 11] <- 11000

## Also edit Osterode (03156) and Göettingen (03152)
## to newly created Kreis Göttingen (3159)
pop2015.1$GeoNew[pop2015.1$GeoNew == 3156] <- 3159
pop2015.1$GeoNew[pop2015.1$GeoNew == 3152] <- 3159


pop2015.2 <- pop2015.1[pop2015.1$GeoNew %in% link$GeoNew,]
miss2015 <- link$GeoNew[! link$GeoNew %in% pop2015.2$GeoNew]
length(unique(pop2015.2$GeoNew))
head(pop2015.2)
str(pop2015.2)

pop2015.2$Total <- as.numeric(pop2015.2$Total)
pop2015.2 <- pop2015.2[!is.na(pop2015.2$Total),]
noPop2015 <- pop2015.2[is.na(pop2015.2$Total),]



## Reduce to Kreise 2016
pop2016$GeoNew <- as.numeric(pop2016$Geo)
pop2016.1 <- pop2016[!is.na(pop2016$GeoNew),]
head(pop2016.1)

## Edit Hamburg and Berlin
pop2016.1$GeoNew[pop2016.1$GeoNew == 2] <- 2000
pop2016.1$GeoNew[pop2016.1$GeoNew == 11] <- 11000

pop2016.2 <- pop2016.1[pop2016.1$GeoNew %in% link$GeoNew,]
miss2016 <- link$GeoNew[! link$GeoNew %in% pop2016.2$GeoNew]
length(unique(pop2016.2$GeoNew))
head(pop2016.2)
str(pop2016.2)

pop2016.2$Total <- as.numeric(pop2016.2$Total)

noPop2016 <- pop2016.2[is.na(pop2016.2$Total),]

## Reduce to Kreise 2017
pop2017$GeoNew <- as.numeric(pop2017$Geo)
pop2017.1 <- pop2017[!is.na(pop2017$GeoNew),]
head(pop2017.1)

## Edit Hamburg and Berlin
pop2017.1$GeoNew[pop2017.1$GeoNew == 2] <- 2000
pop2017.1$GeoNew[pop2017.1$GeoNew == 11] <- 11000

pop2017.2 <- pop2017.1[pop2017.1$GeoNew %in% link$GeoNew,]
miss2017 <- link$GeoNew[! link$GeoNew %in% pop2017.2$GeoNew]
length(unique(pop2017.2$GeoNew))

pop2017.2$Total <- as.numeric(pop2017.2$Total)

noPop2017 <- pop2017.2[is.na(pop2017.2$Total),]

## Reduce to Kreise 2018
pop2018$GeoNew <- as.numeric(pop2018$Geo)
pop2018.1 <- pop2018[!is.na(pop2018$GeoNew),]
head(pop2018.1)

## Edit Hamburg and Berlin
pop2018.1$GeoNew[pop2018.1$GeoNew == 2] <- 2000
pop2018.1$GeoNew[pop2018.1$GeoNew == 11] <- 11000

pop2018.2 <- pop2018.1[pop2018.1$GeoNew %in% link$GeoNew,]
miss2018 <- link$GeoNew[! link$GeoNew %in% pop2018.2$GeoNew]
length(unique(pop2018.2$GeoNew))
head(pop2018.2)
str(pop2018.2)

pop2018.2$Total <- as.numeric(pop2018.2$Total)

noPop2018 <- pop2018.2[is.na(pop2018.2$Total),]


## Missing Population Data
unique(noPop2015$Name)
unique(noPop2016$Name)
unique(noPop2017$Name)
unique(noPop2018$Name)

length(unique(pop2015.2$GeoNew))
length(unique(pop2016.2$GeoNew))
length(unique(pop2017.2$GeoNew))
length(unique(pop2018.2$GeoNew))

## Reshape datasets and merge together
pop2015.2$Year <- 2015
pop2016.2$Year <- 2016
pop2017.2$Year <- 2017
pop2018.2$Year <- 2018

pop <- rbind(pop2015.2, pop2016.2, pop2017.2, pop2018.2)


popF <- pop[, c("Age", "Female", "GeoNew", "Year"),]
popM <- pop[, c("Age", "Male", "GeoNew", "Year"),]

popM$Sex <- 1
popF$Sex <- 2
head(popF)
names(popF) <- names(popM) <- c("Age", "Pop", "GeoNew", "Year", "Sex")
popLong <- rbind(popM, popF)

## Recode Age 
ageRecode <- cbind(unique(popLong$Age), c(rep(1, 60), rep(60, 5),
                                          rep(65, 5), rep(70, 5), 75, 80, 85, 90, 2))

popLong$AgeNew <- mapvalues(popLong$Age, from = ageRecode[,1], to = ageRecode[,2])

head(popLong)

popLongDeath <- popLong[popLong$AgeNew > 2,]
popLongTot <- popLong[popLong$AgeNew == 2,]

## write datafile for Total population (potentially for PD estimation)
write.table(popLongTot, file = "totalPopulation2015to18_20201027.txt",
            row.names = FALSE)

## Aggregate Data
head(popLongDeath)

str(popLongDeath)
popLongDeath$Pop <- as.numeric(popLongDeath$Pop)

popArray <- tapply(popLongDeath$Pop, INDEX = list(popLongDeath$AgeNew,
                                                  popLongDeath$Year,
                                                  popLongDeath$Sex,
                                                  popLongDeath$GeoNew), FUN = sum)

avePopArray <- apply(popArray, MARGIN = c(1,3,4), FUN = function(x) (x[-1]+x[-length(x)])/2)

avePop <- as.data.frame.table(avePopArray, stringsAsFactors = FALSE)
head(avePop)
names(avePop) <- c("Year", "Age", "Sex", "Geo", "Expo")

avePop$ID <- paste(avePop$Age, avePop$Year, avePop$Sex, avePop$Geo,
                   sep ="_")

datLong$ID <- paste(datLong$AgeNew, datLong$Year, datLong$Sex, datLong$GeoNew,
                    sep ="_")

## Merge Population Data with Death Data
avePopMerge <- avePop[,c("ID", "Expo")]
finalDataGer <- merge(datLong, avePopMerge, by = "ID")
dim(finalDataGer)
dim(datLong)
dim(avePopMerge)
head(finalDataGer)

## Link Area Size to Data
head(link)
linkArea <- link[,c("GeoNew", "fl17")]
names(linkArea)[2] <- "Area" 
finalDataGerArea <- merge(finalDataGer, linkArea, by = "GeoNew")
dim(finalDataGerArea)


## Calculate Total average population

head(popLongTot)
popLongTot$Pop <- as.numeric(popLongTot$Pop)
popTotArray <- tapply(popLongTot$Pop, INDEX = list(popLongTot$Year,
                                                   popLongTot$GeoNew), FUN = sum)
dim(popTotArray)
aveTot <- apply(popTotArray, MARGIN = 2, FUN =
                  function(x)
                    mean((x[-1]+x[-length(x)])/2))
length(aveTot)

finalDataGerArea$TotPop <- as.numeric(mapvalues(finalDataGerArea$GeoNew, from
                                                =names(aveTot), to = aveTot))
head(finalDataGerArea)
str(finalDataGerArea)

finalDataGerArea$PD <- finalDataGerArea$TotPop/finalDataGerArea$Area

## quantile(finalDataGerArea$PD, probs = seq(0, 1, by = 0.25))
## finalDataGerArea[finalDataGerArea$GeoNew == 8212,]

write.table(finalDataGerArea, file = "dataGermany20201103.txt",
            row.names = FALSE)





## Merge AGS to data for 2016 and 2017
