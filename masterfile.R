# This is the masterfile for the analysis of 
# rural-urban mortality differences in Germany and England & Wales

# Packages and Functions: 
# This code contains all packages and functions for the analysis 
source("packages_functions.r")

# Step 1: Prepare data for Germany for NUTS-3 regions
# Data has been obtained from Regionalstatistik.de, Eurostat, and 
# Individual Cause of death data (restricted access) prepared by 
# M. Mühlichen
source("data_preparation_Germany.R", encoding = "UTF-8")
# Central object: datGER

# Step 2: Prepare data for England & Wales for LAU-1 regions
# Data has been obtained from https://www.nomisweb.co.uk/sources
# and the census 2011
source("data_preparation_EnglandWales.R")
# Central object: datENW

# Step 3: Correlation LE and pop density Figure
source("scatter_LE_PD.R")

# Step 4: Unadjusted rural-urban differences
source("unadjusted_rural-urban_mortDiff.R")

# Step 5 and 6: Life expectancy adjusted rural-urban differences
# Life expectancy adjusted differences inclduing intermediate group
# Note that these estimation is computationally 
# intensive 
# Number of cores must be adjusted according to local computer 
source("adjusted_rural-urban_mortDiff_wIntermediate.R")

# Step 7: Estimation of differentials using alternative definitons of rural-urban
source("adjusted_rural-urban_mortDiff_alternativeRuralUrban_England.R")
source("adjusted_rural-urban_mortDiff_alternativeRuralUrban_Germany.R")

# Step 8: Sensitivity analysis using only selected regions 
# (England & Wales without London region)
# (Only West Germany)
source("adjusted_rural-urban_mortDiff_specificRegions.R")

# Step 9: Distribution of death rates by sex, country and age
source("distribution_deathrates_e60Cat.R")

# Step 10: Descriptive table distribution of districts 
source("table_distribution_districts.R", verbose = TRUE)