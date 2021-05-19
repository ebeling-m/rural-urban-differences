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

# Step 5: Unadjusted rural-urban differences
# Note that these estimation is computationally 
# intensive 
source("adjusted_rural-urban_mortDiff.R")

