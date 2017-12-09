library(SPARQL)
library(dplyr)

data <- read.csv("data/Liste_des_parcs_et_jardins.csv", header = TRUE, sep = ";")

cleanData <- data %>%
  select(
    longitude = longitude,
    latitude = latitude
)

# Step 1 - Set up preliminaries and define query
# Define the data.gov endpoint
endpoint <- "http://services.data.gov/sparql"

# create query statement
query <-
  "PREFIX  ??: 
SELECT ?? AS longitude ?? AS latitude ?? AS name ?? AS adress ?? AS surface ?? AS description ?? AS image
"

# Step 2 - Use SPARQL package to submit query and save results to a data frame
qd <- SPARQL(endpoint,query)
df <- qd$results

