library(SPARQL)
library(dplyr)
#library(XML)
library(sqldf)
#library(stringdist)

source("helpers.R")

data <- read.csv("data/Liste_des_parcs_et_jardins.csv", header = TRUE, sep = ";")

cleanData <- data %>%
  select(
    longitude = longitude,
    latitude = latitude
)

# Step 1 - Set up preliminaries and define query
# Define the endpoints
endpoint <- "http://fr.dbpedia.org/sparql"
endpoint2 <- "http://192.168.0.17:8890/sparql/"

# create query statement
query <-
  "PREFIX dbpedia-fr:<http://fr.dbpedia.org/resource/>
PREFIX dbpedia-owl:<http://dbpedia.org/ontology/>
SELECT ?x WHERE {?x dbpedia-owl:wikiPageWikiLink dbpedia-fr:Liste_des_parcs_et_jardins_de_Paris .}"

query2 <-
  "PREFIX dbpedia-fr:<http://fr.dbpedia.org/resource/>
PREFIX dbpedia-owl:<http://dbpedia.org/ontology/>
PREFIX category-fr:<http://fr.dbpedia.org/resource/Catégorie:>
SELECT ?x WHERE { ?x dbpedia-owl:wikiPageWikiLink dbpedia-fr:Liste_des_espaces_verts_de_Paris .
}"
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_2e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_3e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_4e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_5e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_6e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_7e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_8e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_9e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_10e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_11e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_12e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_13e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_14e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_15e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_16e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_17e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_18e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_19e_arrondissement_de_Paris }
# UNION { ?x dbpedia-owl:wikiPageWikiLink category-fr:Espace_vert_du_20e_arrondissement_de_Paris }

query3 <-
  "PREFIX schema:<http://schema.org/>
SELECT ?name WHERE {?x schema:name ?name .}"


# Step 2 - Use SPARQL package to submit query and save results to a data frame
qd <- SPARQL(endpoint, query, format = 'xml')
qd2 <- SPARQL(endpoint, query2, format = 'xml')
qd3 <- SPARQL(endpoint2, query3, format = 'xml')

df = list()
for (e in qd$results)
  df = c(df,e)
for (e in qd2$results)
  df = c(df,e)

doublonstest <- which(duplicated(df))
df2 <- df[-doublonstest]

df3 = list()
for (e in qd3$results)
  df3 = c(df3, paste("<http://fr.dbpedia.org/resource/", gsub(" ", "_", e), ">", sep = ""))

# Linking
df3_matches1 = list()
for (e in seq(1, length(df3)))
  df3_matches1[e] = agrep(df3[e], df2)[1]
count = sum(!is.na(df3_matches1))

#Construction des equivalent et du string pour la requete sparql
string = ""
list_matches = list()
indexing =1
for (i in seq(1, length(df3_matches1))){
  e = df3_matches1[i][[1]]
  if (!is.na(e)){
    string = paste(string, df2[e], sep = ", ")
    list_matches[[indexing]] <- as.list(c(i, df2[e][[1]]))
    indexing = indexing + 1
  }
}  

for (i in seq(1, length(list_matches))){
  list_matches[[i]][[2]] = gsub(">", "", as.list(strsplit(list_matches[[i]][[2]], '/')[[1]])[5])
  list_matches[[i]][[2]] = gsub("<", "", list_matches[[i]][[2]])
  list_matches[[i]][[2]] = gsub(">", "", list_matches[[i]][[2]])
}


string = substr(string, 3, nchar(string))
string = paste("(", string, ")) }", sep = "")

test = as.list(strsplit(string, ",")[[1]])


query4 <-
  paste("PREFIX dbpedia-fr:<http://fr.dbpedia.org/resource/>
PREFIX dbpedia-owl:<http://dbpedia.org/ontology/>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
SELECT ?uri ?abstract ?image {
  ?uri dbpedia-owl:abstract ?abstract .
  ?uri foaf:depiction ?image
  FILTER (?uri IN ", string, sep = "")

query4WitoutImage <-
  paste("PREFIX dbpedia-fr:<http://fr.dbpedia.org/resource/>
PREFIX dbpedia-owl:<http://dbpedia.org/ontology/>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
SELECT ?uri ?abstract ?image {
  ?uri dbpedia-owl:abstract ?abstract .
  FILTER (?uri IN ", string, sep = "")

qd4 <- SPARQL(endpoint, query4, format = 'xml')
qd4WithoutImage <- SPARQL(endpoint, query4WitoutImage, format = 'xml')

df4 = qd4$results
df4WithoutImage = qd4WithoutImage$results

for (i in seq(1, nrow(df4)))
  df4[i, 'uri'] = gsub(">", "", as.list(strsplit(df4[i, 'uri'], '/')[[1]])[5])
df4$image = gsub("<", "", df4$image)
df4$image = gsub(">", "", df4$image)

for (i in seq(1, nrow(df4WithoutImage)))
  df4WithoutImage[i, 'uri'] = gsub(">", "", as.list(strsplit(df4WithoutImage[i, 'uri'], '/')[[1]])[5])
df4WithoutImage$image = gsub("<", "", df4WithoutImage$image)
df4WithoutImage$image = gsub(">", "", df4WithoutImage$image)


# df4$uri = utf8decode(df4$uri)
# df4$abstract = utf8decode(df4$abstract)

query5 <-
  "PREFIX schema:<http://schema.org/>
SELECT ?name ?address ?latitude ?longitude ?surface WHERE {
  ?x schema:name ?name .
  ?x schema:address ?address .
  ?x schema:latitude ?latitude .
  ?x schema:longitude ?longitude .
  ?x schema:surface ?surface .
  }"

qd5 <- SPARQL(endpoint2, query5, format = 'xml')
df5 = qd5$results
for (i in seq(1, nrow(df5)))
  df5[i, 'name'] = gsub(" ", "_", df5[i, 'name'])
# df5$name = utf8decode(df5$name)

for (i in seq(1, length(df5$name))){
  for (j in list_matches){
    if (i == j[[1]]){
      df5$name[i] <- j[[2]]
    }
  }
}

dataFinal <- sqldf(
  "SELECT DISTINCT name, address, latitude, longitude, surface, abstract, image FROM df5 JOIN df4 ON name = uri"
)

test2 = rbind(df4, df4WithoutImage)


## Previous technic for matching - doesn't work well

# matching <- stringdistmatrix(unlist(df3), unlist(df2))
# 
# df3_matches = list()
# for (i in seq(1, length(df3)))
#   min = matching[i,1]
#   index = 0;
#   for (j in seq(1, length(df2)))
#     if (matching[i,j] < min)
#       min = matching[i,j]
#       index = j
#   df3_matches[i] = df2[index]


