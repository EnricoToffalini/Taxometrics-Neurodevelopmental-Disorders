rm(list=ls())

# DataPreprocessing ------

## 1. Load libraries ------------------------------
library(readr)

## 2. Load dataframes------------------------------
PsycINFO=read_csv("Literature-review/data/raw/PsycINFO_MC.csv")
PubMed=read_csv("Literature-review/data/raw/PubMed_MC.csv")
Scopus=read_csv("Literature-review/data/raw/Scopus_MC.csv")
WOS=read_csv("Literature-review/data/raw/WebOfScience_MC.csv")

## 2.1. Combine dataframes------------------------
df_raw = rbind(PsycINFO,PubMed,Scopus,WOS)
write_csv(df_raw, file = "Literature-review/data/processed/df_all.csv")

## 3. Check duplicates-----------------------------
table(df_raw$Key)
### 3.1. Remove duplicates
df_1 = unique(df_raw)
table(df_1$Key) #check
write_csv(df_1, file = "Literature-review/data/processed/df_noDup.csv")


## 4. Check item types------------------------------
table(df_1$`Item Type`)
### 4.1 Select journalArticle 
df_2 = df_1[df_1$`Item Type` == "journalArticle", ]
write_csv(df_2, file = "Literature-review/data/processed/df_noDup_jA.csv")



