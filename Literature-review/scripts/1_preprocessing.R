rm(list=ls())

# DataPreprocessing ------

## 1. Load libraries ------------------------------
library(readr)

## 2. Load dataframes------------------------------
PsycINFO=read_csv("Literature-review/data/raw/PsycINFO.csv")
PubMed=read_csv("Literature-review/data/raw/PubMed.csv")
Scopus=read_csv("Literature-review/data/raw/Scopus.csv")
WoS=read_csv("Literature-review/data/raw/WoS.csv")

## 2.1. Combine dataframes------------------------
df_raw = rbind(PsycINFO,PubMed,Scopus,WoS)
write_csv(df_raw, file = "Literature-review/data/processed/df_all.csv")

## 3. Check duplicates-----------------------------
table(df_raw$Key)
### 3.1. Remove duplicates
df_nodup = unique(df_raw)
table(df_nodup$Key) #check

write_csv(df_nodup, file = "Literature-review/data/processed/df_nodup.csv")

# 3.2. Number of duplicates removed
nrow(df_raw)-nrow(df_nodup)

## 4. Check itemType, and select just journalArticle
table(df_nodup$`Item Type`)

df_journalArticle = df_nodup[df_nodup$`Item Type` == "journalArticle", ]
write_csv(df_journalArticle, file = "Literature-review/data/processed/df_ok.csv")



