
################################################################

# load libraries

library(readxl)
library(psych)
library(dplyr)
library(stringr)

################################################################

# import data

scr = read.csv("Literature-review/data/processed/df_ok_screening.csv",sep=";")
df = data.frame(read_excel("Literature-review/data/data_extraction.xlsx"))

################################################################

# coders agreement

# agreement on screening data
cohen.kappa(scr[,c("abs_coder1","abs_coder2")])
sum(scr$abs_coder1==scr$abs_coder2); sum(!is.na(scr$abs_coder1))
cohen.kappa(scr[,c("full_coder1","full_coder2")])
sum(scr$full_coder1==scr$full_coder2,na.rm=T); sum(!is.na(scr$full_coder1))

# agreement on coded data
mean(!is.na(df$conclusion_agreement)); sum(!is.na(df$conclusion_agreement)); nrow(df)
mean(df$conclusion_agreement, na.rm=T)
df$conclusion_agreement
df$key = paste0(df$ID_article,df$ID_sample,df$subsample)
x = aggregate(df$admixture_agreement, by=list(df$key), FUN=mean, na.rm=T)
mean(x$x, na.rm=T)

################################################################

# statistics on coded data

# total number of taxometric analyses, min and max per article
nrow(df)
tb=table(df$ID_article); min(tb); max(tb)

# median sample size across all analyses
median(df$Sample_size)

# median sample size by article (averaged)
median((df |> group_by(ID_article) |> summarize(avg = mean(Sample_size)))$avg)

# number of indicators per analysis
min(df$Indicators_number, na.rm = T)
max(df$Indicators_number, na.rm = T)
median(df$Indicators_number,na.rm=T)

# methods used
grepl("MAMBAC",df$Taxometric_methods_used)
sc = str_count(df$Taxometric_methods_used,";")+1; min(sc); median(sc); max(sc); mean(sc>1)

# taxonic conclusions
sum(df$Taxonic_conclusion)
sum(grepl("ASD", df$Target_disorder) & df$Taxonic_conclusion == 1) # total, concerning ASD
round(100 * sum(grepl("ASD", df$Target_disorder) & df$Taxonic_conclusion == 1) / sum(grepl("ASD", df$Target_disorder)), 1) # average, concerning ASD

# artificial admixture matching taxonic conclusions
sum(df$Artificial_admixture)
round(100 * mean(df$Taxonic_conclusion[df$Artificial_admixture == 1]), 1) # percentage of studies reaching a taxonic conclusion, with artificial admixture
round(100 * mean(df$Taxonic_conclusion[df$Artificial_admixture == 0]), 1) # percentage of studies reaching a taxonic conclusion, without artificial admixture

# 

################################################################


