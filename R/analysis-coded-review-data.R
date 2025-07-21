
# load libraries
library(readxl)
library(psych)

# import screening data
scr = read.csv("Literature-review/data/processed/df_ok_screening.csv",sep=";")
cohen.kappa(scr[,c("abs_coder1","abs_coder2")])
sum(scr$abs_coder1==scr$abs_coder2); sum(!is.na(scr$abs_coder1))
cohen.kappa(scr[,c("full_coder1","full_coder2")])
sum(scr$full_coder1==scr$full_coder2,na.rm=T); sum(!is.na(scr$full_coder1))

# import coded data
df = data.frame(read_excel("Literature-review/data/data_extraction.xlsx"))

# coders agreement
mean(!is.na(df$conclusion_agreement)); sum(!is.na(df$conclusion_agreement)); nrow(df)
mean(df$conclusion_agreement, na.rm=T)
df$conclusion_agreement
df$key = paste0(df$ID_article,df$ID_sample,df$subsample)
x = aggregate(df$admixture_agreement, by=list(df$key), FUN=mean, na.rm=T)
mean(x$x, na.rm=T)
