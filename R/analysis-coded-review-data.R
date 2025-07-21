
# load libraries
library(readxl)

# import data
df = data.frame(read_excel("Literature-review/data/data_extraction.xlsx"))

# coders agreement
1 - mean(is.na(df$conclusion_agreement))
mean(df$conclusion_agreement, na.rm=T)
df$key = paste0(df$ID_article,df$ID_sample,df$subsample)
x = aggregate(df$admixture_agreement, by=list(df$key), FUN=mean, na.rm=T)
mean(x$x, na.rm=T)


