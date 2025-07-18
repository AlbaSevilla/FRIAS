Informal_groups<-read_excel("TablesToStandardize/Informal_groups.xlsx")
names(Informal_groups)
columnas<-Informal_groups[,c(4:27)]
Informal_groups<-Informal_groups%>%
mutate(Keywords=paste(Keywords,...4,...5,...6,...7,...8,...9,...10,...11,...12,...13,...14,...15,
...16,...17,...18,...19,...20,...21,...22,...23,...24,...25,...26,...27, sep = "; "))
columnas <- Informal_groups[,c(4:27)]
Informal_groups <- Informal_groups[,-c(4:27)]

Informal_groups$Keywords <- gsub("NA; ", "", Informal_groups$Keywords)
Informal_groups$Keywords <- gsub("NA", "", Informal_groups$Keywords)

write_xlsx(Informal_groups, "TablesToStandardize/Informal_groups2.xlsx")

library(dplyr)
View(Informal_groups)
