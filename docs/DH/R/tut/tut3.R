#12192.aufgabe 1
#20220508(16.06)
################
install.packages("readtext")

library(readtext)
setwd("~/boxHKW/21S/essais/gith/12057/essais/docs/DH/R/tut")

grimm<-readtext("data/maerchen/")

grimm.names<-grimm[,1] #grimm$doc_id
print(grimm.names)

colnames(grimm)[1]<-"dateiname"
colnames(grimm)

write.csv(grimm,"data/maerchentable.csv",row.names = F)
maerchen<-read.csv2("data/maerchentable.csv",header=T,sep=",")


grepl("Rumpel[a-zA-Z]{,9}",maerchen$dateiname)
maerchen$titel<-maerchen$dateiname
maerchen$jahr
library(stringi)
grep("18[2-4][0|1-9]",maerchen$dateiname,value = T)
#grep("")
maerchen$jahr<-gsub(maerchen$dateiname,"\\(|\\)")
