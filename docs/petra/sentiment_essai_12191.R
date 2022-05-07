#12191.sentiment essai
#petraKIP prepare
#20220507(08.56)
######################
src1<-"https://raw.githubusercontent.com/esteeschwarz/essais/main/docs/DH/data/guhl_benjaminbuch1.txt"
dta<-scan(src1,"character",sep="\n",encoding = "utf-8")
glimpse(dta)
write_clip(dta)
#seperate sentences
library(stringi)
space<-stri_detect_regex(dta,pattern ="\\.\\s")
dta2<-stri_replace_all(dta,regex ="\\.\\s",replacement = "#n#")
dta3<-stri_replace_all(dta2,regex ="\\.\\r",replacement = "#n#")
dta4<-stri_replace_all(dta3,regex ="#n#",replacement = "\n")
dta5<-stri_replace_all(dta4,regex ="[\\.,;\\-&:]",replacement = " ")
dta6<-stri_replace_all(dta5,regex ='"',replacement = "")
dta7<-stri_replace_all(dta6,regex ="\\s{2,}",replacement = " ")
write_clip(dta7)
#wks. results in sentences array.
## amz rezensionen
src2<-"https://raw.githubusercontent.com/esteeschwarz/essais/main/docs/DH/data/temp/amz_rez.csv"
dta<-read.csv2(src2,header = T)
