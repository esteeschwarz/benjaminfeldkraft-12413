#Daten:
srvr<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL&rScript"
srvd<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"
srvd2<-"https://www.soscisurvey.de/hux2022/?act=4eqU86Oc62EfGZm7PbHOjcmL"


srvx<-"https://www.soscisurvey.de/hux2022/?act=XFHFbeVybOP6o72MZIy0lUlR"
#pre<-eval(parse("https://www.soscisurvey.de/hux2022/?act=qM85F9YYrhjRhLB3gvub2AyF&rScript"))

eval(parse(srvr, encoding="UTF-8"))

tst<-as.data.frame(ds)
tst<-attr(ds$FR01,"*",TRUE)
tst<-read_csv(srvwd,sep=" ",skip=1)
read.table(srvx,header="TRUE")
library(readxl)
data_hux2022_2022_02_11_10_02 <- read_excel(srvx)
View(data_hux2022_2022_02_11_10_02)
srvwd<-"hux2022b.xlsx"
download.file(srvx,"hux2022b.csv")
srvdta<-read_csv("hux2022b.xlsx")

library(readr)
hux2022x <- read_table2(srvx)
read_tab
hux2022x$`"F401"`

