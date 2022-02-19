#12077.statistik_R (barghoorn)
#20220218(17.38)
#20220219(23.00)
#lebendgeburten tabelle destatis:
"https://www-genesis.destatis.de/genesis/online?sequenz=tabelleErgebnis&selectionname=12612-0002#abreadcrumb"
#try for API fetch tables
#destatis webservices: https://www-genesis.destatis.de/genesis/online?Menu=Webservice#abreadcrumb
library(readr)
library(stringi)
library(readxl)

#import local destatis credentials
#destatis_cred <- read_csv("/Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")

src<-"https://www-genesis.destatis.de/genesisWS/rest/2020/find/find?username=IHRE_KENNUNG&password=IHR_PASSWORT&term=Lebendgeburten&category=all&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=ARBEITSLOS09&bereich=Alle&sprache=de"
src<-"https://www- 
  genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_KENNUNG&password=IHR_PASSWORT&selectionname=12612-0002&format=csv&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenAufbau&username=IHRE_KENNUNG&password=IHR_PASSWORT&namen=11111KE001&bereich=Alle&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&namen=11111KE001&bereich=Alle&format=csv&werte=true&metadaten=false&zusatz=false&startjahr=&endjahr=&zeitscheiben=1&inhalte=&regionalmerkmal=&regionalschluessel=&sachmerkmal=GES&sachschluessel=GESW&sachmerkmal2=NAT&sachschluessel2=NATA&sachmerkmal3=&sachschluessel3=&stand=01.01.1900&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/DownloadService_2010?method=TabellenDownload&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12411-0002&bereich=Alle&format=csv&komprimieren=false&transponieren=false&startjahr=1995&endjahr=2000&zeitscheiben=&regionalmerkmal=&regionalschluessel=&sachmerkmal=&sachschluessel=&sachmerkmal2=&sachschluessel2=&sachmerkmal3=&sachschluessel3=&auftrag=false&stand=&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&namen=21311BS003&bereich=Alle&format=csv&werte=true&metadaten=false&zusatz=false&startjahr=&endjahr=&zeitscheiben=1&inhalte=&regionalmerkmal=&regionalschluessel=&sachmerkmal=GES&sachschluessel=GESW&sachmerkmal2=NAT&sachschluessel2=NATA&sachmerkmal3=&sachschluessel3=&stand=01.01.1900&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&username=IHRE_KENNUNG&password=IHR_PASSWORT&term=berufsgruppen&bereich=Alle&format=csv&werte=true&&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/rest/2020/find/find?username=IHRE_KENNUNG&password=IHR_PASSWORT&term=Berufsgruppen&category=all&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Meine&format=csv&werte=true&&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=ARBEITSLOS09&bereich=Alle&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Alle&sprache=de"

# stri_detect(src,regex="IHRE_KENNUNG")
# stri_detect(src,regex="IHR_PASSWORT")
# #substitute kennung, pwd in request 
# stri_replace(src,destatis_cred$kennung,regex = "IHRE_KENNUNG")
# stri_replace(src,destatis_cred$pwd,regex = "IHR_PASSWORT")
# 
# #dt1<-eval(parse(src))
# dt2<-read.csv2(src)
# dt3<-read_xml(src)

#i give this one up. i manage no api fetch...
#work with static files
#import destatis dataset:
ns<-c(1:4,"year",6:12,"gender",14:15,"month","month_nm","all")
xcpt1 <- read.csv2("12612-0002_flatcpt.csv",sep = ";",col.names = ns, na="0")

#fuck sonderzeichen im datensatz!
stri_detect(xcpt1$gender,regex="\xe4")

geschlecht<-stri_replace(xcpt1$gender,"ae",regex = "\xe4")
monat<-stri_replace(xcpt1$month_nm,"ae",regex = "\xe4")
table2<-replace(xcpt1,13,values = geschlecht)
table3<-replace(table2,17,values = monat)

sumup<-function(df,gnd,jahr){
  yearxm<-subset(df,year==jahr&gender=="maennlich")
  yearxw<-subset(df,year==jahr&gender=="weiblich")
  ifelse(gnd=="m",return(sum(as.double(yearxm$all))),
  ifelse(gnd=="w",return(sum(as.double(yearxw$all))),"specify gender"))
}

c<-c(sumup(table3,"m",2019),sumup(table3,"w",2019))
d<-c(sumup(table3,"m",2020),sumup(table3,"w",2020))

ns<-c("maennlich","weiblich")
sum1920<-rbind("2019"=c,"2020"=d)
colnames(sum1920)<-ns


#import task dataset
geb<-read.csv2("geburten_d.csv")

geb<-rbind(geb,sum1920)
geb
#sum1920


##########

dim(geb)
mode(geb)
attributes(geb)
names(geb)
class(geb)
row.names(geb)
head(geb,3)
gew<-cbind(geb,apply(geb,1,sum))
dim(gew)
head(gew,3)
colnames(gew)[3]<-"all"
head(gew,3)
tabs<-function(x){
  gew<-cbind(x,apply(x,1,sum))
  colnames(gew)[3]<-"all"
  gew
}
mode(tabs)
tabs(geb)->e1
dim(e1)
sum((e1$maennlich))
####
tabs<-function(x) {
  gew<-cbind(x,apply(x,1,sum)) 
  m<-dim(x)                          # dimension(x)
  colnames(gew)[1+m[2]]<-"Gesamt"    # update je nach Spalten-zahl
  return(gew) }

tabss <-function(x) {
  gew<-cbind(x,apply(x,1,sum)) # Spaltensumme verketten
  colnames(gew)[3]<-"Gesamt"
  gew<-rbind(gew,colSums(gew))
  return(gew) }
m<-tabss(geb)
lastrow<-length(m$Gesamt)
(row.names(m)[lastrow]<-"sum")
print(m)

proz<-geb/apply(geb,1,sum)
dim(proz)
print(proz)
####
####
e1<-round(proz,3)
head(round(100*proz,1))
####
proztab <- function(x) {
  s<-apply(x,1,sum)         # Spaltensumme 
  p<-x/s                    # Tabelle / Spaltensumme
  p<-round((100*p),1)      # *100 gerundet auf eine Stelle
  p   
  print(p)# Ergebnis
}
## #hä?: hier (mit margin=1) wird schon die rowsum berechnet
# welchen sinn soll es haben, den prozentsatz eines jahres innerhalb
# von allen jahren zu betrachten? was wären denn 100%? das mean?
####################
proztab_q <- function(x) {
  s<-apply(x,2,sum)         # Spaltensumme
  p<-x/s                    # Tabelle / Spaltensumme
  p<-round((100*p),1)      # *100 gerundet auf eine Stelle
  p
  print(p)# Ergebnis

  }

proztab(geb)
proztab_q(geb)
#print(s)

print(proz_q<-geb/apply(geb,2,sum))
dim(proz_q)

proztab_q <- function(x) {
  s<-apply(x,2,sum/x)         # Spaltensumme
  p<-x/s                    # Tabelle / Spaltensumme
  p<-round((100*s),1)      # *100 gerundet auf eine Stelle
  p
  print(p)# Ergebnis
  
}
####
barplot(geb$maennlich)
barplot(geb$weiblich,col=2,add=TRUE)

