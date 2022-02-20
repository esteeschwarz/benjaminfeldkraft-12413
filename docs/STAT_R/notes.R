#12077.statistik_R (barghoorn)
#20220218(17.38)
#20220219(23.00)
#lebendgeburten tabelle destatis provided by tutor
"https://www-genesis.destatis.de/genesis/online?sequenz=tabelleErgebnis&selectionname=12612-0002#abreadcrumb"
#try for API fetch tables, pdf with sample request links
#destatis webservices: https://www-genesis.destatis.de/genesis/online?Menu=Webservice#abreadcrumb
library(readr)
library(stringi)
library(xml2)

#import local destatis credentials
destatis_cred <- read_csv("Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")
#dir(".")
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
#wks >
src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Geburten&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
src<-"https://www-ge nesis.d estatis.de/ge nesisWS/web/Recher cheServic e_2010?method=MerkmalAuspraegunge nKatalog&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=BILHS1&auswahl=hs18*&kriterium=code&b ereich=Alle&listenLaenge=10&sprache=de"
#wks: ausprägungen merkmal, xml_children: 6
src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=MerkmalTabellenKata log&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=GES&auswahl=12*&bereich=Alle&listenLaenge= 15&sprache=de"
src<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12612-0002&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=csv&job=false&stand=01.01.1970&language=de"
#wks, spuckt in browser tabelle aus, now read this

#############
#this to remove blanks and substitute kennung/pw in link provided by genesis, link to pdf with sample-links
#for API requests top of page
riplx<-function(){
 stri_detect(src,regex="IHRE_KENNUNG")
 stri_detect(src,regex="IHR_PASSWORT")
# #substitute kennung, pwd in request 
#blank<-"\s"
 k<-1
 x<-1
 for(k in 1:10){ 
 quest<-stri_replace(src,"",regex = "[:space:]");quest
  findspace<-stri_detect(quest,regex="[:space:]");findspace
ifelse(findspace==TRUE,p<-k+1,p<-k)
       for (p in 1:p){
         quest<-stri_replace(quest,"",regex = "[:space:]")}
   };quest
  quest<-stri_replace(quest,destatis_cred$kennung,regex = "IHRE_KENNUNG")
 quest_res<-stri_replace(quest,destatis_cred$pwd,regex = "IHR_PASSWORT")
 print(quest_res)
 return(quest_res)
 
}

riplx()
# #dt1<-eval(parse(src))
# dt2<-read.csv2(src)
 dt3<-read_xml(riplx()) #for request of xml sheets, catalogue requests...
 dt4<-read_csv2(riplx()) #no
 dt5 <- read.csv2(riplx(),sep = ";",skip = 1)
 #wks. yes!
 
#works
 rech1<-xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(dt3))))))
 xp1<-xml_text(rech1)
 print(xp1[145:160])
 xml_attr(rech1,"EVAL")
 rech1<-xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(dt3))))))
 #bei children=7 gibt es keine einträge mehr
 a<-(rech1[20])
 
#i give this one up. i manage no api fetch...
#work with static files
#import destatis dataset:
 #das folgende könnte obsolet werden, weil der API fetch doch funktioniert hat und in diesem dann datensatz
 #(die geburtentabelle von destatis) keine sonderzeichen drin sind im gegensatz zur heruntergeladenen
 #datei. im folgenden absatz habe ich versucht, die <ä>s wieder herzustellen bzw. durch <ae> zu ersetzen,
 #sie waren im datensatz so formatiert, dasz die zellen von R nicht vernünftig gelesen wurden.
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

