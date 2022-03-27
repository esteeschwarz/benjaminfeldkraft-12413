#12077.statistik_R (barghoorn)
#20220218(17.38)
#20220219(23.00)
#lebendgeburten tabelle destatis provided by tutor
"https://www-genesis.destatis.de/genesis/online?sequenz=tabelleErgebnis&selectionname=12612-0002#abreadcrumb"
#try for API fetch tables, pdf with sample request links:
#destatis webservices: https://www-genesis.destatis.de/genesis/online?Menu=Webservice#abreadcrumb
library(readr)
library(stringi)
library(xml2)

#########import local destatis credentials. diese sind in einer csv nach dem muster kennung,pwd abgelegt
#destatis_cred <- read_csv("~/Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")
########
#dir(".")
#src<-"https://www-genesis.destatis.de/genesisWS/web/DownloadService_2010?method=TabellenDownload&ken nung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=12411- 0002&bereich=Alle&format=csv&komprimieren=false&transponieren=false&startjahr=1995&endjahr=200 0&zeitscheiben=&regionalmerkmal=&regionalschluessel=&sachmerkmal=&sachschluessel=&sachmerkmal2 =&sachschluessel2=&sachmerkmal3=&sachschluessel3=&auftrag=false&stand=&sprache=de"
#src<-"https://www- genesis.destatis.de/genesisWS/rest/2020/find/find?username=IHRE_KENNU NG&password=IHR_PASSWORT&term=Abfall&category=all&pagelength=16&lang uage=de"
# src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Alle&sprache=de"
# #wks > this for database query for string ########
 src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Geburten&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
####################################################
#src<-"https://www-ge nesis.d estatis.de/ge nesisWS/web/Recher cheServic e_2010?method=MerkmalAuspraegunge nKatalog&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=BILHS1&auswahl=hs18*&kriterium=code&b ereich=Alle&listenLaenge=10&sprache=de"
# #wks: ausprägungen merkmal, xml_children: 6
# src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=MerkmalTabellenKata log&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=GES&auswahl=12*&bereich=Alle&listenLaenge= 15&sprache=de"
#######genesis source of geburten table
src<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12612-0002&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
lnk8<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12612-0100&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"

#######genesis erwerbstätigen
src_e<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"

#wks, spuckt in browser tabelle aus, now read this in R

#src<-lnk8

#############
#this to remove blanks in copied sample link and substitute kennung/pw in link provided by genesis, link to pdf with sample-links
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
########produces clean link with credentials in it
oxy<-riplx(src)
write_clip(oxy)
genesislx_geburten<-riplx(src) 
genesislx_erwerbstätigen<-riplx(src_e)
####create table of references
genesis_src<-list()
src0<-c("genesis code","content")
src1<-c("12612-002","geburten D geschlecht")
src2<-c("12211-9004","erwerbstätige")
src3<-c("12612-0005","alter mütter zur geburt")
src4<-c("12612-0100","geburten bundesländer geschlecht")
tabsrc<-(rbind(src1,src2,src3,src4));colnames(tabsrc)<-src0
write.csv2(tabsrc,"~/PRO/git/essais/docs/STAT_R/data/genesis_sources.csv")
########
# dt3<-read_xml(riplx()) #for request of xml sheets, catalogue requests...
# dt4<-read_csv2(riplx()) #no
#dt5 <- read.csv2(riplx(),sep = ";",skip=1) #for import regular csv table
# dt5 <- read.csv2(riplx(),sep = ";") #mind no skip rows import flat csv
#####request for genesis table
# dt5<- read.csv2(riplx(), sep = ";", na = c("...","-",".")) #this important to remove [...] NAs
#####
 #if read_delim instead, the variable names are bracketed complicate way in sonderzeichen, not plain as with read.csv2 
  #wks. yes!
#works
#######export table for static use w/o credentials
# write.csv2(dt5,"~/PRO/git/essais/docs/STAT_R/data/geburten_genesis.csv")
###############
 #now you should be able to import geburten table to apply aktualisierung der
 #barghoorn tabele
##############################################
 #run line for line from here with [command]+[return]
##############################################
####import static genesis datenset geburten 1950-2021
 dt6<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburten_genesis.csv")
 dt5<-dt6
 #### neuer ansatz: das untenstehende ab 1.1. hatte den vorlagedatensatz(barghoorn) mit den daten aus einer
 # über die genesis GUI heruntergeladenen tabelle aktualisiert. ich möchte nun nocheinmal versuchen, diese aktualisierung aus dem
 #API-fetched datensatz vorzunehmen
 #OBSOLET:in der tabelle sind leider die jahre nicht vollständig aufgeführt, sondern nur jeweils im monat januar
 #das jahr in der entsprechenden spalte. damit läszt sich
 #hat sich erledigt, das flat csv mit format=ffcsv entspricht dem format, das ich für die aktualisierung 
 #der vorlage benutzt habe.
 
#work with static files
#1.
 #1.1.import destatis dataset:
 #das folgende könnte obsolet werden, weil der API fetch doch funktioniert hat und in diesem dann datensatz
 #(die geburtentabelle von destatis) keine sonderzeichen drin sind im gegensatz zur heruntergeladenen
 #datei. im folgenden absatz habe ich versucht, die <ä>s wieder herzustellen bzw. durch <ae> zu ersetzen,
 #sie waren im datensatz so formatiert, dasz die zellen von R nicht vernünftig gelesen wurden.

#  ns<-c(1:4,"year",6:12,"gender",14:15,"month","month_nm","all")
# xcpt1 <- read.csv2("12612-0002_flatcpt.csv",sep = ";",col.names = ns, na="0")
# 
# #fuck sonderzeichen im datensatz!
# stri_detect(xcpt1$gender,regex="\xe4")
# 
# geschlecht<-stri_replace(xcpt1$gender,"ae",regex = "\xe4")
# monat<-stri_replace(xcpt1$month_nm,"ae",regex = "\xe4")
# table2<-replace(xcpt1,13,values = geschlecht)
# table3<-replace(table2,17,values = monat)

############ altes geladenes set mit korrekturen
# sumup<-function(df,gnd,jahr){
#   yearxm<-subset(df,year==jahr&gender=="maennlich")
#   yearxw<-subset(df,year==jahr&gender=="weiblich")
#   ifelse(gnd=="m",return(sum(as.double(yearxm$all))),
#   ifelse(gnd=="w",return(sum(as.double(yearxw$all))),"specify gender"))
# }

# c<-c(sumup(table3,"m",2019),sumup(table3,"w",2019))
# d<-c(sumup(table3,"m",2020),sumup(table3,"w",2020))
#########################################################

################
#2.neues set aus API fetch

#2.1.del mal rows
#@barghoorn apropos numerische variablen:
#im genesis datensatz erscheinen in den zeilen mit den zahlen für dezember 2021
#drei punkte (character, [...]). das macht eine declaration der spalte als numerisch
#unmöglich und erschwert die auswertung ungemein, da die gesamte spalte nur als type=character
#gelesen werden kann. warum dort von den verantwortlichen kein NA eingefügt wurde, ist mir schleierhaft...
#NT man kann beim import durch den parameter [na = "..."] sicherstellen, dasz solche
#fake NAs durch richtige ersetzt werden und die spalte dadurch als double integer
#interpretiert werden kann. man musz nur in der spalte erstmal suchen und finden, dasz
#fehlende werte (geburtenzahlen für monate) durch [...] dargestellt werden, bzw. in anderen
#genesis tabellen auch durch ["-"] oder ["."] / es lohnt sich also, beim csv import
#als parameter für die deklaration von NAs direkt: na = c("...","-",".") anzugeben.
###################
#2.2.sum genderspecified geburtenanzahl per year
###2.2.1. when dataset is imported without removing NA with import, sum generated out of double
# sumup<-function(df,gnd,jahr){
#   yearxm<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="männlich")
#   yearxw<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="weiblich")
#   ifelse(gnd=="m",return(sum(as.double(yearxm$BEV001__Lebendgeborene__Anzahl),na.rm=TRUE)),
#          ifelse(gnd=="w",return(sum(as.double(yearxw$BEV001__Lebendgeborene__Anzahl),na.rm=TRUE)),"specify gender"))
# }
###2.2.2.neu
sumup<-function(df,gnd,jahr){
  yearxm<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="männlich")
  yearxw<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="weiblich")
  ifelse(gnd=="m",return(sum(yearxm$BEV001__Lebendgeborene__Anzahl,na.rm=TRUE)),
         ifelse(gnd=="w",return(sum(yearxw$BEV001__Lebendgeborene__Anzahl,na.rm=TRUE)),"specify gender"))
}

# gnd<-"m"
#  yearxm<-subset(dt5,Zeit==2021&X2_Auspraegung_Label=="männlich")
#  ifelse(gnd=="m",(sum(as.double(yearxm$BEV001__Lebendgeborene__Anzahl),na.rm=TRUE)),
#         ifelse(gnd=="w",(sum(as.double(yearxw$BEV001__Lebendgeborene__Anzahl))),"specify gender"))
#  
# sum(as.double(yearxm$BEV001__Lebendgeborene__Anzahl),na.rm = TRUE)
#dt5$X2_Auspraegung_Label
#2.3.create new array with sums
c<-c(sumup(dt5,"m",2019),sumup(dt5,"w",2019))
d<-c(sumup(dt5,"m",2020),sumup(dt5,"w",2020))
e<-c(sumup(dt5,"m",2021),sumup(dt5,"w",2021))
####works
ns<-c("maennlich","weiblich")
#die columnnames müssen genauso wie in der vorlage(barghoorn) heiszen, sonst können die
#reihen nicht mit rbind kombiniert (also die neuen daten den alten angefügt) werden.
sum1920<-rbind("2019"=c,"2020"=d,"2021"=e)
colnames(sum1920)<-ns
#sum1920 beinhaltet jetzt die daten von 2019 und 2020, m/w

#2.4.import task barghoorn dataset
#static:
#geb<-read.csv2("PRO/git/essais/docs/STAT_R/data/geburten_d.csv")
#gith:
geb<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburten_d.csv")
##################################
#2.5.
#hier werden die geforderten aktualisierungen vorgenommen, bevor die funktionen laut script
#ausgeführt werden. also per <rbind> dem datensatz zwei zusätzliche reihen (2019,2020) hinzugefügt.
geb<-rbind(geb,sum1920)
geb
####works add years 2019-2021 to barghoorn dataset

##########
#3.
#das folgende sind die übertragungen, nachbauten aus dem seminarscript

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
###################################################
#EDA20(1) #3.9 erwerbstätige auswertung nach bundesland/jahr
#import datenset#fk where is erwerbstaet_land.txt?
#look in genesis myself
src_x2<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Erwerbstaetige&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
dt3<-read_xml(riplx(src_x2)) #for request of xml sheets, catalogue requests...
#export sheet to scan in editor
write_xml(dt3,"data/erwerbstaetige_genesis_q.xml")
#source:table 12211-9004
src_e<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
dt7<- read.csv2(riplx(src_e), 
                sep = ";", na = c("-",".","...")) #this important to remove [...] NAs
bnc_ns<-c("SH","HH","NS","BR","NR","HS","RE","BW","BA","SR","BE","BR","MV","SC","SA","TH")
sumovery<-function(y){
  bplot<-c(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==1])
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==2]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==3]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==4]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==5]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==6]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==7]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==8]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==9]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==10]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==11]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==12]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==13]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==14]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==15]))
           ,(sum(dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==16])))
}
s1<-sumovery()
hundert<-sum(s1)
barplot(s1) #well wks, but whole sum. now percentage
psh<-100/hundert*s1[1]
phundert<-c(100/hundert*s1[1:16])
barplot(phundert,1,1,bnc_ns,"v.H:1991-2020")
barplot(s1/29,1,1,bnc_ns) #same

s1mean<-c(mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==1]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==2]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==3]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==4]),
          mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==5]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==6]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==7]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==8]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==9]),
          mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==10]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==11]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==12]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==13]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==14]),
          mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==15]),mean( dt7$ERW002__Erwerbstaetige__1000[dt7$X1_Auspraegung_Code==16]))
barplot(s1mean,1,1,bnc_ns,"mean 1991-2020") #gleiche ansicht
##########
####§6.7,stundenaufgaben:
src8<-riplx(lnk8)#genesis: 12612-0100
dt8<-read.csv2(src8,sep=";",na = c("...","-","."))
write.csv2(dt8,"~/PRO/git/essais/docs/STAT_R/data/geburten_genesis12612-0100.csv")

ar1<-array(data=dt8$BEV001__Lebendgeborene__Anzahl,dim=c(16,5,2))
print(ar1)
ar1
ontop1<-1:(16*5)
ontop2<-ontop1+16*5
o1<-subset(dt8,dt8$X2_Auspraegung_Code=="GESM"&as.double(dt8$Zeit)>=2006&as.double(dt8$Zeit)<=2010)
o2<-subset(dt8,dt8$X2_Auspraegung_Code=="GESW"&as.double(dt8$Zeit)>=2006&as.double(dt8$Zeit)<=2010)
#ar1[[1:80]]<-o1
#ar1[81:160]<-o2
abind(matrix(o1$BEV001__Lebendgeborene__Anzahl,ncol=5),matrix(o2$BEV001__Lebendgeborene__Anzahl,ncol=5),along=2.5)
#wks.
#12126.aufgaben 3
tapply(Mg,list(tag, stunde, bsrkey, ort))
zn<-ceiling(505/100)
s1<-sample(15,15,T)
unsplit(10,c(1,1,1))

library(abind)
arr <- abind(matrix(1:14, ncol=3), matrix(20:34, ncol=3), along=3)
arr <- list(matrix(1:14, ncol=3), matrix(20:34, ncol=3), along=2.5)
arr[,,2][,1][3]

mat<-matrix(1:30,ncol=3)
a1<-function(x,y,z){x+mat[,2]+mat[,3]}
apply(mat,1,a1)
mat[,3]

x<-1:10
cave <- function(x, c1, c2) c(mean(x[c1]), mean(x[c2]))
apply(x, 1, cave,  c1 = "x1", c2 = c("x1","x2"))
get("*")
5%*%5
5*5
math
#12132.
require(stats)

(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <- c(2,1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x, y, z)[,ii] # shows the reordering (ties via 2nd & 3rd arg)

## Suppose we wanted descending order on y.
## A simple solution for numeric 'y' is
rbind(x, y, z)[, order(x, -y, z)]
## More generally we can make use of xtfrm
cy <- as.character(y)
rbind(x, y, z)[, order(x, -xtfrm(cy), z)]
## The radix sort supports multiple 'decreasing' values:
rbind(x, y, z)[, order(x, cy, z, decreasing = c(FALSE, TRUE, FALSE),
                       method="radix")]

## Sorting data frames:
dd <- transform(data.frame(x, y, z),
                z = factor(z, labels = LETTERS[9:1]))
## Either as above {for factor 'z' : using internal coding}:
dd[ order(x, -y, z), ]
## or along 1st column, ties along 2nd, ... *arbitrary* no.{columns}:
dd[ do.call(order, dd), ]
library(tidyverse)
gdf <- iris %>% group_by(Sepal.Length)
gdf %>% select(group_cols())
iris

df <- tibble(x = c(1,1,2,2))
group_vars(df)
group_rows(df)
group_data(df)
group_indices(df)

gf <- group_by(df, x)
group_vars(gf)
group_rows(gf)
group_data(gf)
group_indices(gf)

mtcars[with(mtcars, order(cyl, disp)), ]
umw<-um[with(um, order(GESCHL)),]

###
data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
data

# Insert
rows_insert(data, tibble(a = 4, b = "z"))
try(rows_insert(data, tibble(a = 3, b = "z")))

# Update
rows_update(data, tibble(a = 2:3, b = "z"))
rows_update(data, tibble(b = "z", a = 2:3), by = "a")

# Variants: patch and upsert
rows_patch(data, tibble(a = 2:3, b = "z"))
rows_upsert(data, tibble(a = 2:4, b = "z"))

# Delete and truncate
rows_delete(data, tibble(a = 2:3))
rows_delete(data, tibble(a = 2:3, b = "b"))
try(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")))

###
(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <- c(2,1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x, y, z)[,ii] # shows the reordering (ties via 2nd & 3rd arg)

## Suppose we wanted descending order on y.
## A simple solution for numeric 'y' is
rbind(x, y, z)[, order(x, -y, z)]

order(x <- c(1:5,1:3,10:14))
x      

###
# Multiple observations per row
anscombe
anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

