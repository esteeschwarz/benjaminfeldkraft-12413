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
#NT
#grep
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
unlist(lapply(x, mean))

e1<-rep(NULL,7)
e1[sapply(bsr[1,], is.character)]<-"character"
e1[sapply(bsr[1,], is.numeric)]<-"numerisch"
e1[sapply(bsr[1,], is.logical)]<-"logisch"
e1[sapply(bsr[1,], is.factor)]<-"faktor"
e1
mode(bsr[1,])
mode(bsr[,2])
dat[dim(dat)[1],4]
length(dat)
dim(dat)[1]
rev<-rev(dat[4]) #reverse   hä?
rev
rev<-rev(order(dat[2]))
d1<-c(1:10)
rev(d1)
d2<-(dat[4])
rev<-rev(d2) #no. läszt sich nicht umkehren die reihenfolge einer liste.
#d4<-c(d2[1:length(d2)])
d4[1]
#d5<-rev(unlist(d2))
d5<-rev(unlist(dat[[4]]))
###wks
##########
#zuweisungen
d1<-unlist(dat[2][dat[2]>2000])
d1
###
liste<-split (1:10, c(1,1,1,2,2,2,3,3,3,4))
liste
lapply(liste,length)
names(liste)<-c("eins","zwei","drei","vier")
unlist(lapply(liste,mean)) 
liste[1]
liste[1][1][2] #no
liste[1][[1]]
liste[[1]]
liste[[1]][3] #yes

mat<-matrix(1:25, ncol=5)
mat
split(mat, col(mat)) 
mat1<-split(mat, col(mat)) 
mat1$`1`
mat1$`2`
mat2<-lapply(mat1,mean)
mat2
library(abind)
mat3<-abind(mat2)
mat3
mat3[4]<-6
mat3[4]
mat4<-split(mat,row(mat))
mat4
mat5<-abind(lapply(mat4,mean))
mat5
liste[[1]]==4:6
(liste[[1]][1:5])
attributes(liste)
liste
match(liste[[1]],2)# nur so findet die 2
match(liste[[1:4]],2)#fail
match(unlist(liste[[1:3]]),2)
l1<-unlist(liste,recursive=T)
l1
as.list(l1)
liste[1,]
dim(liste)


#12133.aufgaben 3
sum1[,1]<-NA
sum1
dplyr_col_modify(sum1,cols = 0)


sum2<-  na.omit(sum1)
for (k in 1:length(sum1)){
  ifelse(match(sum1[[k]],0),"k.a",F)
  
  cat("NA at",k)
}
nas<-ifelse(match(sum1[[2]],0),5)
sum1[[2]][1]<-NA
getElement(sum1[[2]],match(sum1[[2]],0)) 
zt1
na.omit(sum1)
sum2<-list(sum1[1,],sum1[2,],sum1[3,],sum1[4,])
sum1[3,]
mean(sum2[[1]],na.rm = T)
dim(drei)
drei
Mg
###lotto:

pool<-c(1:49)
a<-sample(pool,6)
b<-sample(pool,6)

proof2<-function(wh,k){
  a<-c(1:49)
  a1<-sample(a,6);a1
  a3<-sample(a,6);a3
  ma<-match(a1,a3)
  #a2<-unique(a1)
  print(ma)
  mac<-c(NA,NA,NA,NA,NA,NA)
  macsum<-sum(!is.na(match(a1,a3)))
print(macsum)
    ifelse(macsum>=4,macsum,macsum)
    stopifnot(macsum<=4)
  cat("keine wiederholungen in",k, "durchläufen\n")
#sum(match(ma,mac),na.rm = T)
}
for (k in 1:100000){
  proof2(T,k) 
}

b<-c(1:10)
b1<-c(4:10)
sum(match(b,b1),na.rm = T)
!is.na(match(b,b1))<-1
sum(!is.na(match(b,b1)))

#12135. #######################
# lotto

###lotto:



pool<-c(1:49)

a<-sample(pool,6)

b<-sample(pool,6)

c1<-c(2,12,13,27,33,42)
c2<-c(7,15,23,27,33,42)
c3<-c(4,10,16,19,20,40)
c4<-c(5,11,16,17,22,46)

 #7/15/23/24/27/37
 #4/10/16/19/20/40
#c)5/11/16/17/22/46


proof2<-function(wh,k,s){

k<-k

  a<-c(1:49)

  a1<-sample(a,6)

  a3<-sample(a,6)

  c<-c(2,12,13,27,33,42)

  a3<-s

  ma<-match(a1,a3)

  #a2<-unique(a1)

  print(a1)

  mac<-c(NA,NA,NA,NA,NA,NA)

#ifelse(match(ma,mac)==T,map<-1,map<-0)

    stopifnot(sum(!is.na(match(a1,a3)))<=4)

  cat("keine wiederholungen in",k, "durchläufen\n")

  #print(k)



}
#for (p in 1:2){
  #cat("run",p,"\n")
for (k in 1:1000){

  proof2(T,k,c4)

}
#}

#12135.###
attach(bsr)
drei<-tapply(!is.na(Mg), list(tag, Laga, P), sum)
drei
###12136.
unsplit(dec,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
strReverse(c("abc", "Statistics"))
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
stri_join_list(stri_extract_all(send))
library(stringi)
stri_join(c('abc', '123', 'xyz'),'###', 1:6, sep=',')
stri_join(dec)
stri_join(1:13, letters, collapse='; ')
stri_join(c(dec[1:79]),collapse = "")
###12141.bh 3/6
#stammblatt stem&leaf
bsr<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/bsrorg.csv",header=T)

stich<-sample(15401, 500)
d1<-bsr$Mg[stich]    ; length(d1)

d2<-bsr$bsrkey[stich]    ; length(d2)

stem(d1[d2=="HM"])
stem(d1[d2=="IND"])
attach(bsr)
length(zeit[tag==110])
stem(zeit[tag==110])


umfr<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/Umfrage.csv",header=T)
stich<-sample(3471, 250)

reduct<-umfr[stich,]

attach(reduct)
library(aplpack) #benötigt X11
stem.leaf.backback(NETTO[GESCHL=="WEIBLICH"], NETTO[GESCHL=="MAENNLICH"], m=1)
#what is that?
x<-stem.leaf.backback(NETTO[GESCHL=="WEIBLICH"], NETTO[GESCHL=="MAENNLICH"], m=1)
y<-stem.leaf.backback(GRO[GESCHL=="WEIBLICH"], GRO[GESCHL=="MAENNLICH"], m=1)
detach(reduct)
stich<-sample(3471, 300)
reduct<-umfr[stich,]
attach(reduct)
y<-stem.leaf.backback(GRO[GESCHL=="WEIBLICH"], GRO[GESCHL=="MAENNLICH"], m=1)

x
plot(as.double(x[[10]]))
as.double(x[[10]])
library(stringi)
x1<-strsplit(x[[10]],"")
x2<-strsplit(x[[11]],"")
x11<-as.double(x1[[2]])*-1
plot(x11,x1[[1]])
plot(x1[[1]],x2[[1]])
plot(x1[[1]])
par(new=TRUE)
plot(x1[[2]],col=2)
par(new=TRUE)

plot(x1[[3]],col=3)
par(new=TRUE)

plot(x2[[1]])
par(new=TRUE)

plot(x2[[2]],col=2)

par(new=TRUE)
plot(x2[[3]],col=3)
par(new=TRUE)
plot(x2[[4]],col=4)
par(new=TRUE)
plot(x2[[5]],col=5)

title("eee")
x1

butterfl<-function(points) {
  # Schmetterlingskurve als X-Y Graphik
  # Points: Anzahl von Punkten
  theta<-pi*(1:points)/100            
  r<-x11^(theta)-x11*(4*theta)+(theta/12)^5   # Gleichung
  print(r)
  x<-r*(theta)                  # Konvertieren aus Polarkoordinaten
  y<-r*(theta)                  # Konvertieren aus Polarkoordinaten
  res<-cbind(x,y)
  res
}

d<-butterfl (2000)                   # 2000 Punkte
d<-d[with(d,!is.na),]
par(mfrow=c(1,1))
matplot(d[,1],d[,2], type="l", xlab="", ylab="", col="green")
title("Gr?ner digitaler Schmetterling mit 2000 Punkten")

#Um den Schmetterling mit Farben zu f?llen, wird das Programm polygon ben?tigt.
matplot(d[,1], d[,2], xlab="", ylab="", type='l')
polygon(d[,1], d[,2], col = "orange", lwd = 3, border = "green")
polygon(d[1:100,1], d[1:100,2], col = "blue", lwd = 3, border = "green")
title("Bunter digitaler Schmetterling mit 200 Punkten")
############
butterfl<-function(points) {
  # Schmetterlingskurve als X-Y Graphik
  # Points: Anzahl von Punkten
  theta<-pi*(0:points)/100            
  r<-exp(1)^cos(theta)-2*cos(4*theta)+sin(theta/12)^5   # Gleichung
  x<-r*sin(theta)                  # Konvertieren aus Polarkoordinaten
  y<-r*cos(theta)                  # Konvertieren aus Polarkoordinaten
  res<-cbind(x,y)
  res
}

d<-butterfl (2000)                   # 2000 Punkte
par(mfrow=c(1,1))
matplot(d[,1],d[,2], type="l", xlab="", ylab="", col="green")
title("Gr?ner digitaler Schmetterling mit 2000 Punkten")

#Um den Schmetterling mit Farben zu f?llen, wird das Programm polygon ben?tigt.
matplot(d[,1], d[,2], xlab="", ylab="", type='l')
polygon(d[,1], d[,2], col = "orange", lwd = 3, border = "green")
polygon(d[1:100,1], d[1:100,2], col = "blue", lwd = 3, border = "green")
title("Bunter digitaler Schmetterling mit 200 Punkten")


x<- c(0, 4, 7.5, 12)
y<- c(3, 2, -5, 3) 
#Es handelt sich um die prozentuale Abweichnung der M?lmenge vom Mittelwert, gemessen wurde drei mal, der erste Wert entspricht dem letzten. Gesch?tzt werde soll die sogenannte Jahresganglinie des M?lls.
#R-Spline-function:
 # spline(x, y = NULL, n = 3*length(x), method = "fmm",
  #       xmin = min(x), xmax = max(x), xout, ties = mean)

#Gezeichnet werden sollen die Messpunkte, Nulllinie (y=0), und die ermittelte Spline-Kurve.

#Mit xlim und ylim kann man den Graphikbereich (problemspace) formatieren.
#Monate
#[1]  0.0  4.0  7.5 12.0
#Proz_Abweichung
#[1]  3  2 -5  3

plot(x, y, ylim=c(-6,6), pch=19,  col="Red")
lines(a<-0:12,b<-rep(0,13))
lines(spline(x, y), col="Blue")
#title("Saisonale Schwankung der M?llmenge, gesch?tzt durch Splines")

#####
#12142.
zuf<-matrix(sample(400, 200), ncol=5, nrow=5)
image(zuf, col=c(1:4))
title("Rainbow Zufallsmuster")

bitmap<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/ICE.csv",header=T)
class(bitmap)
#[1] "data.frame"
bild<-as.matrix(bitmap)
dim(bild)
#[1] 217 216
bild[1:10,1:20]

unique(c(bild))               # funktion c vektorisiert die Matrix
#[1]  0  1  3  6  4  2  5  7  8  9 10 11 12 13 14 15  # hexadez
length(c(bild))               #  alle Datenpunkte
#[1] 46872
#Die Matrix bild muss transponiert werden, sonst ist das Bild quer.
image(t(bild), col=gray.colors(16,rev=T))
title("Der erste ICE in Michendorf 1993")
t(c(1,2,3,4,5))
###
ice<-scan("data/icebmpcsv.txt",what="character",sep=" ",allowEscapes = F)
i3<-(ice[1])
unique(ice)
i4[1:142]<-utf8ToInt(ice[1:142])
i5<-matrix(ice[])
i6<-as.matrix(apply(i5,1,utf8ToInt))
i6[1]
i7<-matrix(unlist(i6),ncol = i9)
i77<-matrix(unlist(i6))
image(i7,col=gray.colors(16,rev = T))
i8<-unique(i77)
i9<-sqrt(length(i77))
###
r<-scan("data/rainbmp.txt",what = "character",sep = " ")
r2<-scan("data/Rplot02.txt",what = "character",sep = " ")
r2==r
r5<-matrix(r2)
utf8ToInt (r5[4])
mr<-matrix(unlist(r2))
r6<-as.matrix(apply(r5,1,utf8ToInt))
r77<-matrix(unlist(r6))
r3<-matrix(unlist(r6),ncol = i9)
i77<-matrix(unlist(i6))
#############
install.packages("spt")
library(spt)
#help(spt)
(abc = spt(50,60))

#SPT(50,60,70) 

#Dimension =  1.604103   Viewport=( 0 , 0 , 94.13205 , 100 )

##Angles        x   y
# A     50  0.00000   0
# B     70 94.13205   0
# C     60 57.73503 100

plot(abc, iter=7)
#####
dreh <- function (dat) {
  x <- seq(0, pi, length= 30)
  y<-sin(x)
  z<-outer(y, y, "*") 
  for(i in 1:360) {
    persp(x, x, z, theta = 45, phi = i, expand = 0.5, col = rainbow(10))}
  for(i in 1:360) {
    persp(x, x, z, theta = i, phi = 45, expand = 0.5, col = rainbow(10))}
}
dreh(c(1:20))

###
library(mapdata)
map("worldHires", "Italy")
map.cities(country="Italy", minpop=1e5, capital=1)

#################
# Öffnen eines nicht zu komplizierten Bildes mit einem Bildbearbeitungsprogramm, ich verwende IrfanView.
# 2. Reduktion des Bildes auf nicht mehr 1000 x 1000 Bildpunkte (Pixel)
# 3. Umwandlung in Graustufen, Farben kommen später
# 4. Speichern des Bildes im Format Portable Gray Map PGM, rechts
# in dem Optionskasten Ascii Encoding auswählen
# 5. Es entsteht eine Textdatei, in der hauptsächlich Zahlen stehen, die SW-Graustufen-Nummern. In den ersten Zeilen
# Dieser Datei steht etwa das: # Created by IrfanView
#   600
# 255
# Vor
# Zahlen in der Datei
# 533 # Größe des Bildes in Pixeln # 255 Graustufen
# dem R-Import diese Zeilen löschen, danach sind nur noch
# 6. Im R soll das in meinem Beispiel eine Bild-Matrix mit 600 x 533 Pixeln geben, Import in R mit scan (Vektor)
7. mond <- scan("D:/Bilder/Bilder_TOP/mond2.pgm")
length(mond) # der Graustufenvektor, der Maimond [1] 319800
m1<-matrix(mond, nrow=533) > image(m1)
image(t(m1)) # Transponieren, das Bild wird gekippt
m2<-as.matrix(rev(as.data.frame(m1)))
# Bild waagerecht spiegeln
image(m2,col = rainbow(256)) # mit 256 Regenbogenfarben


#414x430
m<-scan("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/fadenlogo.pgm",skip=4)
ml<-length(m)
m1<-matrix(m,sqrt(ml))
image(t(m1))
m2<-as.matrix(rev(as.data.frame(m1)))
image(m2)
#########
#12143.
a<-c(rep(rep(c(130,255),length.out=2),rep(c(255,130),length.out=2),length.out))
a<-c(rep(rep(c(130,255),4),rep(c(255,130),4),length.out=64))
a<-rep(c(130,255),4)
b<-rep(c(255,130),4)
c<-c(rep(c(a,b),4))
c
sqrt(length(a))
b<-matrix(c,sqrt(length(c)))
#colnames(b)<-c(letters[1:8])
#row.names(b)<-c(1:8)
image(b)
####
x <- matrix(1:12,3,4)
y <- x+100
dim(abind(x,y,along=0))     # binds on new dimension before first
dim(abind(x,y,along=1))     # binds on first dimension
dim(abind(x,y,along=1.5))
dim(abind(x,y,along=2))
dim(abind(x,y,along=3))
dim(abind(x,y,rev.along=1)) # binds on last dimension
dim(abind(x,y,rev.along=0))
####
#length correction
a<-c(10,30,10,10)
b<-c(6,5,4,2)
c<-cbind(a,b)
d<-residuals(lm(b~a))
d[4]
e<-d+d[4]*-1
e
d
###
adcontrol<-function(set){
con1<-set[1,]
con1$lfd<-length(dta$lfd)+1
con1$participant<-"admin"
con1$tnid<-999
con1$gilt<-2
con1$group<-"control"
con1$group14<-"control"
con1$category<-"control"
con1$itemId<-"control"
con1$item<-"control"
con1$regionId<-"control"
con1$elapsedTime<-NA
con1$timeinterval<-300
con1$target<-0
con1$string<-"dies ist ein control string durchschnittlicher länge"
con1$char<-stri_count_boundaries(con1$string,"character")
con1$rt_corr<-NA
con1$speed<-NA
con1$adinterval<-NA
con1$wds<-stri_count_boundaries(con1$string)
con1$addwds<-NA
con1$proctbywd<-NA
con1$addproct<-NA
con1$proctbywdavg<-NA
con1$addproctbywds<-NA
con1$proctbychar<-NA
con1$addchar<-NA
con1$addproctbychar<-NA
con1$explique<-"control string zur festlegung der minimal RT"
return(rbind(set,con1))
}

as.function(alist(a = , b = 2, a+b))(1)
as.function(alist(a = , b = 2, a+b))(2)
as.function(alist(a = ,b=3,lmerun(a)))()

str2expression(set1)
print(str2expression("2+3"))
lmerun(as.expression(set1))
########################## THIS
eval(parse(text="2+3"))
############################
eval(parse(text=set1))
lmerun(set2)
set1



length(ex1 <- expression("1 + 0:9")) # 1
ex1
eval(ex1) 
set1
set1<-"dta,ti,1,c(0,0,0,1,em,vso),0"
paste("lmerun(",)
#as.formula(paste("lmerun(",set1,")"
           as.formula(paste("lmerun( ", set1,")"))
lme2<-    paste0("(",set1,")")
lmerun(as.formula(set1))
lme2.form.cpt.XvsO<- paste(lme2.form2.XvsO,"+",lme2.form2.rnd,"+(1+",lme2.form2.XvsO,"|participant)")
rtc<-"rtc ~ "
rtcc<-"rtc.1 ~ "
ti<-"timeinterval ~ "
ifelse(resp=="rtc",rt<-rtc,ifelse(resp=="rtcc",rt<-rtcc,rt<-ti))
(fmlRTCgr <- as.formula(paste("rtc ~ ", lme2.form.cpt)))
(fmlTIgr <- as.formula(paste("timeinterval ~ ", lme2.form.cpt)))
(fmlTIvs<-  as.formula(paste("timeinterval ~ ", lme2.form.cpt.XvsO)))
(fmlRTCvs <- as.formula(paste("rtc ~ ", lme2.form.cpt.XvsO)))

fmlRTCgr <- as.formula(paste("lmerun",lme2))

(fmlxgr <- as.formula(paste(rt, lme2.form.cpt)))
(fmlxvs <- as.formula(paste(rt, lme2.form.cpt.XvsO)))

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
eval(parse(text="mean(1:10)"))
##################################
lme2<-paste0("lmerun(",set1,")") #set1 = "lmerun(dta,ti,1,c(0,0,0,1,em,vso),0)"
lme2
eval(parse(text=lme2)) #wks.
##################################

sum3<-data.frame("eins"=1:10,"zwei"=11:20)
sum3$eins
w<-"eins"
v<-"zwei"
o<-"drei"
sum(sum3[o])
sum3[o]<-31:40
sum3

a<-rbind(c(1:10),c(3:9))
b<-matrix(1:20,4)
b
c<-matrix(1:30,5)
c
a<-list()
a$eins<-b
a$zwei<-c
b<-unlist(a)
b
a
xtabs(a)
as.data.frame(b)
d<-unlist(saveset)
e<-as.data.frame(d)
h<-matrix(d,ncol=1)
g<-as.data.frame(f)
g<-round(g,3)
sumset[[5]]
  sumset[[2]]
  
  240/30
3120/30
10

ens<-row.names(e)
ens1<-stri_count_boundaries(ens2,"character")
unique(ens1)
grep(pattern=stri_flatten(  as.character(setx[2,]),collapse=","),ens2)
ens3<-sub(pattern=stri_flatten(  as.character(setx[1:30,]),collapse=","),replacement=stri_flatten(  as.character(setx[2,]),collapse=","),ens2)
ens4<-matrix(ens3,ncol=1)
ens2<-stri_replace(ens,regex=" ",replacement = "")

stri_cmp('number100', 'number2')
stri_cmp('number100', 'number2', opts_collator=stri_opts_collator(numeric=TRUE))
c('a', 'b', 'c') %stri>=% 'b'
stri_coll("number10","2")
stri_locale_get()
stri_replace_last("nu1mber10",regex="1",replacement = "")
##12151.
chose<-c(0,0,0,1,sm,vso)
mnset<-dta_setx(set,chose[1],chose[2],chose[3],chose[4],chose[5],chose[6])

dta<-mnset
#attach(dta)
# chose<-c(t1,t2,t3,xo,g1,g2)
#chose[1]<-t1
#chose[2]<-
mnx<-mean(mnset$timeinterval,na.rm=T)

############ LOGIK
checkvsall<-function(set){
  stri_detect(set$category,regex="vs")
}

#wie mit setzung von x variablen, abfrage derselben zwei ergebnisse möglich sind auszugeben
#flagno<-1
chose<-c(0,0,0,0,em,lc)
flagset<-dta_setx(dta,c(0,0,0,1,em,lc),1)
flag<-chose[5]
flagall<-0
#c1<-flagset[with(flagset,category==flag&flagno==0|category!=flag&flagno==1&checkvsall(flagset)),]
#length(c1$category)
#mean(c1$timeinterval,na.rm=T)
#
#checkvsall(flagset)
#ifelse(flagall==0,c1<-flagset[with(flagset,category==flag),]c1<-flagset[with(flagset,category!=flag&flagall==1&checkvsall(flagset)),])
c1<-flagset[with(flagset,category==flag),]
length(c1$category)
mean(c1$timeinterval,na.rm=T)
#c2<-flagset[with(flagset,checkvsall(flagset)),]
ifelse(flagall==0,c2<-flagset[with(flagset,group==chose[6]),],
       c2<-flagset[with(flagset,checkvsall(flagset)),])
mn2<-mean(c2$timeinterval)
length(c2$category)
mean(c2$timeinterval,na.rm=T)
mn1<-mean(c1$timeinterval,na.rm=T)
mnx<-cbind(mn1,mn2)
rownames(mnx)<-"mean"
ifelse(flagall==1,col2<-"vsAll",col2<-chose[6])
colnames(mnx)<-c(chose[5],col2)
print(mnx)

#ausgabe 1
#ausgabe 2


for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("r", i, sep = ".")
  assign(nam, 1:i)
}
ls(pattern = "^r..$")
nam

####
## Fit sex-specific variances by constructing numeric dummy variables
## for sex and sex:age; in this case the estimated variance differences
## between groups in both intercept and slope are zero ...
data(Orthodont,package="nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
Orthodont$nsexage <- with(Orthodont, nsex*age)
Orthodont$groose[1:64]<-sample(100:150,64,replace = T)
Orthodont$groose[65:108]<-c(sample(100:150,64,replace = T))-10
sum3<-lmer(distance ~ age + (1|age)+(age|Subject) + (0+nsex|Subject) +
       (0 + nsexage|Subject) + (1+groose) +(0+Sex), data=Orthodont)
sum6<-lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
             (0 + nsexage|Subject) + (1+groose) +(0+Sex), data=Orthodont)

sum4<-lmer(distance ~ age + (1|age)+(age|Subject) + (0+nsex|Subject) +
             (0 + nsexage|Subject) +(0+Sex), data=Orthodont)
sum5<-lmer(distance ~ age + (1|age)+(age|Subject) + (0+nsex|Subject) +
             (0 + nsexage|Subject) +(0+Sex), data=Orthodont,offset = a2)
sum1<-lmer(distance ~ age + (1|age)+(age|Subject) + (0+nsex|Subject) +
             (0 + nsexage|Subject), data=Orthodont)
sum7<-lmer(timeinterval ~   group  + (1+char) +(0+item) + (1 + tnid) + (0+group | tnid),dtax) 

summary(sum7)
a1<-lm(distance~groose,Orthodont)
summary(a1)
a2<-residuals(a1)

mean(Orthodont$distance)
# 24*0.662 = estimate < age * fixed effect of age (15.88) near 16.76 (estimate)
# mean distance = 24.023

24*0.662
#16/0.66=24.24
#16.76111/0.66019=25.3883

#"In the model, we posited a main effect of Category (single vs. other) 
#and random effects of Participant and Item, along with a random slope of Category by Participant"
getmean(dta,c(0,0,0,1,sm,vso),1,1,ti)
sum1<-lmer(timeinterval ~  1 + category  + (1|item) + (1 | tnid) + (1 + category : tnid),dtax,offset=rtc) 
sum1<-lmer(timeinterval ~   category  + (1+char) +(1|item) + (1 | tnid) + (category | tnid),dtax) 

#sum1<-lmer(timeinterval ~  0 + group  + (1| item) + (1 + tnid) + (1 + group | tnid),dtax,offset=rtc) 
summary(sum1)


