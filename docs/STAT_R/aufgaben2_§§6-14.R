#12104.aufgaben 2, script s.40-107
#20220308(13.52)
#20220328(08.56) abgabe; ich werde das nur noch etwas aufräumen...
#script source: "https://github.com/esteeschwarz/essais/blob/main/docs/STAT_R/aufgaben2_§§6-14.R"
# Die folgenden Aufgaben behandeln den Skript Seite 40 bis 107 §§6-14
# 1. Nennen Sie Unterschiede zwischen Matrix und Data.frame
#matrix elemente direkt adressierbar, s. 6.
##########
# 2. Was versteht man unter selektiver Zuweisung
# (selective assignment) im R? Zeigen Sie ein Beispiel
a<-c(1:10)
a
a[5]<-20;a
############
# 3. Erzeugen Sie eine beliebige Liste mit der Funktion split
b1<-c(1,1,1,2,2,2,3,3,3,4)
b<-(1:10)
#b1<-c(1:5);b1
b<-split(1:10,b1);b

###################
#try loop through groups
# bl<-length(b)
# b3<-b2
# gr<-3 #grouping factor (index)
# for (k in 1:gr){
# b4<-c(rep(c(rep(1,3)),3))
# }
# k<-0
# b4
# b5<-c(1:bl)
# b5
# c<-b
# c[1:gr1]<-1
# c[(gr1+1):(gr*2)]<-2
# c[gr*2]
# gr1<-gr/gr
# gr2<-gr1*gr-gr1*(gr-gr+gr1)
# gr3<-gr2+gr1
# grx<-c(1:gr)
# grx[grx[gr1]]
# grx2<-0
# for (k in 1:gr){
#   print(k)
#   grx2<-gr1*gr+gr1*k
#   
#   grx[k]<-grx2
# 
# grx
# c[grx]<-grx
# 
#   }
# 
# c[]
# c
# 
# rep(1,3)
############ no.

# 4. Erzeugen Sie bitte eine Liste, die aus den Zeilen einer
# Matrix besteht
mat<-matrix(1:20,4)
#dm<-dim(mat)
#dm[2]
#dm[1]
#m2<-mat[1:dm[1],]
#print(mat[1,]:mat[dm[1],])
#for (k in dm[1]){
#dm1<-dm[1]/dm[1]
#  l2<-list(rep(mat[k-k+dm1*k,],k))
#}
#l2
# give up.
l3<-list(mat[1,],mat[2,],mat[3,],mat[4,])
l3[2]
#or easy (bh script)
l4<-split(mat,row(mat))
l4
#proof xml:
write_xml(l4,"matlist.xml") #no write
write_csv2(l4,"matlist.csv")
#######################
# 5. Finden Sie mit der Funktion match eine einzelne Zeile der
# Matrix in dieser Liste. Achtung, diese Zeile muss vorher mit
# list eingelistet werden
match(l3,list(mat[1,]))
#match in listenpunkt 1
#######################
# 6. Wofür benötigt man arrays?
#zusammenfassung von daten gleichen typs (homogen) auch in mehreren dimensionen, die zb. dann auch mathematisch
#angesprochen werden können.
a1<-c(10:20)
x<-1
y<-2
a1[x+y] #das würde in einem data.frame nicht funktionieren
###############################
#   7. Erzeugen sie aus 2 Matrizen, welche mindestens in einer
# Dimension übereinstimmen, mit Hilfe der Funktion abind einen
# dreidimensionalen array. abind ist ein extra package.

library(abind)
abind(matrix(1:4, ncol=2), matrix(4:1, ncol=2),along=2.5)
dim(abind(a, a, along=3.5))                   
asum<-abind(a,apply(a,c(1,2),sum),along=3) ; dim(asum)
dimnames(asum)[[3]][3]<-"Gesamt"
dim(asum)
asum
###############
# 8. Erzeugen Sie einen dreidimensionalen Data-Cube aus der Datei
# geburt_land.csv (Geburtsdaten nach Land) oder einen anderen
# Data-Cube ihrer Wahl.
####§6.7,stundenaufgaben:
library(readr)
library(stringi)
#destatis_cred <- read_csv("~/boxHKW/21S/SPUND/R/destatis_cred.csv")
lnk8<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12612-0100&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
geb_gen<-("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburten_genesis.csv")
geb_bh<-("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburt_land.csv")
riplx<-function(src){
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
#with api fetch genesis geburten table, need local genesis credentials
#src8<-riplx(lnk8) #genesis: 12612-0100
#dt8<-read.csv2(src8,sep=";",na = c("...","-","."))

#static gith genesis table
dt8<-read.csv2(geb_gen,sep=";",na = c("...","-","."))


ar1<-array(data=dt8$BEV001__Lebendgeborene__Anzahl,dim=c(16,5,2))
print(ar1)
ar1
ontop1<-1:(16*5)
ontop2<-ontop1+16*5
o1<-subset(dt8,dt8$X2_Auspraegung_Code=="GESM"&as.double(dt8$Zeit)>=2006&as.double(dt8$Zeit)<=2010)
o2<-subset(dt8,dt8$X2_Auspraegung_Code=="GESW"&as.double(dt8$Zeit)>=2006&as.double(dt8$Zeit)<=2010)
#ar1[[1:80]]<-o1
#ar1[81:160]<-o2
o3<-abind(matrix(o1$BEV001__Lebendgeborene__Anzahl,ncol=5),matrix(o2$BEV001__Lebendgeborene__Anzahl,ncol=5),along=2.5)
o3
#wks.
####tabs3? why?
#asum<-abind(o1,o2,along=2.5)
#dim(asum)
#asum
#cube?
asum<-abind(o3,apply(o3,c(1,2),sum),along=3)
dim(asum)
asum
###
bula<-unlist(strsplit("BW BY BE BB HB HH HE MV NI NW RP SL SN ST SH TH", " "))                  
geburt<-read.csv2(geb_bh)
head(geburt)
dim(geburt)
is.numeric(as.matrix(geburt[,3:31]))
num<-geburt[,3:31]
dim(num)
bev.cube<-array(as.matrix(num), dim=c(3,16,29))
dim(bev.cube)
dimnames(bev.cube)[[1]]<-c("m","w","all")
dimnames(bev.cube)[[2]]<-bula
dimnames(bev.cube)[[3]]<-1990:2018
bev.cube[3,,1:10]
bev.cube[,,1]

# 9. Verbindung von R mit Datenbank durch ODBC, das R-package
# heißt RODBC. Machen Sie eine Auswertung am besten mit oder, wenn es bei Euch nicht gehen sollte, auch ohne ODBC. RODBC ist ein extra package. Ersatzweise kann man auch die Funktion grep im R anstelle von LIKE verwenden und die Abfragen bzw. Auswertung analog mit R ohne ODBC machen.
um<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/Umfrage.csv",sep=";",header=T)
um10000<-subset(um,um$NETTO>10000)
mean(um10000$NETTO)
mean(um$NETTO,na.rm = T)
median(um$NETTO,na.rm = T)
mean(um$ARBEITSSTD,na.rm = T)
median(um$ARBEITSSTD,na.rm = T)
mean(um$ZUFR,na.rm = T)
#note: mich würde interessieren, wo die zahlen herkommen:
#1300/40h/7.5 erscheint mir reichlich unrealistisch, es sei denn es handelt sich um 90% selbständige,
#wo die definition von arbeitszeit etwas unsicher wird...
#menschen, die bei 1500 netto 90 stunden arbeiten mit einer zufriedenheit von 9?
#menschen mit hochschulabschlusz, die für 1350! 84h arbeiten? wie wurde hier "arbeitszeit" definiert?
#gruppierung
umw<-subset(um,um$GESCHL=="WEIBLICH")
umm<-subset(um,um$GESCHL=="MAENNLICH")
mean(umm$NETTO,na.rm = T)
attach(um)
um1<-um[with(um, order(GESCHL,GEBJAHR)),]
um2<-mean(um[with(um, um$GESCHL=="MAENNLICH"),]$NETTO,na.rm = T)
#um2<-mean(um[with(um, um$GESCHL=="MAENNLICH"),]$NETTO,na.rm = T) #wks.
um2<-mean(um[with(um, um$GESCHL=="MAENNLICH"),]$NETTO,na.rm = T)
um3<-mean(um[with(um, um$GESCHL=="WEIBLICH"),]$NETTO,na.rm = T)
y<-unique(um$GEBJAHR)
um0<-cbind("year"=NA,"netto"=NA)

ummean<-function(set,y){
  um0<-cbind("year"=NA,"netto"=NA)
  for(k in y){

      um4<-((mean(set[with(set,set$GEBJAHR==k),]$NETTO,na.rm = T)))
print(k)
print(um4)
um5<-cbind("year"=k,"netto"=um4)
um0<-rbind(um0,um5)
  }
#  um0<-um0[2:length(um0[,1])]
  return(um0)
}

um8<-ummean(umw,y)
um10<-ummean(umm,y)
um13<-cbind(um8,um10[,2])
colnames(um13)<-c("year","netto W","netto M")
#sortieren
um14<-as.data.frame(um13)
arrange(um14,-desc(um14$year))#yes
plot(um14$`netto W`~um14$year)


# 10. Vergleichender Boxplot mit BSR-Daten Gewicht in Mg mit HM (Hausmüll), APC (Abfallpresscontainer) und AC (Abfallcontainer) + kurze Interpretation der Graphik
bsr<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/bsrorg.csv",header=T)
dat<-bsr
dim(dat)


class(dat)                                     # ein Data.frame
dat[1,]
dat$bsrkey
attach(dat)
bsrhm<-subset(dat,bsrkey=="HM");bsrhm
bsrapc<-subset(dat,bsrkey=="APC");bsrapc
bsrac<-subset(dat,bsrkey=="AC");bsrac
bsrbox<-cbind("HM"=bsrhm$Mg,"APC"=bsrapc$Mg,"AC"=bsrac$Mg)
boxplot(bsrbox,main="Müllentsorgung in Megatonnen")
#interpretation:
#mir ist nicht ganz klar, was ein boxplot hier darstellen soll, da die abhängigkeit der
#müllmengen/tag/stunde mir jdfs. keine aussage zuläszt, solange man das nicht näher analysiert
#und der mean als gegenüberstellung hier doch besser geeignet ist als der median?
barplot(bsrbox)
mnbsr<-c("HM"=mean(bsrbox[,1]),"APC"=mean(bsrbox[,2]),"AC"=mean(bsrbox[,3]))
barplot(mnbsr)
##############################

# 11. Korrelationsmatrix der Eierdaten (ohne Hof) + Regression + Interpretation
eier<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/eier.csv",sep=";",header=T)
dat<-eier
dim(dat)
summary(eier)
class(eier)
attach(eier)
corl<-cor(eier$Gewicht,eier$Laenge*eier$Breite)
corb<-cor(eier$Gewicht,eier$Breite)
product<-function(Gewicht,y,z){Gewicht/y*z}
coreier<-apply(eier,1,prod)/10000
median(coreier)
plot(Gewicht,coreier)
#############################################
#############################################

# 12. Was ist der Unterschied zwischen Pixelgraphik und Vektorgraphik, erzeugen Sie bitte eine Vektorgraphik z.B. einen schönen Stern zum Fest oder Schmetterling oder eine Vektorgraphik Eurer Wahl.
#load("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/vektor.RData")
tmp<-tempfile()
  download.file("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/vektor.RData",tmp)
load(tmp)
d<-stern
k<-3
#for (p in 1:10){

 # for (k in 1:5){
    
matplot(d[,1], d[,2], type="l", xlab="", ylab="", col="green")
par(new=TRUE)
matplot(0.3*d[,1], 0.3*d[,2], type="l", xlab="", xlim=c(0,90), ylim=c(0,70), col="blue")
title("Zwei simulierte Sterne")
polygon(d[,1], d[,2],col = k-1, lwd = 3, border = k+2)
#}

#}
# 13. Wie ist das Haushaltseinkommen in Deutschland verteilt? Auswertung, Graphik, Interpretation!
#src_x<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Haushaltseinkommen&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
#src_d<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
src_ha<-("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/haushaltseinkommen_genesis.csv")

dt5<- read.csv2(src_ha,sep = ";", na = c("-",".","..."))
attach(dt5)#local
#bsp
#um2<-mean(um[with(um, um$GESCHL=="MAENNLICH"),]$NETTO,na.rm = T)

ha500<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="unter 500 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha1000<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="500 bis unter 1000 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha1250<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="1000 bis unter 1250 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha1500<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="1250 bis unter 1500 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha2000<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="1500 bis unter 2000 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha2500<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="2000 bis unter 2500 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha3000<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="2500 bis unter 3000 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha3500<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="3000 bis unter 3500 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha4000<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="3500 bis unter 4000 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha5000<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="4000 bis unter 5000 EUR"),]$HSH020__Hauptwohnsitzhaushalte__1000)
ha5000plus<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="5000 EUR und mehr"),]$HSH020__Hauptwohnsitzhaushalte__1000)
haka<-sum(dt5[with(dt5,dt5$X3_Auspraegung_Label=="Ohne Angabe"),]$HSH020__Hauptwohnsitzhaushalte__1000)

hacpt<-cbind(ha500,ha1000,ha1250,ha1500,ha2000,ha2500,ha3000,ha3500,ha4000,ha5000,ha5000plus,haka)
colnames(hacpt)<-c("<500","<1000","<1250","<1500","<2000","<2500","<3000","<3500","<4000","<5000",">5000","k.A")
#hans<-rbind(500,1000,1250,1500,2000,2500,3000,3500,4000,5000,5001,NA)
#hatbl<-cbind(hans,hacpt)
#colnames(hatbl)<-c("Einkommen","Anzahl Tsd. Haushalte")
#barplot(hatbl[,2]~hatbl[,1],xlab = "Einkommen",ylab = "Anzahl Tsd. Haushalte",main="haushaltseinkommen 2020")
barplot(hacpt,ylab = "Anzahl Tsd. Haushalte",main="haushaltseinkommen 2020",las=3)


#########################################################
#   Spätester Abgabetermin ist der 20.3.
#########################################
