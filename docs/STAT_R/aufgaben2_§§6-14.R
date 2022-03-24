#12104.aufgaben 2, script s.40-107
#20220308(13.52)
# Die folgenden Aufgaben behandeln den Skript Seite 40 bis 107 §§6-14
# 1. Nennen Sie Unterschiede zwischen Matrix und Data.frame
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
#######################
# 5. Finden Sie mit der Funktion match eine einzelne Zeile der
# Matrix in dieser Liste. Achtung, diese Zeile muss vorher mit
# list eingelistet werden
match(l3,list(mat[1,]))
#match in listenpunkt 1
#######################
# 6. Wofür benötigt man arrays?
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
destatis_cred <- read_csv("~/boxHKW/21S/SPUND/R/destatis_cred.csv")
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
# 10. Vergleichender Boxplot mit BSR-Daten Gewicht in Mg mit HM (Hausmüll), APC (Abfallpresscontainer) und AC (Abfallcontainer) + kurze Interpretation der Graphik
# 11. Korrelationsmatrix der Eierdaten (ohne Hof) + Regression + Interpretation
# 12. Was ist der Unterschied zwischen Pixelgraphik und Vektorgraphik, erzeugen Sie bitte eine Vektorgraphik z.B. einen schönen Stern zum Fest oder Schmetterling oder eine Vektorgraphik Eurer Wahl.
# 13. Wie ist das Haushaltseinkommen in Deutschland verteilt? Auswertung, Graphik, Interpretation!
#   Spätester Abgabetermin ist der 20.3.
#########################################
#grep
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
unlist(lapply(x, mean))
bsr<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/bsrorg.csv",header=T)
dat<-bsr
dim(dat)
     

class(dat)                                     # ein Data.frame
dat[1,]
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
liste[[[1]]] #no
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
