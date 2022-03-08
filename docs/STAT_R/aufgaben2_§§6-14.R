#12104.aufgaben 2, script s.40-107
#20220308(13.52)
# Die folgenden Aufgaben behandeln den Skript Seite 40 bis 107 §§6-14
# 1. Nennen Sie Unterschiede zwischen Matrix und Data.frame
# 2. Was versteht man unter selektiver Zuweisung
# (selective assignment) im R? Zeigen Sie ein Beispiel
# 3. Erzeugen Sie eine beliebige Liste mit der Funktion split
# 4. Erzeugen Sie bitte eine Liste, die aus den Zeilen einer
# Matrix besteht
# 5. Finden Sie mit der Funktion match eine einzelne Zeile der
# Matrix in dieser Liste. Achtung, diese Zeile muss vorher mit
# list eingelistet werden
# 6. Wofür benötigt man arrays?
#   7. Erzeugen sie aus 2 Matrizen, welche mindestens in einer
# Dimension übereinstimmen, mit Hilfe der Funktion abind einen
# dreidimensionalen array. abind ist ein extra package.
# 8. Erzeugen Sie einen dreidimensionalen Data-Cube aus der Datei
# geburt_land.csv (Geburtsdaten nach Land) oder einen anderen
# Data-Cube ihrer Wahl.
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
bsr<-read.table("bsrorg.csv",header=T)
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
rev<-rev(order(dat[,4]))
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
