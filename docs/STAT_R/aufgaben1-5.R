#12082.barghoorn aufgaben kapitel 1-5
#20220220(18.01)
#src:https://github.com/esteeschwarz/essais/blob/main/docs/STAT_R/aufgaben1-5.R
###########################################################
#Beantworten Sie bitte die folgenden Fragen in ganzen Sätzen bzw. mit dem R-Code. 
#Stellen Sie sich bitte vor, dass Sie die Themenbereiche jemandem erklären sollen, 
#der (leider) keine Ahnung von Statistik oder R hat. Verwenden Sie bitte möglichst eigene Formulierungen.
#Skript Kapitel 1 bis 5
# 1. Aus welchen Wurzeln hat sich die Statistik entwickelt?
# 2. In welche Gebiete lässt sich Statistik einteilen?
# 3. Erstellen Sie eine kleine Tabelle und ergänzen Sie die im Skript
# genannten Skalen mit dem passenden Datentyp (nominal, etc.)
# 4. Welche Zeichen (Buchstaben, Ziffern, Symbole etc.) können beim
# Datentyp numerisch enthalten sein? Dies ist sehr wichtig bei
# Importroutinen für die Erkennung des Datentyps
# 5. Nennen Sie statistische Kenngrößen in Abhängigkeit vom Datentyp
# 6. Erklären Sie die Begriffe Häufigkeits- und Kontingenztabelle
# 7. Vorteil des Median gegenüber Mittelwert, praktisches Beispiel
# 8. Beschreiben sie die statistische Kenngröße Streuung verbal
# 9. Erklären Sie den Unterschied zwischen Kovarianz und dem
# Korrelationskoffizienten
# 10. Welche Vorteile haben Stichproben im Vergleich zu Totalerhebungen?
# 11. Nennen Sie Nachteile von Stichproben
# 12. Was ist die Repräsentativität einer Stichprobe und wie kann man
# diese erreichen?
# 13. Was ist eine einfache Zufallsauswahl?
# 14. Was ist eine geschichtete Stichprobe?
# 15. Ziehen Sie mit R zwei Zufallsstichproben aus, eine ohne und eine
# mit Zurücklegen
# 16. Erzeugen Sie die Matrix stich wie im Skript durch 4 einfache
# Zufallsstichproben der Datei umfrage.csv. Berechnen Sie den Mittelwert der Matrix stich und vergleichen sie diesen mit dem Mittelwert der 3471 Fälle der Umfragedaten.
# Wieviel beträgt die prozentuale Abweichung?
# 17. Welche Folgen können die NA-Fälle in den Umfragedaten oder allgemein haben?
##a: dasz variablenlängen unterschiedlich ausfallen und variablen deshalb nicht mehr vergleichbar/kombinierbar sind.
# 18. Im R-System lassen sich die Datenobjekte mit den Funktionen length und dim abfragen. Nennen Sie mindestens 4 verschiedene Objekte und ihre Eigenschaften
##a: ##a: array: kombination (c()) von gleichartigen (typ) werten in einer matrix, matrix (kombination von werten in spalten/reihen), list: mehrdimensionales datenarray, in dem columns ($) erzeugt werden wie, cube, table, vector (allgemein)
# 19. Warum hat man in der Informatik und in R den Datentyp Liste eingeführt? Vorteile?
# 20. Vergleichen Sie für Ihre Stichproben das Nettoeinkommen und die Köpergröße die Genauigkeit der einfachen und der nach Geschlecht geschichteten Zufallsauswahl
# 21. Ergänzen Sie die die Datei Geburten Deutschland ab 1950 um die Daten von 2019 (der link ist im Skript blau) und berechnen Sie mit ihrem selbstgeschriebenen Programm tabs die Summe von maennlich und weiblich in einer 3. Spalte GESAMT. Schreiben Sie ein Programm tabss, das auch die Spaltensummen als Zeile an eine beliebige Matrix unten anfügt und in dieser Summenzeile GESAMT erzeugt. Stellen Sie die Geburtenentwicklung von 1950 bis 2019 graphisch dar und interpretieren Sie bitte das Ergebnis
# 22. Bei den Phasen einer typischen Erhebung habe ich eine Aufgabe bewusst "vergessen". Leider wird diese in der Praxis auch oft vergessen. Was sollte man vor der abschließenden Auswertung unbedingt getan haben? Und welche Techniken kann man dazu anwenden?
# Martin Barghoorn FU-Berlin Digitale Datenanalyse und statistische Methoden mit Bitte die Antworten incl. R-Code in ein Textverarbeitungs-Dokument
# einfügen. Formate: doc, docx, pdf, odt oder txt
# Bei Gruppenarbeiten sollten alle Gruppenmitglieder angegeben sein. Das Dokument sollte nicht mehr als 10 Seiten haben und spätestens bis zum 6. März bei mir per Email abgegeben werden.
# Viel Erfolg wünscht Online-Dozent Martin B.
################################################################################
#3.tabelle erstellen
a<-c("eins","zwei","drei","vier","funf","sechs")
b<-c(1:6)
c<-c(seq(1.6,1.85,0.05))
ns<-c("nominal","ordinal(zensuren)","metrisch(grösze)")
tab1<-cbind(a,b,c)
colnames(tab1)<-ns;tab1
#######################
#4.zeichentypen numerischer variablen
#4.1.numerisch
#4.1.1.integer (ganzzahlen)
int<-c(1:6)
typeof(int)
#4.1.2.double integer (flieszkommazahlen)
dobl<-c(seq(1.6,1.85,0.05))
typeof(dobl)
#test:
char<-"drei"
char*int
char*dobl
char*char
#fail: nicht-numerisches argument für binären operator
typeof(int*int)
typeof(int*dobl)
#stays same
#antwort: in numerischen datentypen dürfen NUR zahlen vorkommen.
#5.statistische kenngröszen
c<-c(1:7)
#5.1.mean, numeric double
mean(c)
lc<-length(c)
#dim(c)
c2<-cbind(c)
apply(c2,2,sum)/lc
#5.2.median, numeric double
median(c)
#rank(c)
#md<-lc/2
#(c[md]+c[md+1])/2

#5.2.1.advantage median vs. mean: outliers, practical: durchschnittseinkommen
c<-c(1:7,20:27,1000)
mean(c)
median(c)
head(c)
lc<-length(c)
#lc2<-round(lc/2)

rank(c)
md<-(lc-lc/2)
mdcor<-lc-round(lc/2)
#limes function
k<-1
mdx1<-(c[md]+c[mdcor])/2
mdx2<-(c[md]+c[mdcor])/2+1
mdx3<-(mdx1+mdx2)/2
print(mdx3) #not working with both gerade/ungerade
medx<-function(c){
  print(mean(c))
  print(median(c))
  head(c)
  lc<-length(c)
  rank(c)
  rest<-(lc-(lc%/%2)*2)
pos0<-lc%/%2
pos1<-pos0+rest+1
print(medc<-(c[pos0]+c[pos1])/2)
}
nmed<-medx(c)

#5.3.standardabweichung, numeric double
sd(c)
#5.4.1.kovarianz
d<-c(seq(1,20,2))
e<-c(seq(1,10,1))
cbind(d,e,d/e)
e2<-c(seq(1,40,4))
cbind(d,e,"pos"=d/e,e2,"neg"=d/e2)
#5.4.2.korrelation
cbind(d,e,1/d*1/e,e2,1/d*1/e2)
#6.1.häufigkeitstabelle
f<-sample(1:100,10)
g<-letters[1:10]
h<-cbind(f)
row.names(h)<-g;h
#6.2.kontingenztabelle
n<-sample(1:100,10)
h<-cbind(f,n)
row.names(h)<-g;h
#7.1.median vs mean s.o. 5.2.1
#8.streuung
#werte können um einen mittelwert eng oder weit gestreut sein, d.h. eine durchschnittlich (standard)
#grosze oder kleine abweichung vom mittelwert aufweisen. je weiter die streuung einer wertesammlung,
#desto gröszer die relative standardabweichung vom mittelwert.
#9.kovarianz vs. korrelation s. 5.4.
#9.1.kov: skalenabhängige beziehung zwischen zwei variablen, wenn x+, dann y+
#9.2.kor: skalenunabhänige lineare beziehung zwischen zwei variablen, wert zwischen -1 und 1
#10.stichprobe vs totalerhebung
#kleines datenvolumen zu erheben und auszuwerten, ggf. (meist) randomisiert
#11.nachteile stichprobe
#repräsentativität nicht gesichert, stark abhängig von der auswahl der sample
#12.repräsentativität
#stichprobe soll getreues abbild der grundgesamtheit sein
#14.
#3 einkommensschichten
a<-c(1000:2000)
b<-c(2000:10000)
c<-c(10000:100000)
#zufälliges ziehen aus schichten
sa<-sample(a,100)
sb<-sample(b,100)
sc<-sample(c,100)
grundgesamtheit<-gg<-c(a,b,c)
sample_geschichtet<-s_g<-c(sa,sb,sc)
tab1<-cbind(sa,sb,sc)
ns<-c("1000-2000","2000-10000","10000-100000")
colnames(tab1)<-ns
####
mean(gg)
median(gg)
sample_ungeschichtet<-s_u<-sample(gg,300)
mean(s_u)
median(s_u)
####wesentlich realistischere mittelwerte
mean(s_g)
median(s_g)
#15.
a<-c(1:10)
b<-sample(a,4);b
c<-sample(a,10);c
d<-sample(a,10,replace=TRUE);d #as you can see, mit zurücklegen können werte mehrmals gezogen werden
######
#16.
dat<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/Umfrage.csv")
dim(dat)
mean(dat$NETTO,na.rm=TRUE)
median(dat$NETTO,na.rm=TRUE)
sum(!is.na(dat$NETTO))
sum(is.na(dat$NETTO))
sx<-function(s,p){
s<-dat$NETTO[sample(length(dat$NETTO),length(dat$NETTO)*(p/100))]
}
###
p<-10 #for 10%sample
stich<-rbind(NULL,summary(sx(s,p),na.rm=TRUE))
#s3<-dat$NETTO[sample(length(dat$NETTO),length(dat$NETTO)/10)]
stich<-rbind(stich,summary(sx(s,p),na.rm=TRUE))
stich<-rbind(stich,summary(sx(s,p),na.rm=TRUE))
stich<-rbind(stich,summary(sx(s,p),na.rm=TRUE))
dim(stich)
(stich[,"Mean"])
stich[,"Median"]
###
#und wie soll man die sd eines samples berechnen, das nicht mehr verfügbar ist?
#also nochmal
s1<-sx(s,p)
s2<-sx(s,p)
s3<-sx(s,p)
s4<-sx(s,p)
stich<-rbind(NULL,summary(s1))
stich<-rbind(stich,summary(s2))
stich<-rbind(stich,summary(s3))
stich<-rbind(stich,summary(s4))
stich
dim(stich)
(stich[,"Mean"])
stich[,"Median"]
sd1<-rbind(sd(s1,na.rm = TRUE),sd(s2,na.rm = TRUE),sd(s3,na.rm = TRUE),sd(s4,na.rm = TRUE))
colnames(sd1)<-"standardabweichung/samples"
sd1
###########################################
#aufgabe 21.
####21.1.import static genesis datenset geburten 1950-2021
dt6<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburten_genesis.csv")
dt5<-dt6
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
#21.2.sum genderspecified geburtenanzahl per year
###21.2.2.neu
sumup<-function(df,gnd,jahr){
  yearxm<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="männlich")
  yearxw<-subset(df,Zeit==jahr&X2_Auspraegung_Label=="weiblich")
  ifelse(gnd=="m",return(sum(yearxm$BEV001__Lebendgeborene__Anzahl,na.rm=TRUE)),
         ifelse(gnd=="w",return(sum(yearxw$BEV001__Lebendgeborene__Anzahl,na.rm=TRUE)),"specify gender"))
}

#21.3.create new array with sums
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

#21.4.import task barghoorn dataset
#static:
#geb<-read.csv2("PRO/git/essais/docs/STAT_R/data/geburten_d.csv")
#gith:
geb<-read.csv2("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/geburten_d.csv")
##################################
#21.5.
#hier werden die geforderten aktualisierungen vorgenommen, bevor die funktionen laut script
#ausgeführt werden. also per <rbind> dem datensatz zwei zusätzliche reihen (2019,2020) hinzugefügt.
geb<-rbind(geb,sum1920)
geb
####works add years 2019-2021 to barghoorn dataset

##########
#3.
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