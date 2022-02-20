#12082.barghoorn aufgaben kapitel 1-5
#20220220(18.01)
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
# 18. Im R-System lassen sich die Datenobjekte mit den Funktionen length und dim abfragen. Nennen Sie mindestens 4 verschiedene Objekte und ihre Eigenschaften
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
#5.1.mean
mean(c)
lc<-length(c)
#dim(c)
c2<-cbind(c)
apply(c2,2,sum)/lc
#5.2.median
median(c)
#rank(c)
#md<-lc/2
#(c[md]+c[md+1])/2

#for the difference: outliers
c<-c(1:5,20:27,1000)
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
print(mdx3)

#5.3.standardabweichung
sd(c)
#5.4.kovarianz
d<-c(seq(1,20,2))
e<-c(seq(1,10,1))
cbind(d,e,d/e)
e2<-c(seq(1,40,4))
cbind(d,e,"pos"=d/e,e2,"neg"=d/e2)
