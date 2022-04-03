#12126.aufgaben 3
#20220328(09.40)
#20220403(14.59)
#script source: "https://github.com/esteeschwarz/essais/blob/main/docs/STAT_R/aufgaben3.R"
###############
# 0. Bezug auf Frage 15 im Aufgabenset EINS: Ziehen sie wiederum Zufallsstichproben im Umfang 15 aus 15 Zahlen 
#und stellen Sie mit geeigneten Algorithmen fest, ob es Wiederholungen in der Stichprobe gibt. 
#Hierfür kann man auch eine eigene Funktion schreiben, muss aber nicht.
proof<-function(wh){
a<-c(1:15)
a1<-sample(a,15,replace=wh);a1
match(a,a1)
a2<-unique(a1)
stopifnot(length(a2)==length(a1))
cat("keine wiederholungen")
}
proof(F) #hier F oder T für ziehung mit zurücklegen anpassen, 
#bei TRUE ist eine fehlermeldung sehr wahrscheinlich, aber nicht notwendig.

#dasselbe invers um oben angedeutete wahrscheinlichkeit zu eruieren:
#ein abbruch der funktion bedeutet eine zahlenreihe, die auch mit
#zurücklegen keine doppelten ziehungen einer zahl aufweist.
#bei 
proof2<-function(wh,k){
  m<-0
  a<-c(1:15)
  a1<-sample(a,15,replace=wh);a1
  match(a,a1)
  a2<-unique(a1)
  ifelse(length(a2)==length(a1),m<-1,m<-0)
  
ifelse(m==1,text<-c("wiederholung nach",k,"durchläufen\n"),text<-c("keine wiederholungen in",k, "durchläufen\n"))
cat(text)
run1<-k-1
stopifnot(m==0)
#ifelse(m==1,return(k),F)

    }
for (k in 1:1000000){
run1<-proof2(T,k) 

}

############################
# 1. Erklären Sie kurz mit eigenen Worten, was eine Kreuztabelle (Pivot) ist. 
#Wie hängen die Klassifikationsvariable und die Dimensionen des Resultats zusammen? Was ist der Data-Cube?
#im grunde eine tabelle zweiter ordnung, die eine zusammenfassung, auswertung einer anderen tabelle darstellt,
#um zb. signifikanzen feststellen zu können. die daten selber werden nicht verändert,
#aber wenn sich die daten der tabelle erster ordnung ändern, hat das auswirkungen auf die
#tabelle 2. ordnung und die damit zusammenhängenden erkenntnisse. es werden nur
#zb. mittelwerte gegenübergestellt und daraus aussagen abgeleitet
############################

#   2. Erzeugen Sie bitte einen 4-dimensionalen Data-Cube (DC) mit den BSR-Daten. 
#Zweckmäßigerweise wird der Data.frame zuerst attached (attach(bsr)). 
#Wählen Sie für den Cube die Variable tag, zeit (Umrechnung in Stunden!), 
#bsrkey und P (Anliefer- ort). Aggregiert werden soll als Summe das Abfallgewicht Mg. 
#Kontrollieren Sie die dim(DC) und sum(Mg)==sum(DC,na.rm=T).

bsr<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/bsrorg.csv",header=T)

attach(bsr)
dim(bsr)
zt<-ceiling(zeit/100)
#drei<-table(tg,zt,Mg,bsrkey,P)
ein<-tapply(Mg, ceiling(Mg), sum)     # nur Gewichtsklasse 1
length(ein)
key<-unique(bsrkey)
ort<-unique(P)
zt<-unique(zt)
tg<-unique(tag)
#um4<-((mean(set[with(set,set$GEBJAHR==k),]$NETTO,na.rm = T)))
sums<-function(){
tg1<-0
for (k in 1:length(tg)){
tg1<-append(tg1,(sum(bsr[with(bsr,tag==tg[k]),]$Mg)),after = T)

}
zt1<-0
for (k in 1:length(zt)){
  zt1<-append(zt1,(sum(bsr[with(bsr,ceiling(zeit/100)==zt[k]),]$Mg)),after = T)
  
}
p1<-0
for (k in 1:length(ort)){
  p1<-append(p1,(sum(bsr[with(bsr,P==ort[k]),]$Mg)),after = T)
  
}
key1<-0
for (k in 1:length(key)){
  key1<-append(key1,(sum(bsr[with(bsr,bsrkey==key[k]),]$Mg)),after = T)

}
sum1<-list(tg1[2:length(tg1)],zt1[2:length(zt1)],p1[2:length(p1)],key1[2:length(key1)])
#return(sum1<-list(tg1,zt1,p1,key1))
}
sum1<-sums()


# 3 Schreiben Sie bitte zwei R-Programme für die alphabetische Verschlüsselung 
#nach Caesar, Kodierung und Entkodierung. 
#Dem Programm soll übergeben werden der Normaltext bzw. der kodierte Text 
#und jeweils ein Verschiebeschlüssel als Integerzahl. 
#Führen Sie bitte diese beiden Programme mit einem selbstgewählten Textbeispiel vor.
text<-"Unser Zeit-Datum Zahlensystem ist abhaengig vom Kalendersystem (Sonne und Mond)"
i###########
#die codierungs/encodierungs übungen sind in dem zweiten script, damit sie nicht in meinem githubfolder landen sollen...

#########
encode<-function (dat, code) 
{
  # dat: mit decode entcodierte Zahlen       # geschrieben 1993
  tmpcode <- rev(c(1, (cumprod(rev(code[-1])))))  # modify cod
  R <- NULL
  for(i in 1:length(code)) {
    R <- c(R, (dat %/% tmpcode[i])) # integer division
    dat <- dat %% tmpcode[i]        # modulo dat[i]
  }
  R <- as.vector(R)
  R 
}

decode<-function (dat, code) 
{
  # Daten als Vector                      Programm geschrieben 1993
  tmpcode <- rev(c(1, (cumprod(rev(code[-1])))))  # modify code
  R <- as.vector(tmpcode %*% (dat))       # Inner product
  R
}
####
#wieviel monate + tage
encode(450, c(12, 365/12))
#14,24
decode(c(3,2,15,30) ,c(365,24,60,60))
decode(c(12,13,5) ,c(365,53,7))
encode(8400,c(24,60,60))
8130/60/60/24
2*365+20*24
8400/365*12
y<-8400/365
y1<-floor(y)
y2<-abs(y-y1)
m0<-y2*30
m1<-floor(m0)
m1<-abs(m0-m1)
#rest monat
d0<-m1*24
d1<-floor(d0)
d2<-abs(d0-d1)
h0<-d2*60
h1<-floor(h0)
h2<-abs(h0-h1)
min0<-h2*60
ifelse(m1<1,mx<-0,mx<-floor(m))
#m2<-(y5*30)%/%24
#m2<-abs(m-mx)
#m3<-m1/24
#m4<-floor(m3)
#m5<-abs(m3-m4)
#m1<-m1*12
#m1<-floor(m1)
d0<-d*24
d1<-floor(d0)
d1<-abs(d0-d1)
ifelse(d1<1,dx<-0,dx<-floor(m))
h1<-d1*60
min<-floor(h1)
min<-abs(h1-h2)
d3<-d2*30
d4<-floor(d3)
d5<-abs(d3-d4)

h<-d5/24
ifelse(dh<1,hx<-0,hx<-floor(m))
h2<-d5%/%24
h2<-abs(h-hx)
h3<-h2*24
h4<-floor(h3)
h5<-abs(h3-h4)

#ds<-ceiling(y2/4)
h1<-d1*24
d1<-floor(d1)
d1<-d1-ds
h<-d1/60
h2<-d1%/%60
h1<-abs(h-h2)
h1<-h1*60
h1<-floor(h1)
decode(c(y2,y4,d4),c(365,12,30))
encode(8400,c(c(365,12,30)))
decode(c(23,4,0),c(365,12,30))
# 4. Jemand hat bis heute 8400 Tage gelebt, wie alt ist er in Jahren, Monaten und Tagen 
#und wann genau ist sein Geburtstag? 
#Denken Sie bitte an die Schalttage es sind 6. Verwenden Sie meine Programme encode und decode oder eigene.
# 5. Ihre Geheimzahl (PIN) für Ihr Konto bei der Studentenbank lautet 3981. Bitte verschlüsseln sie 
#diese mit dem Prim- zahlen-Key c(67,67,67). Man braucht einen dreistelligen Schlüssel, um auch 
#noch die maximal vierstellige Geheimzahl 9999 verschlüsseln zu können. Wie lautet die verschlüsselte 
#Geheimzahl und entschlüsseln Sie diese wieder zur Kontrolle, so dass wieder 3981 rauskommt. 
#Warum wird verschlüsselt?

#   6. Erzeugen Sie bitte ein zweiseitiges Stamm&Blatt (St&Bl) mit der Körpergröße aus den 
#Umfragedaten nach Geschlecht. Am besten vorher eine Zufallsstichprobe im Umfang 300 ziehen. 
#Bitte auch etwas Interpretation der Ergebnisse.
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
#####################
#habe versucht, das zu visualisieren, was wohl das ziel sein sollte, also ähnlich einer schmetterlingsgrafik dann..., schaff ich nicht
#####################
#####################

# 7. Erzeugen Sie mit IMAGE ein Bild. Zur Auswahl stehen noch die Datei Oliven.txt und Schnee.txt Oder eine
# eigene Bilddatei. Schnee.txt ist eine Fraktalgraphik, 
#sie wurde aus der Matrix > snow<- matrix(c(1,0,1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1), 5,5) 
#mit 4-facher Rekursion erzeugt. Oder erzeugen Sie mit dem Package spt ein Sierpinski-Dreieck. 
#Bitte auch unten den Exkurs beachten: Herstellung einer Bitmap aus einem Foto.
bitmap<-read.table("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/ICE.csv",header=T)
bild<-as.matrix(bitmap)
image(t(bild), col=gray.colors(16,rev=T))


# 8. Erzeugen Sie bitte eine Landkartengraphik mit R
library(mapdata)
map("worldHires", "Italy")
map.cities(country="Italy", minpop=1e5, capital=1)

# 9. Verbinden Sie sich über ODBC mit der Bibliotheks-Datenbank
# books.xls (Informatik) und erforschen Sie die
# Entwicklung der Programmiersprache BASIC, so wie sie in den Buchtiteln vorkommt, 
#indem Sie eine SQL-Abfrage (query) machen, in der LIKE '%_____%' vorkommt. 
#Wenn ODBC nicht geht, den R-Befehl grep nehmen. 
#Bitte auch Graphik und Interpretation anfertigen.
library(readxl)
books <- read_excel("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/books.xls")

basic<-grep("basic[^s]",books$TITEL,ignore.case = T) #REGEX !!!!!!!!!!!

books[basic,]

b1<-books[basic,]
attach(b1)
by<-unique(JAHR)
plot(b1$JAHR)
match(by[4],JAHR)
ay<-by
for (k in 1:length(by)){
#a<-(JAHR==by[1])
ay[k]<-sum(JAHR==by[k],na.rm = T)
}
by
basicy<-cbind(by,ay)
ay2<-rbind(ay)
colnames (ay2)<-by
ay3<-sort(ay2)
ay3
barplot(ay2,las=3)
#man müszte das noch sortieren nach jahren, was mir jetzt grad nicht gelingen will...
########################################
m<-scan("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/icebmp.pgm")
m1<-matrix(m,414)
image(t(m1))
m2<-as.matrix(rev(as.data.frame(m1)))
image(m2)

########################################
#ich breche die bearbeitung hier mal ab und schicke dir dann jetzt den letzten stand. ich werde noch daran arbeiten, denn viele sachen verlangen
#noch nach verbesserung, die ich da improvisiert habe.
#ich habe jdfs. sehr viel gelernt in dem kurs und bedanke mich für die ausführlichen vorlagen usw.,
#ich hoffe, das format, in dem ich das jeweils zur verfügung stelle, ist halbwegs lesbar und durchführbar, daran musz ich noch arbeiten.
#fokus lag für mich erstmal auf den aufgaben...


