library(ggplot2)

cpt<-X12612_0002_flatcpt
sum(cpt$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
stri_detect(cpt,regex="...")
dt6<-stri_replace(cpt,0,regex = "...")
type_convert(cpt,na="...")
typeof(cpt$BEV001__Lebendgeborene__Anzahl)

df <- data.frame(
  x = as.character(runif(10)),
  y = as.character(sample(10)),
  stringsAsFactors = FALSE
)
str(df2)
str(type_convert(df))

df <- data.frame(x = c("...", "10","5"), stringsAsFactors = FALSE)
apply(df,1,sum(str(type_convert(df,na="...")),na.rm=TRUE))
apply(str(type_convert(df,na="...")),2,sum)
df
sum(as.double(df2$x))
typeof(df2)
# Type convert can be used to infer types from an entire dataset

# first read the data as character
data <- read_csv(readr_example("mtcars.csv"),
                 col_types = list(.default = col_character())
)
str(data)
# Then convert it with type_convert
type_convert(data)

X12612_0002_flatcpt <- read_delim("PRO/git/essais/docs/STAT_R/data/12612-0002_flatcpt.csv", 
                                  delim = ";", escape_double = FALSE, na = "...", 
                                  trim_ws = TRUE)

sum(dt5$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
typeof(dt5$BEV001__Lebendgeborene__Anzahl)
#################
### get userinput how
# #fun <- function() {
#   ANSWER <- readline("Are you a satisfied R user? ")
#   ## a better version would check the answer less cursorily, and
#   ## perhaps re-prompt
#   if (substr(ANSWER, 1, 1) == "n")
#     cat("This is impossible.  YOU LIED!\n")
#   else
#     cat("I knew it.\n")
# }
# if(interactive()) fun()

fun2 <- function() {
  v1 <- as.matrix(readline("computate: V1 > "))
  v2<- (readline("computate: V2 > "))
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
#  if (substr(ANSWER, 1, 1) == "n")
   # cat("This is impossible.  YOU LIED!\n")
 # else
#    c(v1,v2)
   # print(2*v3)
  print(as.data.frame(v1))
  read.csv(v1,skip=1)
# fun3(as.data.frame(v1))   
    }
if(interactive()) fun2()

fun3<-function(a,b,c,d,e){
  print(a+b+c+d+e)
}


inp()
e1<-data.frame(10,10,10)
e1<-new_list(20)
e3<-c(seq(1,20,2))
(e1)
e3<-
e2<-new_list(10)
e1$a<-e3
sum(e2$a)
data.frame(v1)


#4.mixed model on genesis data
library(lme4)
dt5form<-as.formula(dt5$BEV001__Lebendgeborene__Anzahl~dt5$X2_Auspraegung_Code+(1|dt5$X3_Auspraegung_Code))
lmer(data=dt5,dt5form,na.exclude)
lmer
median(dt5$BEV001__Lebendgeborene__Anzahl,na.rm = TRUE)
na.action()
length(dt5$X2_Merkmal_Code)
length(dt5$X2_Auspraegung_Code)
length(dt5$BEV001__Lebendgeborene__Anzahl)
ncol(dt5)
#########
library(readr)
library(stringi)
library(xml2)

#import local destatis credentials. diese sind in einer csv nach dem muster kennung,pwd abgelegt
destatis_cred <- read_csv("~/boxHKW/21S/SPUND/R/destatis_cred.csv")

###destatis sample links:
# src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Alle&sprache=de"
# #wks >
#for xml request of keywords
src_x<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Erwerbstaetige&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
# src<-"https://www-ge nesis.d estatis.de/ge nesisWS/web/Recher cheServic e_2010?method=MerkmalAuspraegunge nKatalog&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=BILHS1&auswahl=hs18*&kriterium=code&b ereich=Alle&listenLaenge=10&sprache=de"
# #wks: ausprägungen merkmal, xml_children: 6
# src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=MerkmalTabellenKata log&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=GES&auswahl=12*&bereich=Alle&listenLaenge= 15&sprache=de"
###for csv data request
src_d<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"


#this to remove blanks in copied sample link and substitute kennung/pw in link provided by genesis, link to pdf with sample-links
#for API requests top of page
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

riplx() #produces clean link with credentials in it
 dt3<-read_xml(riplx(src_x)) #for request of xml sheets, catalogue requests...
#export sheet to read in editor
 write_xml(dt3,"data/erwerbstaetige_genesis_q.xml")
#get:<kurztext>Bevölkerung, Erwerbstätige, Erwerbslose, Erwerbspersonen,
# Nichterwerbspersonen: Bundesländer, Jahre (bis 2019)</kurztext>12211-9004
  # dt4<-read_csv2(riplx()) #no
#dt5 <- read.csv2(riplx(),sep = ";",skip=1) #for import regular csv table
# dt5 <- read.csv2(riplx(),sep = ";") #mind no skip rows import flat csv
 src_d<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
 
 dt5<- read.csv2(riplx(src_d), 
                sep = ";", na = c("-",".","...")) #this important to remove [...] NAs
 write.csv2(dt5,"data/erwerbstaetige_genesis.csv")
 #import as csv wks. now what was the task again?
 #3.9.entwicklung im zeitverlauf nach bundesländern
 #col X1auspr.label=bundesland, ERW002=erwerbstätige
 sum(dt5$ERW002__Erwerbstaetige__1000)
 #numeric works
 sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Label=="Berlin"&dt5$Zeit=="04/1991"])
 #wks
 bnd<-unique(dt5$X1_Auspraegung_Label)
 mnt<-unique(dt5$Zeit)
 #wks
 #loop sum through monat/bundesland
 y<-1;ey<-2;land<-1
 mnt[sy:ey]
 sumloop<-function(land,y){
   sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==land&dt5$Zeit==mnt[y]])
   
 }
 #for sum under condition (bundesland[1:16],year[1:29])
 sumloop(1,1) #wich is senseless because theres only one value bundesland/year
 #also task: nur abbilden des verlaufs bundesland~years
 #####
 #plot
 #better take land x1 ausprägung code, numbers 1:16
 SH<-(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==land&dt5$Zeit==mnt[y]])
 y<-1#nope
 #subset
 SH1<-dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==land]
 bnc_in<-c(SH1<-1,HH2<-2,NS3<-3,BR4<-4,NR5<-5,HS6<-6,RE7<-7,BW8<-8,BA9<-9,
 SR10<-10,BE11<-11,BR12<-12,MV13<-13,SC14<-14,SA15<-15,TH16<-16)
 bnc<-c(SH1,HH2,NS3,BR4,NR5,HS6,RE7,BW8,BA9,
        SR10,BE11,BR12,MV13,SC14,SA15,TH16)
 bnc_ns<-c("SH","HH","NS","BR","NR","HS","RE","BW","BA","SR","BE","BR","MV","SC","SA","TH")
 
 sumpery<-function(y){
 bplot<-c(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==1&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==2&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==3&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==4&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==5&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==6&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==7&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==8&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==9&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==10&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==11&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==12&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==13&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==14&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==15&dt5$Zeit==mnt[y]]
 ,dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==16&dt5$Zeit==mnt[y]])
 }
 #col_y<-(sumpery(1:29))
 1991+29
 barplot(sumpery(1:29),1,1,bnc_ns,"1991-2020") #what exactly is he doing?
# barplot(sumpery(1):sumpery(10),col=1:10)
 #barplot(bplot,col=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
 #bplot<-sort(bplot)
 #plot(bplot[1:16],col=1:16,type="s",)
 #df<-data.frame(bplot)
 print (bplot)
 #try with sum over years percentage of whole
 sumovery<-function(y){
   bplot<-c(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==1])
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==2]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==3]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==4]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==5]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==6]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==7]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==8]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==9]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==10]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==11]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==12]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==13]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==14]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==15]))
            ,(sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==16])))
 }
 #dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==
 s1<-sumovery()
 hundert<-sum(s1)
 barplot(s1) #well wks, but whole sum. now percentage
 barplot(s1/29,1,1,bnc_ns)
 psh<-100/hundert*s1[1]
 phundert<-c(100/hundert*s1[1:16])
 barplot(phundert,1,1,bnc_ns,"v.H:1991-2020")
 
 s1mean<-c(mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==1]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==2]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==3]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==4]),
           mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==5]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==6]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==7]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==8]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==9]),
           mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==10]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==11]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==12]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==13]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==14]),
           mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==15]),mean( dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Code==16]))
 barplot(s1mean,1,1,bnc_ns,"mean 1991-2020") #gleiche ansicht
 
 HH2<-19
 ######
#rech1<-xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(dt3))))))
# #bei children=7 gibt es keine einträge mehr
 #(rech1[1:20])
#
write_xml(dt3,"genesis_cat.xml")

dt51<-subset(dt5[!is.na(dt5$BEV001__Lebendgeborene__Anzahl),])
sum(dt51$BEV001__Lebendgeborene__Anzahl)
max<-max(dt51$BEV001__Lebendgeborene__Anzahl)
print(row.names(max(dt51$BEV001__Lebendgeborene__Anzahl)))
###this! > row.names(dt5[dt51$BEV001__Lebendgeborene__Anzahl==max,])

maxrow<-row.names(dt5[dt51$BEV001__Lebendgeborene__Anzahl==max,])
#print(dt51[,maxrow])
     dt51[1586,]      
#####interessant: peak bei 30jahren, erstes kind, 2020 mit 31628 geburten
#md<-median(dt51$BEV001__Lebendgeborene__Anzahl)     
#mdrow<-row.names(dt5[dt51$BEV001__Lebendgeborene__Anzahl==md,])
#dt51[mdrow,]          
     
     
#barghoorn §4.9
dat<-     read.csv2("data/Umfrage.csv")
ld<-length(dat$ID)
tab<-cbind(a<-colSums(is.na(dat)),round(100*a/ld))  # tab

colnames(tab)<-c("Antwort abs","Anwort rel") ; tab
red<-subset(dat[!is.na(dat$NETTO),])  # bereinigt um na-Fälle
dim(red)
gg<-mean(red$NETTO)
tapply(red$NETTO, list(red$GESCHL), FUN=mean)
tapply(red$NETTO, list(red$GESCHL), var)
tapply(red$NETTO, list(red$GESCHL), sd)
s1<-dim(red)[1]
s<-sample(s1,s1*0.02) ; length(s)
head(s)
simple<-mean(red$NETTO[s]) ; simple
strata<-tapply(red$NETTO[s], list(red$GESCHL[s]), mean); strata
mean(strata)
c(simple, mean(strata))/gg
###2.
red<-subset(dat[!is.na(dat$GRO),])  ; dim(red)
gg<-mean(red$GRO)
s1<-dim(red)[1]
s<-sample(s1,s1*0.02) ; head(s)               
strata<-tapply(red$GRO[s], list(red$GESCHL[s]), mean); strata
simple<- mean(red$GRO[s]) ; simple
c(simple, mean(strata))/gg              # Vergleich gg
1-c(simple, mean(strata))/gg

###6
bsr<-read.table("data/bsrorg.csv", header=TRUE)
is.list(bsr[1,])
mode(bsr[1,])

sauber<-c("IMI", "ATA", "PERSIL")
menge<-c(50,100,200)
names(menge)<-sauber ; menge                 
sum(menge)
summary(menge)
menge[rev(1:3)]                        
menge1<-menge                          # Kopie von menge1 erzeu-gen
menge1[2:3]<-menge1[2:3]*5 ; menge1

menge1[menge1 == 50]<- 1000 ; menge1  # Zuweisung logische Ab-frage

menge1["IMI"]<- 2000 ; menge1         # nur label IMI

menge1[]<-555 ; menge1

liste<-split (1:10, c(1,1,1,2,2,2,3,3,3,4))
names(liste)<-c("IMI", "ATA", "PERSIL", "FEWA");liste  

lapply(liste,length)
lapply(liste,max)                              
unlist(lapply(liste,mean))  
sapply(liste, mean)
length(liste[1])
length(liste[[1]])
liste[[1]][3]<-7
liste[[1]]
liste[[4]]<-1:10                            
liste[4]
tabs<-
  function(x) {
    gew<-cbind(x,apply(x,1,sum)) 
    m<-dim(x)                          # dimension(x)
    colnames(gew)[1+m[2]]<-"Gesamt"    # update je nach Spaltenzahl
    gew}
liste[4]<-list(tabs)

mat<-matrix(1:25, ncol=5)
mat
split(mat,col(mat))
split(mat, row(mat))                    
liste["ATA"]
liste[names(liste)=="ATA"]
match(4:6,liste)
match(list(4:6), liste)
match(liste,list(4:6,7:9))
liste[!is.na(match(liste,list(4:6,7:9)))]
listdata<-split(mat, row(mat))
listdata<-c(listdata, zweite<-listdata[2]) ; length(listdata)
names(listdata)<-c("EINS","ZWEI","DREI","VIER","FUENF","SECHS")
listdata                   
match(listdata, zweite)   
listdata[!is.na(match(listdata, zweite))]
###
a <- array(data=1:24, dim=c(4,3,2))
dimnames(a)=list(letters[1:4],LETTERS[1:3],c("Mann","Frau"))
dimnames(a)
      
dim(a) ; dim(aperm(a, c(2,1,3)))   
aperm(a, c(2,1,3))                 
apply(a,c(1,2),sum)
dim(apply(a,c(1,2),sum))              
apply(a,c(1,2),mean)                     
cbind(a[,,1],a[,,2], apply(a,c(1,2),mean))
sweep(a, c(1,2), apply(a, c(1,2), mean), "-")    

####################
## §§ 6
library(abind)
abind(matrix(1:4, ncol=2), matrix(4:1, ncol=2),along=2.5)
dim(abind(a, a, along=3.5))                   
asum<-abind(a,apply(a,c(1,2),sum),along=3) ; dim(asum)
dimnames(asum)[[3]][3]<-"Gesamt"
dim(asum)
asum
####§6.7,stundenaufgaben:
src8<-riplx(lnk8)#genesis: 12612-0100
dt8<-read.csv2(src8,sep=";",na = c("...","-","."))
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
geburt<-read.csv2("data/geburt_land.csv")
head(geburt)
#geil. export als utf-8 wäre schön gewesen. fk sonderzeichen.
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
###odbc
library(RODBC)
chan<-odbcDriverConnect("")
#no connection
chan
odbcDataSources()
odbcConnect("https://www-genesis.destatis.de")
sqlQuery(chan)
warnings()
#2 sources, but no connection
fix(bev.cube)
