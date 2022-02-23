#12084.play task
#20220222(07.13)
#users without genesis credentials loaded load libraries below and start at # 4. #
#########
library(readr)
library(stringi)
library(xml2)
#########
#import local destatis credentials. diese sind in einer csv nach dem muster kennung,pwd abgelegt
#setwd("~/Users/guhl/Nextcloud/UNI/21S/SPUND/R/")
destatis_cred <- read_csv("~/Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")
###destatis sample links:
# src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Alle&sprache=de"
# #wks >
#for xml request by keywords
src_x<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=Recherche&luceneString=Geburten&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&listenLaenge=100&sprache=de&kategorie=tabellen"
# src<-"https://www-ge nesis.d estatis.de/ge nesisWS/web/Recher cheServic e_2010?method=MerkmalAuspraegunge nKatalog&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=BILHS1&auswahl=hs18*&kriterium=code&b ereich=Alle&listenLaenge=10&sprache=de"
# #wks: ausprägungen merkmal, xml_children: 6
# src<-"https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=MerkmalTabellenKata log&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=GES&auswahl=12*&bereich=Alle&listenLaenge= 15&sprache=de"
###for csv data request
src_d<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12612-0005&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"


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

#riplx() #produces clean link with credentials in it
#####
dt3<-read_xml(riplx(src_x)) #for request of xml sheets, catalogue requests...
#####wks
# dt4<-read_csv2(riplx()) #no
#dt5 <- read.csv2(riplx(),sep = ";",skip=1) #for import regular csv table
# dt5 <- read.csv2(riplx(),sep = ";") #mind no skip rows import flat csv
####12084.
dt5<- read.csv2(riplx(src_d),sep = ";", na = c("-",".","...")) #this important to remove [...] NAs
##########
#if read_delim instead, the variable names are bracketed complicate way in sonderzeichen, not plain as with read.csv2 
#wks. yes!
#works
####for xml dissociate
rech1<-xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(dt3))))))
# #bei children=7 gibt es keine einträge mehr
(rech1[1:20])
####save the request into file for scanning manual for interesting statistics at genesis
write_xml(dt3,"genesis_cat.xml")
####
####subset of dataset without NAs
dt51<-subset(dt5[!is.na(dt5$BEV001__Lebendgeborene__Anzahl),])
sum(dt51$BEV001__Lebendgeborene__Anzahl)
#wks
max<-max(dt51$BEV001__Lebendgeborene__Anzahl)
#####
#wks! > row.names(dt5[dt51$BEV001__Lebendgeborene__Anzahl==max,])
#extract row number = row name with max value
maxrow<-row.names(dt51[dt51$BEV001__Lebendgeborene__Anzahl==max,])
dt51[maxrow,]
#dt51[1586,]      
####interessant: peak bei 30jahren, erstes kind, 2020 mit 31628 geburten
#md<-median(dt51$BEV001__Lebendgeborene__Anzahl)     
#mdrow<-row.names(dt51[dt51$BEV001__Lebendgeborene__Anzahl==md,])
#dt51[mdrow,]         
####2.now research task:
####2.1.werden heute mehr kinder mit späterem alter der mutter geboren
####als vor zehn jahren?
####3.save data
getwd()
#setwd("~/PRO/git/essais/docs/STAT_R/")
write.csv2(dt51,"~/PRO/git/essais/docs/STAT_R/data/dt51_outl.csv")
####push gith...
####reimport from gith
##################################################################
####4.
####from here #####START#### for users with no genesis credentials loaded above.
##################################################################
git<-("https://github.com/esteeschwarz/essais/raw/main/docs/STAT_R/data/dt51_outl.csv")
dt6<-read.csv2(git,sep=";")
sum(dt6$BEV001__Lebendgeborene__Anzahl)
#wks.now repeat routine
dt51<-dt6 #fresh set without outliers
max<-max(dt51$BEV001__Lebendgeborene__Anzahl)
#####
#wks! > row.names(dt5[dt51$BEV001__Lebendgeborene__Anzahl==max,])
#extract row number = row name with max value
maxrow<-row.names(dt51[dt51$BEV001__Lebendgeborene__Anzahl==max,])
dt51[maxrow,]

#acc. barghoorn §3.8
dt51[dt51$BEV001__Lebendgeborene__Anzahl==max(dt51$BEV001__Lebendgeborene__Anzahl),]
dt51[which.max(dt51$BEV001__Lebendgeborene__Anzahl),]


