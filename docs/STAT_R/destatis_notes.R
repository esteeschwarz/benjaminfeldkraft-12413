

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
destatis_cred <- read_csv("~/Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")

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
 write_xml(dt3,"data/dt3_genesis_q_erwerbstaetige.xml")
#get:<kurztext>Bevölkerung, Erwerbstätige, Erwerbslose, Erwerbspersonen,
# Nichterwerbspersonen: Bundesländer, Jahre (bis 2019)</kurztext>12211-9004
  # dt4<-read_csv2(riplx()) #no
#dt5 <- read.csv2(riplx(),sep = ";",skip=1) #for import regular csv table
# dt5 <- read.csv2(riplx(),sep = ";") #mind no skip rows import flat csv
 src_d<-"https://www- genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_ KENNUNG&password=IHR_PASSWORT&name=12211-9004&area=all&compress=false&transpose=false&startyear=1950&endyear=2021&tim eslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingk ey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyi ngkey3=&format=ffcsv&job=false&stand=01.01.1970&language=de"
 
 dt5<- read.csv2(riplx(src_d), 
                sep = ";", na = c("-",".","...")) #this important to remove [...] NAs
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
 y<-2;ey<-2;land<-1
 mnt[sy:ey]
 sumloop<-function(land,y){
   sum(dt5$ERW002__Erwerbstaetige__1000[dt5$X1_Auspraegung_Label==bnd[land]&dt5$Zeit==mnt[y]])
   
 }
 sumloop(1,1)
 
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
