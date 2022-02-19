#12077.statistik_R (barghoorn)
#20220218(17.38)
#lebendgeburten tabelle destatis:
"https://www-genesis.destatis.de/genesis/online?sequenz=tabelleErgebnis&selectionname=12612-0002#abreadcrumb"
#try for API fetch tables
#destatis webservices: https://www-genesis.destatis.de/genesis/online?Menu=Webservice#abreadcrumb
library(readr)
library(stringi)

#import local destatis credentials
destatis_cred <- read_csv("Nextcloud/UNI/21S/SPUND/R/destatis_cred.csv")

src<-"https://www-genesis.destatis.de/genesisWS/rest/2020/find/find?username=IHRE_KENNUNG&password=IHR_PASSWORT&term=Lebendgeburten&category=all&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&passwort=IHR_PASSWORT&name=ARBEITSLOS09&bereich=Alle&sprache=de"
src<-"https://www- 
  genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=IHRE_KENNUNG&password=IHR_PASSWORT&selectionname=12612-0002&format=csv&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenAufbau&username=IHRE_KENNUNG&password=IHR_PASSWORT&namen=11111KE001&bereich=Alle&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&namen=11111KE001&bereich=Alle&format=csv&werte=true&metadaten=false&zusatz=false&startjahr=&endjahr=&zeitscheiben=1&inhalte=&regionalmerkmal=&regionalschluessel=&sachmerkmal=GES&sachschluessel=GESW&sachmerkmal2=NAT&sachschluessel2=NATA&sachmerkmal3=&sachschluessel3=&stand=01.01.1900&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/DownloadService_2010?method=TabellenDownload&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12411-0002&bereich=Alle&format=csv&komprimieren=false&transponieren=false&startjahr=1995&endjahr=2000&zeitscheiben=&regionalmerkmal=&regionalschluessel=&sachmerkmal=&sachschluessel=&sachmerkmal2=&sachschluessel2=&sachmerkmal3=&sachschluessel3=&auftrag=false&stand=&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&namen=21311BS003&bereich=Alle&format=csv&werte=true&metadaten=false&zusatz=false&startjahr=&endjahr=&zeitscheiben=1&inhalte=&regionalmerkmal=&regionalschluessel=&sachmerkmal=GES&sachschluessel=GESW&sachmerkmal2=NAT&sachschluessel2=NATA&sachmerkmal3=&sachschluessel3=&stand=01.01.1900&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&username=IHRE_KENNUNG&password=IHR_PASSWORT&term=berufsgruppen&bereich=Alle&format=csv&werte=true&&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/rest/2020/find/find?username=IHRE_KENNUNG&password=IHR_PASSWORT&term=Berufsgruppen&category=all&language=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=DatenExport&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Meine&format=csv&werte=true&&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=ARBEITSLOS09&bereich=Alle&sprache=de"
src<-"https://www-genesis.destatis.de/genesisWS/web/ExportService_2010?method=AuspraegungInformation&kennung=IHRE_KENNUNG&password=IHR_PASSWORT&name=12612-0002&bereich=Alle&sprache=de"

stri_detect(src,regex="IHRE_KENNUNG")
stri_detect(src,regex="IHR_PASSWORT")
#substitute kennung, pwd in request 
stri_replace(src,destatis_cred$kennung,regex = "IHRE_KENNUNG")
stri_replace(src,destatis_cred$pwd,regex = "IHR_PASSWORT")

#dt1<-eval(parse(src))
dt2<-read.csv2(src)
dt3<-read_xml(src)

#i give this one up. i manage no api fetch...
#work with static files
#import dataset:
lebendgeburten <- read.csv2("PRO/git/essais/docs/STAT_R/data/12612-0002_flat.csv",sep= ";")
#fuck sonderzeichen im datensatz!
stri_detect(lebendgeburten$X2_Auspraegung_Label,regex="\xe4")
geschlecht<-stri_replace(lebendgeburten$X2_Auspraegung_Label,"ae",regex = "\xe4")
monat<-stri_replace(lebendgeburten$X3_Auspraegung_Label,"ae",regex = "\xe4")
table2<-replace(lebendgeburten,13,values = geschlecht)
table2<-replace(table2,17,values = monat)
