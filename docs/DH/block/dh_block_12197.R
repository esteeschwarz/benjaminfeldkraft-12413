#12197.dh.stylometrie
#20220513(14.26)
#####################
library(stylo)
#corpus<-"..data/corpora/benjamin"
#corpuscpt<-"..data/corpora/grimm-kontrastanalyse/corpus"

corpus2<-"~/PRO/git/essais/docs/DH/block/corpora/grimm-kontrastanalyse/corpus/primary_set"
corpus1<-"~/PRO/git/essais/docs/DH/block/corpora/grimm-kontrastanalyse/corpus/secondary_set"
corpus1<-"~/PRO/git/essais/docs/DH/block/corpora/benjamin/kontrast/2"
corpus2<-"~/PRO/git/essais/docs/DH/block/corpora/benjamin/kontrast/3"

work<-"~/PRO/git/essais/docs/DH/block/temp"
#work<-"~/PRO/git/essais/docs/DH/block/temp"
#stylo(corpus.dir = corpus, path=work, gui = T)
comp_e<-oppose(primary.corpus.dir=corpus1,secondary.corpus.dir=corpus2,path=work)
#funktionsfehler: setzt WD nicht von R WD aus, also path absolut bestimmen
comp$comparison
library(stringi)

stri_detect_regex()
###################
src<-"/Users/guhl/PRO/git/clones/forTEXT.net/Preprocessing mit NLTK/Werther_Saetze_neu1.txt"
txt<-scan(src," ")
regex<-"(.{5,20}Werther)(.{5,20})"
regex="Werther"
kwic<-grep(pattern=regex,txt,value=T)
#write_csv(kwic,"/Users/guhl/PRO/git/clones/forTEXT.net/Preprocessing mit NLTK/rgexout.txt")
print(kwic)
###########
#corpus cite:
#Rauh, Christian; Schwalbach, Jan, 2020, 
#"The ParlSpeech V2 data set: Full-text corpora of 6.3 million 
#parliamentary speeches in the key legislative chambers of nine representative democracies", 
#https://doi.org/10.7910/DVN/L4OAKN, Harvard Dataverse, V1
###
#src<-"//dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/L4OAKN/PCYUNY"
#src<-"//dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/PCYUNY&version=1.0#"
#con<-gzfile(src)
#dta<-unz("https",filename=src,open = "r")
src<-"/Users/guhl/Nextcloud/UNI/21S/DH/data/corpus/BT/Corp_Bundestag_V2.rds"
dta<-readRDS(src)
colnames(dta)







