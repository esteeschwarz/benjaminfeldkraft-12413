#12191.sentiment essai
#petraKIP prepare
#20220507(08.56)
######################
src1<-"https://raw.githubusercontent.com/esteeschwarz/essais/main/docs/DH/data/guhl_benjaminbuch1.txt"
dta<-scan(src1,"character",sep="\n",encoding = "utf-8")
glimpse(dta)
write_clip(dta)
#seperate sentences
library(stringi)
space<-stri_detect_regex(dta,pattern ="\\.\\s")
dta2<-stri_replace_all(dta,regex ="\\.\\s",replacement = "#n#")
dta3<-stri_replace_all(dta2,regex ="\\.\\r",replacement = "#n#")
dta4<-stri_replace_all(dta3,regex ="#n#",replacement = "\n")
dta5<-stri_replace_all(dta4,regex ="[\\.,;\\-&:]",replacement = " ")
dta6<-stri_replace_all(dta5,regex ='"',replacement = "")
dta7<-stri_replace_all(dta6,regex ="\\s{2,}",replacement = " ")
write_clip(dta7)
#wks. results in sentences array.
## amz rezensionen
src2<-"https://raw.githubusercontent.com/esteeschwarz/essais/main/docs/DH/data/temp/amz_rez.csv"
dta<-read.csv2(src2,header = T)
#row<-2
path2<-"temp/rez"
for (row in 1:nrow(dta)){
  rezkey <- as.character(dta[row,1])
  dtarez<-dta[row,]
  text <- select(dtarez,rezension )
  write.table(text, file = paste0(path2, rezkey), sep="", row.names = FALSE)
}
cat(dtarez)
#wks.
glimpse(dta)
table(dta$sentiment)
prop.table(table(dta$sentiment))

library(dplyr)
library("readr") 
library("tm") 
install.packages("caret")
library("wordcloud") 
library("e1071") 
library("caret")

corpus <- Corpus(VectorSource(dta$rezension))
inspect(corpus[1:3])
corpus_clean<-corpus %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords,stopwords(kind="de"))%>%
  tm_map(stripWhitespace)

dtm<-DocumentTermMatrix((corpus_clean))
inspect(dtm)
dtm_matrix<-as.matrix(dtm)
dtm_frequency_sort<-sort(colSums(dtm_matrix),decreasing = T)
df_frequency<-data.frame(word=names(dtm_frequency_sort),freq=dtm_frequency_sort)
head(df_frequency)
