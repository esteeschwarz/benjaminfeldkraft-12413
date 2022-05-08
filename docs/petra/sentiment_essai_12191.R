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
  write.table(text, file = paste0(path2, rezkey,".txt"), sep="", row.names = FALSE)
}
cat(dtarez)
#wks.
#ref of next essais: Lei, L., & Liu, D. (2021). Conducting Sentiment Analysis 
#(Elements in Corpus Linguistics). 
#Cambridge: Cambridge University Press. doi:10.1017/9781108909679
#################################################################
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
wordcloud(words=df_frequency$word,
          freq=df_frequency$freq,
          min.freq=50,
          random.order=F,
          colors=brewer.pal(6,"Dark2"))
set.seed(2021)
train_sample_id<-sample(1:19,6)
df<-dta
df_train<-df[train_sample_id,]
df_test<-df[-train_sample_id,]
dtm_train<-dtm[train_sample_id,]
dtm_test<-dtm[-train_sample_id,]
corpus_clean_train<-corpus_clean[train_sample_id]
corpus_clean_test<-corpus_clean[-train_sample_id]
dtm_train<-DocumentTermMatrix(corpus_clean_train,control=list(dictionary="high_freq"))
dtm_test<-DocumentTermMatrix(corpus_clean_test,control=list(dictionary="high_freq"))
convert_count<-function(x){
  y<-ifelse(x>0,1,0)
  y<-factor(y,levels=c(0,1),labels = c("No","Yes"))
  y
}
dtm_train_final<-apply(dtm_train,2,convert_count)
dtm_test_final<-apply(dtm_test,2,convert_count)
train_features<-as.data.frame(dtm_train_final)
train_sentiments<-as.factor(df_train$sentiment)
test_features_sentiments<-as.data.frame(dtm_test_final)%>%
  mutate(sentiment=as.factor(df_test$sentiment))
model<-naiveBayes(train_features,train_sentiments,laplace=1)
prediction<-predict(model,newdata=test_features_sentiments)
dim(test_features_sentiments)
#stops here working.
#####################
#ref next: https://journal.r-project.org/archive/2016/RJ-2016-007/RJ-2016-007.pdf
##########
raw.corpus <- load.corpus(files = "all", corpus.dir = "data/", encoding = "UTF-8")
data(raw.corpus)
summary(raw.corpus)
tokenized.corpus <- txt.to.words.ext(raw.corpus, preserve.case = FALSE)
summary(tokenized.corpus)
corpus.no.pronouns <- delete.stop.words(tokenized.corpus, stop.words = stylo.pronouns(corpus.lang = "German"))
corpus.no.pronouns <- delete.stop.words(corpus.no.pronouns, stop.words = c("ein","einer","eine","der","die","das"))

corpus.char.3.grams <- txt.to.features(corpus.no.pronouns, ngram.size = 3, features = "c")
frequent.features <- make.frequency.list(corpus.no.pronouns, head = 20)
freqs <- make.table.of.frequencies(corpus.no.pronouns, features = frequent.features)
head(freqs)
stylo(corpus.dir = "data/", mfw.min = 3, mfw.max = 8, analysis.type = "PCR", sampling = "normal.sampling", sample.size = 10, gui = T)




