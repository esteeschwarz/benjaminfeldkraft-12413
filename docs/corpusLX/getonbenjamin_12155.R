###########12155.
library(stringi)
src<-"https://github.com/esteeschwarz/essais/raw/main/docs/corpusLX/data/11302.benjaminbuch5.txt"

ben5<-scan(src," ") 
#ben1: #35247 #9491 #0.2692711
#ben2: #13461 #4733 #0.3516084
#ben3: #36545 #9017 #0.2467369
#ben4: #38129 #9828 #0.2577566
#ben5: #37310 #9754 #0.2614313
s5<-unique(ben5)
head(s1)
9491/35247
4733/13461
9017/36545
9828/38129
9754/37310

m2<-!is.na(match(s2,s1))
m1<-match(s1,s2)
m2<-m1[!NA]
m1[1:100]
s21<-s2[m2]
s11[1:300]
write_clip(s21)
s14<-s13
s13<-s12
s12<-s11