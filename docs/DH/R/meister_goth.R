# 12172.DH
# 20220424(16.43)
#################
library (stringi)
#goethe wilhelm meister 1-8: (2335)-(2343)
src1<-"https://gutenberg.org/ebooks/2335.txt.utf-8"
src2<-"https://gutenberg.org/ebooks/2336.txt.utf-8"
src3<-"https://gutenberg.org/ebooks/2337.txt.utf-8"
src4<-"https://gutenberg.org/ebooks/2338.txt.utf-8"
src5<-"https://gutenberg.org/ebooks/2339.txt.utf-8"
src6<-"https://gutenberg.org/ebooks/2340.txt.utf-8"
src7<-"https://gutenberg.org/ebooks/2341.txt.utf-8"
src8<-"https://gutenberg.org/ebooks/2342.txt.utf-8"
#src9<-"https://gutenberg.org/ebooks/2343.txt.utf-8"




dta<-scan(src2,"")
dta[1400:2200]
end<-which(dta==".")
dta3<-dta[end[length(end)]:length(dta)]
tail(dta3)
start<-which(dta=="PRINT!")
dta[1898:2000]
dta[start[1]:start[1]+20]
start2<-start[1]+20
startbook<-which(dta[start[1]:start2]=="Wilhelm")
start1<-startbook-1
dta4<-dta[start[1]+start1:length(dta)]
dta5<-dta4[!is.na(dta4)]
tail(dta5)
dta4[16000:16100]
#write(dta1,"meister_bd1.txt")
dta2<-scan(src,"",sep="\n")
write(dta2,"meister_p_bd1.txt")
dt5<-unique(dta4)

ext<-function(src){
  dta<-scan(src,"")
  #end<-which(dta==".")
  start<-which(dta=="PRINT!")
  
  #dta3<-dta[end[length(end)]:length(dta)]
  start2<-start[1]+20
  startbook<-which(dta[start[1]:start2]=="Wilhelm")
  start1<-startbook-1
  dta4<-dta[start[1]+start1:length(dta)]
  dta5<-dta4[!is.na(dta4)]
  
}
#bd1[12500:12718]
bd1<-ext(src1)
bd2<-ext(src2)
bd3<-ext(src3)
bd4<-ext(src4)
bd5<-ext(src5)
bd6<-ext(src6)
bd7<-ext(src7)
bd8<-ext(src8)

#uni<-funtion(dta){
  bduni<-cbind(unique(bd1),
               unique(bd2),
               unique(bd3),
               unique(bd4),
               unique(bd5),
               unique(bd6),
               unique(bd7),
               unique(bd8))

  }