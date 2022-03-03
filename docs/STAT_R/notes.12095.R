x<-c(1:5)
y<-c(101:105)
x<-101
y<-100
z<-cbind(x,y)



#chisq.test(x,y,correct = FALSE)
chisq.test(z,correct = FALSE)

text<-unlist(strsplit("eins zwei drei zwei drei eins zwei", " "))
tx<-readline()
txa<-as.double(unlist(strsplit(tx,",")))
txa[1]+txa[2]
cat("helo",txa)
