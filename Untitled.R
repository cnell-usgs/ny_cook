url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url, destfile = 'acs.data.csv', method='curl')
acs<-read.table('acs.data.csv', sep=",", header=T, stringsAsFactors = FALSE)

#VAL is property value
unique(acs$SERIALNO)
length(acs$VAL == 24)
acst<-acs%>%
  group_by(ST, PUMA, SERIALNO)%>%
  filter(VAL == 24)
length(acst)
View(acst)
ads<-filter(acs, VAL == 24)
str(ads)
length(unique(ads$SERIALNO)) ##each uniwue house that has value over 1000000

str(acs$FES)
unique(acs$FES)

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx'
download.file(url, destfile = 'gas.data.csv', method='curl', mode='wb')
dat<-read.table('gasr.csv', sep=',', header=TRUE)
str(dat)
sum(dat$Zip*dat$Ext,na.rm=T)

library(XML)
url<-'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml'
download.file(url, destfile='baltrest.xml', method='curl')
tt<-xmlParse('baltrest.xml')
name<-getNodeSet(tt, '//name')
zips<-getNodeSet(tt, '//zipcode')
rowid<-getNodeSet(tt, '//row')
names <- lapply(rowid, xpathSApply, ".//name", xmlValue)
names[sapply(names, is.list)] <- NA
zip <- sapply(rowid, xpathSApply, "string(.//zipcode/text())")
zip
balty<-data.frame(ZIP=rep(zip, sapply(names, length)),
           REST=unlist(names))
View(balty)
bz<-filter(balty, ZIP==21231)
str(bz)
length(unique(bz$REST))
