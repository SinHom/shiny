
#####create Nskep export file
######
##### detemine 13 next months
date<-Sys.Date()
# date character string containing POSIXct date
date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
mon<-list()
year<-list()
thirteen_months<-list()
mon[1] <- date.lt$mon
year[1] <- date.lt$year
for (i in 1:13){
  mon[i+1]<-as.integer(mon[i])+1
  year[i+1]<-as.integer(year[i])+as.integer(mon[i+1]==13)# if month is December add a year
  mon[i+1]=as.integer(mon[i+1])-as.integer(mon[i+1]==13)*12
  thirteen_months[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
thirteen_months<-unlist(thirteen_months)

mon<-list()
year<-list()
six_months<-list()
mon[1] <- date.lt$mon+1
year[1] <- date.lt$year
for (i in 1:6){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  six_months[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
six_months<-unlist(rev(six_months))

mon<-list()
year<-list()
last_twelve<-list()
mon[1] <- date.lt$mon+1
year[1] <- date.lt$year
for (i in 1:12){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  last_twelve[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
last_twelve<-unlist(rev(last_twelve))

mon<-list()
year<-list()
last_24m<-list()
mon[1] <- date.lt$mon+1
year[1] <- date.lt$year
for (i in 1:24){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  last_24m[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
last_24m<-unlist(rev(last_24m))

######
#### create folder if it doesn't exist
dir.create("X:\\Nskep",showWarnings = FALSE)
dir.create("X:\\Nskep\\Shiny_up_to_date",showWarnings = FALSE)
#### write xslt file (for nskep extract)
fileConn<-file("X:\\Nskep\\nSkepCSV_DPperMonth.xslt")
lines13<-c()
for (i in 1:13){
  lines13<-c(lines13,"<xsl:text>;</xsl:text>",paste("<xsl:value-of select=\"translate(round(substring(descendant::node()[DimValue='",thirteen_months[i],"'],7)),'Na','0')\"/>",sep=""))
}
writeLines(c(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<xsl:stylesheet version=\"2.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">",
  "<xsl:output encoding=\"UTF-8\" indent=\"no\" method=\"text\" omit-xml-declaration=\"yes\" standalone=\"yes\"/>",
  "<xsl:decimal-format decimal-separator=\".\" infinity=\"+INF\" minus-sign=\"-\" NaN=\"########\"/>",
  "<xsl:template match=\"/\">",
  "<xsl:for-each select=\"descendant::ParamDimValue\">",
  "<xsl:if test=\"DimValue/@Name = 'Item'\">", 				
  "<xsl:value-of select=\"translate(node(),'*',';')\"/>",
  lines13,
  "<xsl:element name=\"Dummy\" xml:space=\"preserve\"><xsl:text>",
  "</xsl:text></xsl:element>",
  "</xsl:if>",
  "</xsl:for-each>",
  "</xsl:template>",
  "</xsl:stylesheet>"
  ),      
  fileConn)
close(fileConn)

fileConn<-file("X:\\Nskep\\nSkepCSV_Hist24.xslt")
lines24<-c()
for (i in 1:24){
  lines24<-c(lines24,"<xsl:text>;</xsl:text>",paste("<xsl:value-of select=\"translate(round(substring(descendant::node()[DimValue='",last_24m[i],"'],7)),'Na','0')\"/>",sep=""))
}
writeLines(c(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<xsl:stylesheet version=\"2.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">",
  "<xsl:output encoding=\"UTF-8\" indent=\"no\" method=\"text\" omit-xml-declaration=\"yes\" standalone=\"yes\"/>",
  "<xsl:decimal-format decimal-separator=\".\" infinity=\"+INF\" minus-sign=\"-\" NaN=\"########\"/>",
  "<xsl:template match=\"/\">",
  "<xsl:for-each select=\"descendant::ParamDimValue\">",
  "<xsl:if test=\"DimValue/@Name = 'Item'\">",   			
  "<xsl:value-of select=\"translate(node(),'*',';')\"/>",
  lines24,
  "<xsl:element name=\"Dummy\" xml:space=\"preserve\"><xsl:text>",
  "</xsl:text></xsl:element>",
  "</xsl:if>",
  "</xsl:for-each>",
  "</xsl:template>",
  "</xsl:stylesheet>"
),      
fileConn)
close(fileConn)


######

#setwd("C:\\Users\\glaboure\\Documents\\R\\Test_DPM2")
setwd("S:/Procurement/Commun/SIOP/Central org SIOP/N. R and shiny/data")
library(plyr)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)

DivName<-read.csv2("DivName.csv")
DivName<-data.table(DivName, key='Div')
lasttable<-NULL

###### Start of function to remove leading 0

remove_leading_0<-function(ordered_vector)
{
  aaa<-0
  bbb<-0
  indiceStart<-0
  while(aaa==0 & indiceStart<length(ordered_vector)[1])
  {
    indiceStart<-indiceStart+1
    if(substr(ordered_vector[indiceStart],1,1)>=0)
    {aaa<-1}    
  }
  
  indiceEnd<-indiceStart
  
  while(bbb==0)
  {
    if(substr(ordered_vector[indiceEnd+1],1,1)==0)
    {indiceEnd<-indiceEnd+1} 
    else 
    {bbb<-1}
  }
  ordered_vector[indiceStart:indiceEnd]<-sapply(ordered_vector[indiceStart:indiceEnd], function(x) {sub("^[0]+", "",x)})
  return(ordered_vector)
}

###### End of function to remove leading 0

###### Start of function to clean for VST
#### try to load 2 times of the data, first w/o VST, 2nd with VST, then split the 2nd into TR01 and DB01

cleanvst <- function(x){
  
  x1 <- x[x$Div != "D10",]
  
  t <- select(DivName, c(1,3))
  
  x1 <- left_join(x1, t, by = "Div")
  
  x1 <- data.table(x1,key='Material,Div,CustGr')
  
  x2 <- x[x$Div == "D10",]
  
  x2 <- data.table(x2,key='Material,Div,CustGr')
  
  x2 <- left_join(x2, t, by = "Div")
  
  x2 <- data.table(x2,key='Material,Div,CustGr')
  
  tr01 <- x2[str_sub(x2$Material,-5,-1) == "_TR01",]
  
  tr01$Material <-  str_replace(tr01$Material,"_TR01","")
  
  db01 <- x2[str_sub(x2$Material,-5,-1) == "_DB01",]
  
  db01$Platform <- str_replace(db01$Platform, "ISL","DBI")
  db01$Material <-  str_replace(db01$Material,"_DB01","")
  
  x <- unique(x)
  
  x <- rbind(x1,tr01,db01)
  
  return(x)
}


cleanvst2 <- function(x)
{
  x <- as.data.frame(x)
  
  x1 <- x[x$Platform != "ISL",]
  
  x2 <- x[x$Platform == "ISL",]
  
  
  tr01 <- x2[str_sub(x2$Material,-5,-1) == "_TR01",]
  
  tr01$Material <-  str_replace(tr01$Material,"_TR01","")
  
  db01 <- x2[str_sub(x2$Material,-5,-1) == "_DB01",]
  
  db01$Platform <- str_replace(db01$Platform, "ISL","DBI")
  
  db01$Material <-  str_replace(db01$Material,"_DB01","")
  
  x <- unique(x)
  
  x <- rbind(x1,tr01,db01)
}
###### End of function clean

###### Function of finding duplicates in table

find_dups <- function(X){
  
  X = as.data.frame(X)
  
  ### finding the duplicate lines
  dups = duplicated(X)
  X = as.data.table(X)
  res_dups = X[,fD:= dups | c(tail(dups,-1),FALSE)]
  res_dups = res_dups[res_dups$fD == TRUE]
  res = res_dups[res_dups$fD == FALSE]
  
  ### merge the duplicate lines
  numeric_cols <- which(sapply(res_dups, is.numeric))
  res_dups[, lapply(.SD, sum), by = c('Div,Material,CustGr'), .SDcols = numeric_cols]
  
  ### merge two tables and remove the last column
  res_final = rbind(res,res_dups)
  
  res_final[,fD:= NULL]
  
  return(res_final)
}

###### End of function finding duplicates

#########Import DP M-2
######


currentDP<-subset(
  read.table("DPMatDFU2.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Div","Material","CustGr",thirteen_months),colClasses=c("Material"="character"))
  ,Div %in% c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20"))
currentDP<-currentDP[rowSums(currentDP[4:16])!=0,] ###Remove empty lines (full of 0)
currentDP<-data.table(currentDP, key='Material,Div,CustGr')
currentDP <- cleanvst(currentDP)
currentDP<-data.table(currentDP, key='Material,Div,CustGr,Platform')
currentDP$Material<-remove_leading_0(currentDP$Material)  ###Remove leading 0
for (Months in names(currentDP)[!is.na(as.numeric(substr(names(currentDP),2,7)))])
{setnames(currentDP,Months,paste("DP_",substr(Months,2,7),sep=""))}  ##### Add demand type before month in column labels
currentDP<-data.table(currentDP, key='Material,Div,CustGr,Platform')

######


DDquota<-subset(
  read.table("DD_used_valid.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Div","Material","CustGr","Month","DDquota"),colClasses=c("Material"="character"))
  ,Div %in% c("D02","D03","D05","D06","D08","D09","D10","D13","D20")) ## note that D07 is excluded as DD are managed separately
DDquota<-data.table(DDquota, key='Material,Div,CustGr,Month')
DDquota$Material<-remove_leading_0(DDquota$Material)  ###Remove leading 0
DDquota<-data.table(DDquota, key='Material,Div,CustGr,Month')
DDquota<-dcast.data.table(DDquota,Material+Div+CustGr~Month,
                          fill=as.integer(0),value.var='DDquota')
for (Months in names(DDquota)[!is.na(as.numeric(names(DDquota)))])
{setnames(DDquota,Months,paste("DD%_",Months,sep=""))}  ##### Add demand type before month in column labels
DDquota<-data.table(DDquota, key='Material,Div,CustGr')
DDquota <- cleanvst(DDquota)
DDquota<-data.table(DDquota, key='Material,Div,CustGr,Platform')
######

previousDP<-subset(
  read.table("DPM-1atDFU2.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Div","Material","CustGr",thirteen_months),colClasses=c("Material"="character"))
  ,Div %in% c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20"))
previousDP<-data.table(previousDP, key='Material,Div,CustGr')
previousDP <- cleanvst(previousDP)
previousDP<-data.table(previousDP, key='Material,Div,CustGr,Platform')
previousDP$Material<-remove_leading_0(previousDP$Material)  ###Remove leading 0
for (Months in names(previousDP)[!is.na(as.numeric(substr(names(previousDP),2,7)))])
{setnames(previousDP,Months,paste("lastDP_",substr(Months,2,7),sep=""))}  ##### Add demand type before month in column labels
previousDP<-data.table(previousDP, key='Material,Div,CustGr,Platform')

######


#########Import Demand
######

demandN<-read.table("HISTatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDN_",last_24m,sep="")))
##demandN<-find_dups(demandN)
demandN <- cleanvst(demandN)
demandN<-data.table(demandN, key='Material,Div,CustGr,Platform')
demandN$Material<-remove_leading_0(demandN$Material)  ###Remove leading 0
demandN<-unique(demandN, key='Material,Div,CustGr,Platform')
demandN<-data.table(demandN, key='Material,Div,CustGr,Platform')

###Run those code only if there are duplicates
# ### finding and remove duplicates for this table
# dups = duplicated(demandN)
# res_dups = demandN[,fD:= dups | c(tail(dups,-1),FALSE)]
# res_true = res_dups[res_dups$fD == TRUE]
# res_false = res_dups[res_dups$fD == FALSE]
# 
# ### merge the duplicate lines
# numeric_cols <- which(sapply(res_true, is.numeric))
# res_true = res_true[, lapply(.SD, sum), by = c('Div,Material,CustGr'), .SDcols = numeric_cols]
# 
# ### merge two tables and remove the last column
# res_false[,fD:= NULL]
# res_false[,Platform:= NULL]
# 
# res_final = rbind(res_true,res_false)
# 
# demandN <- res_final
# demandN<-data.table(demandN, key='Material,Div,CustGr')
# demandN <- cleanvst(demandN)
# demandN<-data.table(demandN, key='Material,Div,CustGr,Platform')
# ### End finding and remove duplicates for this table

demandD<-read.table("HISTDDatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDD_",last_24m,sep="")))
######correction for SWF demand recording specificity 
demandSWF<-subset(demandN,CustGr=="DIRECTSWF")
demandD<-data.table(demandD, key='Material,Div,CustGr')
demandD <- cleanvst(demandD)
demandD<-rbind(demandD,demandSWF,use.names=FALSE)
demandSWF<-NULL
demandD<-data.table(demandD, key='Material,Div,CustGr,Platform')
demandD$Material<-remove_leading_0(demandD$Material)  ###Remove leading 0
demandD<-data.table(demandD, key='Material,Div,CustGr,Platform')

demandZ<-read.table("HISTZEatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDE_",last_24m,sep="")))
demandZ<-data.table(demandZ, key='Material,Div,CustGr')
demandZ <- cleanvst(demandZ)
demandZ<-data.table(demandZ, key='Material,Div,CustGr,Platform')
demandZ$Material<-remove_leading_0(demandZ$Material)  ###Remove leading 0
demandZ<-data.table(demandZ, key='Material,Div,CustGr,Platform')

#########Unique keys
######
listkey<-rbind(currentDP[,c("Material","Div","CustGr","Platform"),with=FALSE],previousDP[,c("Material","Div","CustGr","Platform"),with=FALSE],demandN[,c("Material","Div","CustGr","Platform"),with=FALSE],demandD[,c("Material","Div","CustGr","Platform"),with=FALSE],demandZ[,c("Material","Div","CustGr","Platform"),with=FALSE])
listkey<-data.table(unique(listkey, key='Material,Div,CustGr,Platform'))

# Corresp<-data.table(Div=c("D02","D03","D05","D06","D07","D08","D09","D10","D10","D13","D20"),Platform=c("BRT","BRT","GEE","SAA","HED","REH","HED","ISL","DBI","OCW","MOW"),key='Div')
# listkey<-data.table(listkey, key='Div,Material,CustGr,Platform')
# listkey<-Corresp[listkey,by=.EACHI]
# listkey<-data.table(listkey, key='Material,Div,CustGr,Platform')
# listkey<-unique(listkey)
######

#########Import DP families
######
DPfamily<-read.csv2("DPfamPL.txt",colClasses=c("Material"="character","Product.Line"="character"))
DPfamily<-data.table(DPfamily, key='Material')
DPfamily$Material<-remove_leading_0(DPfamily$Material)  ###Remove leading 0
DPfamily<-data.table(DPfamily, key='Material')
######

#########Import PAP
######
Forex_rate<-subset(
  read.table("Forex rate.txt",sep=";",fileEncoding = "UTF-16",col.names=c("currency","rate"),colClasses=c("currency"="character","rate"="numeric"))
  ,currency %in% c("GBP","PLZ","TRY","TRY1","RUB"))
Forex_rate<-cbind(Forex_rate,data.frame(Platform=c("REH","OCW","ISL","DBI","MOW")))
Forex_rate<-rbind(Forex_rate,data.frame(currency=rep("EUR",4),rate=rep(1,4),Platform=c("BRT","GEE","SAA","HED")))
Forex_rate<-data.table(Forex_rate, key='Platform')

PAP<-subset(
  read.table("PAP.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Platform","Material","PAP"),colClasses=c("PAP"="numeric","Material"="character"),dec=".")
  ,Platform %in% c("BRT","GEE","SAA","HED","REH","ISL","DBI","OCW","MOW"))
PAP<-cleanvst2(PAP)
PAP<-data.table(PAP, key='Material,Platform')
PAP$Material<-remove_leading_0(PAP$Material)  ###Remove leading 0
PAP<-PAP[eval(dim(PAP)[1]:1),]  ###Reverse order as leading 0 are the one we suppose to be removed by unique()
PAP<-data.table(PAP, key='Material,Platform')

PAP<-unique(PAP,key='Material,Platform')
PAP<-data.table(PAP, key='Platform,Material')
PAP<-Forex_rate[PAP][,list(Platform,Material,PAP_EUR=PAP*rate,PAP_LC=PAP)]
PAP<-data.table(PAP, key='Material,Platform')
######



#########Import SemNet price
SemNet <- subset(
  read.table("SemNet.txt",sep=";",fileEncoding = "UTF-16",col.names = c("Div","Material","CustGr","empty","SemNet"),colClasses =c("Material"="character","SemNet"="numeric"),dec=".")[,c(1,2,3,5)]
  ,Div %in% c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20"))
SemNet <- cleanvst(SemNet)
SemNet <- data.table(SemNet, key = 'Material,Div,CustGr,Platform')
SemNet$Material <- remove_leading_0(SemNet$Material) ###Remove leading 0
SemNet <- SemNet[eval(dim(SemNet)[1]:1),]  ###Reverse order as leading 0 are the one we suppose to be removed by unique()
SemNet <- data.table(SemNet, key = 'Material,Div,CustGr,Platform')

DivName <- data.table(DivName, key = 'Platform')
fo_ra <- left_join(Forex_rate, DivName, by = 'Platform')
fo_ra <- data.frame(fo_ra)
fo_ra <- fo_ra[-1, c(1,2,3,4)]
fo_ra <- data.table(fo_ra, key = 'Div')

SemNet <- unique(SemNet, key = 'Material,Div,CustGr')
SemNet <- data.table(SemNet, key = 'Div,Material,CustGr')
SemNet <- fo_ra[SemNet][,list(Div, Material, CustGr, SemNet_EUR = SemNet*rate, SemNet_LC = SemNet)]
SemNet <- data.table(SemNet, key = 'Material,Div,CustGr')
SemNet <- unique(SemNet, key = 'Material,Div,CustGr')
#########


#########Import RP families
######
RPfamily<-read.table("RP_Family2.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Platform","Material","RPfamily","Supplier","empty"),colClasses=c("Material"="character","RPfamily"="character","Supplier"="character"))[,1:4]
RPfamily$Supplier <-  str_replace(RPfamily$Supplier,"803967","915206")
RPfamily<-cleanvst2(RPfamily)
RPfamily<-data.table(RPfamily, key='Material,Platform')
RPfamily$Material<-remove_leading_0(RPfamily$Material)  ###Remove leading 0
RPfamily<-data.table(RPfamily, key='Material,Platform')
RPfamily<-unique(RPfamily) ### Remove duplicate (can exist because of leading 0 making 2 keys...)
RPfamily<-data.table(RPfamily, key='Material,Platform')
######

lasttable<-data.table(demandZ[listkey],key='Material,Div,CustGr,Platform')
lasttable<-data.table(demandD[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(demandN[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(currentDP[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(previousDP[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(DPfamily[lasttable],key='Material,Platform,Div,CustGr')
lasttable<-data.table(PAP[lasttable],key='Material,Platform,Div,CustGr')
lasttable<-data.table(RPfamily[lasttable], key='Material,Div,CustGr,Platform')
lasttable<-data.table(SemNet[lasttable], key='Material,Div,CustGr,Platform')
lasttable<-DDquota[lasttable]
lasttable<-data.frame(lasttable)
lasttable[lasttable$CustGr=="DIRECTSWF",c(4,5,6,7,8,9,10,11,12,13,14,15,16)]<-c(1,1,1,1,1,1,1,1,1,1,1,1,1)

lasttable<-data.table(lasttable, key='Material,Div,CustGr,Platform')

setwd(".././output")

#####remove NA function
#removeNA = function(DT) {
  # or by number (slightly faster than by name) :
  #for (j in seq_len(ncol(DT)))
    #set(DT,which(is.na(DT[[j]])),j,0)
#}

#####remove NA
for (j in c(4:18,22:23,26:123)){set(lasttable,which(is.na(lasttable[[j]])),j,0)}

tableauCGvendor<-subset(
  lasttable[,list("month_-24_tot"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-23_tot"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-22_tot"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-21_tot"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-20_tot"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-19_tot"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-18_tot"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-17_tot"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-16_tot"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-15_tot"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-14_tot"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-13_tot"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-12_tot"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-11_tot"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-10_tot"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-9_tot"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-8_tot"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-7_tot"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-6_tot"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-5_tot"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-4_tot"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-3_tot"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-2_tot"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-1_tot"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-24_os"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_"))-get(paste("HDD",last_24m[1],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-23_os"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_"))-get(paste("HDD",last_24m[2],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-22_os"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_"))-get(paste("HDD",last_24m[3],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-21_os"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_"))-get(paste("HDD",last_24m[4],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-20_os"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_"))-get(paste("HDD",last_24m[5],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-19_os"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_"))-get(paste("HDD",last_24m[6],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-18_os"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_"))-get(paste("HDD",last_24m[7],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-17_os"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_"))-get(paste("HDD",last_24m[8],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-16_os"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_"))-get(paste("HDD",last_24m[9],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-15_os"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_"))-get(paste("HDD",last_24m[10],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-14_os"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_"))-get(paste("HDD",last_24m[11],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-13_os"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_"))-get(paste("HDD",last_24m[12],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-12_os"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_"))-get(paste("HDD",last_24m[13],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-11_os"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_"))-get(paste("HDD",last_24m[14],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-10_os"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_"))-get(paste("HDD",last_24m[15],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-9_os"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_"))-get(paste("HDD",last_24m[16],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-8_os"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_"))-get(paste("HDD",last_24m[17],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-7_os"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_"))-get(paste("HDD",last_24m[18],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-6_os"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_"))-get(paste("HDD",last_24m[19],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-5_os"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_"))-get(paste("HDD",last_24m[20],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-4_os"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_"))-get(paste("HDD",last_24m[21],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-3_os"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_"))-get(paste("HDD",last_24m[22],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-2_os"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_"))-get(paste("HDD",last_24m[23],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-1_os"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_"))-get(paste("HDD",last_24m[24],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-24_dd"=sum((get(paste("HDD",last_24m[1],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-23_dd"=sum((get(paste("HDD",last_24m[2],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-22_dd"=sum((get(paste("HDD",last_24m[3],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-21_dd"=sum((get(paste("HDD",last_24m[4],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-20_dd"=sum((get(paste("HDD",last_24m[5],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-19_dd"=sum((get(paste("HDD",last_24m[6],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-18_dd"=sum((get(paste("HDD",last_24m[7],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-17_dd"=sum((get(paste("HDD",last_24m[8],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-16_dd"=sum((get(paste("HDD",last_24m[9],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-15_dd"=sum((get(paste("HDD",last_24m[10],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-14_dd"=sum((get(paste("HDD",last_24m[11],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-13_dd"=sum((get(paste("HDD",last_24m[12],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-12_dd"=sum((get(paste("HDD",last_24m[13],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-11_dd"=sum((get(paste("HDD",last_24m[14],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-10_dd"=sum((get(paste("HDD",last_24m[15],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-9_dd"=sum((get(paste("HDD",last_24m[16],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-8_dd"=sum((get(paste("HDD",last_24m[17],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-7_dd"=sum((get(paste("HDD",last_24m[18],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-6_dd"=sum((get(paste("HDD",last_24m[19],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-5_dd"=sum((get(paste("HDD",last_24m[20],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-4_dd"=sum((get(paste("HDD",last_24m[21],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-3_dd"=sum((get(paste("HDD",last_24m[22],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-2_dd"=sum((get(paste("HDD",last_24m[23],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_-1_dd"=sum((get(paste("HDD",last_24m[24],sep="_")))*PAP_EUR/1000,na.rm=TRUE),
                  "month_1_DP_os"=sum(get(paste("DP",thirteen_months[1],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[1],sep="_"))),na.rm=TRUE),
                  "month_2_DP_os"=sum(get(paste("DP",thirteen_months[2],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[2],sep="_"))),na.rm=TRUE),
                  "month_3_DP_os"=sum(get(paste("DP",thirteen_months[3],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[3],sep="_"))),na.rm=TRUE),
                  "month_4_DP_os"=sum(get(paste("DP",thirteen_months[4],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[4],sep="_"))),na.rm=TRUE),
                  "month_5_DP_os"=sum(get(paste("DP",thirteen_months[5],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[5],sep="_"))),na.rm=TRUE),
                  "month_6_DP_os"=sum(get(paste("DP",thirteen_months[6],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[6],sep="_"))),na.rm=TRUE),
                  "month_7_DP_os"=sum(get(paste("DP",thirteen_months[7],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[7],sep="_"))),na.rm=TRUE),
                  "month_8_DP_os"=sum(get(paste("DP",thirteen_months[8],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[8],sep="_"))),na.rm=TRUE),
                  "month_9_DP_os"=sum(get(paste("DP",thirteen_months[9],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[9],sep="_"))),na.rm=TRUE),
                  "month_10_DP_os"=sum(get(paste("DP",thirteen_months[10],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[10],sep="_"))),na.rm=TRUE),
                  "month_11_DP_os"=sum(get(paste("DP",thirteen_months[11],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[11],sep="_"))),na.rm=TRUE),
                  "month_12_DP_os"=sum(get(paste("DP",thirteen_months[12],sep="_"))*PAP_EUR/1000*(1-get(paste("DD.",thirteen_months[12],sep="_"))),na.rm=TRUE),
                  "month_1_DP_dd"=sum(get(paste("DP",thirteen_months[1],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[1],sep="_")),na.rm=TRUE),
                  "month_2_DP_dd"=sum(get(paste("DP",thirteen_months[2],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[2],sep="_")),na.rm=TRUE),
                  "month_3_DP_dd"=sum(get(paste("DP",thirteen_months[3],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[3],sep="_")),na.rm=TRUE),
                  "month_4_DP_dd"=sum(get(paste("DP",thirteen_months[4],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[4],sep="_")),na.rm=TRUE),
                  "month_5_DP_dd"=sum(get(paste("DP",thirteen_months[5],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[5],sep="_")),na.rm=TRUE),
                  "month_6_DP_dd"=sum(get(paste("DP",thirteen_months[6],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[6],sep="_")),na.rm=TRUE),
                  "month_7_DP_dd"=sum(get(paste("DP",thirteen_months[7],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[7],sep="_")),na.rm=TRUE),
                  "month_8_DP_dd"=sum(get(paste("DP",thirteen_months[8],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[8],sep="_")),na.rm=TRUE),
                  "month_9_DP_dd"=sum(get(paste("DP",thirteen_months[9],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[9],sep="_")),na.rm=TRUE),
                  "month_10_DP_dd"=sum(get(paste("DP",thirteen_months[10],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[10],sep="_")),na.rm=TRUE),
                  "month_11_DP_dd"=sum(get(paste("DP",thirteen_months[11],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[11],sep="_")),na.rm=TRUE),
                  "month_12_DP_dd"=sum(get(paste("DP",thirteen_months[12],sep="_"))*PAP_EUR/1000*get(paste("DD.",thirteen_months[12],sep="_")),na.rm=TRUE),
                  "month_1_DP"=sum(get(paste("DP",thirteen_months[1],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_2_DP"=sum(get(paste("DP",thirteen_months[2],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_3_DP"=sum(get(paste("DP",thirteen_months[3],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_4_DP"=sum(get(paste("DP",thirteen_months[4],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_5_DP"=sum(get(paste("DP",thirteen_months[5],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_6_DP"=sum(get(paste("DP",thirteen_months[6],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_7_DP"=sum(get(paste("DP",thirteen_months[7],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_8_DP"=sum(get(paste("DP",thirteen_months[8],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_9_DP"=sum(get(paste("DP",thirteen_months[9],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_10_DP"=sum(get(paste("DP",thirteen_months[10],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_11_DP"=sum(get(paste("DP",thirteen_months[11],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_12_DP"=sum(get(paste("DP",thirteen_months[12],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_1_lastDP"=sum(get(paste("lastDP",thirteen_months[1],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_2_lastDP"=sum(get(paste("lastDP",thirteen_months[2],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_3_lastDP"=sum(get(paste("lastDP",thirteen_months[3],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_4_lastDP"=sum(get(paste("lastDP",thirteen_months[4],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_5_lastDP"=sum(get(paste("lastDP",thirteen_months[5],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_6_lastDP"=sum(get(paste("lastDP",thirteen_months[6],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_7_lastDP"=sum(get(paste("lastDP",thirteen_months[7],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_8_lastDP"=sum(get(paste("lastDP",thirteen_months[8],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_9_lastDP"=sum(get(paste("lastDP",thirteen_months[9],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_10_lastDP"=sum(get(paste("lastDP",thirteen_months[10],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_11_lastDP"=sum(get(paste("lastDP",thirteen_months[11],sep="_"))*PAP_EUR/1000,na.rm=TRUE),
                  "month_12_lastDP"=sum(get(paste("lastDP",thirteen_months[12],sep="_"))*PAP_EUR/1000,na.rm=TRUE)
  ),by=list(Product.Line,DP.Family,Div,CustGr,Supplier)]
  ,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))

tableauCGvendor<-tableauCGvendor[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]
write.table(tableauCGvendor,file = ".././my_app/my_app_v16/tableauCGvendor.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendor,file = "X:/Nskep/Shiny_up_to_date/tableauCGvendor.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendor,file = "S:/Procurement/Commun/SIOP/Central org SIOP/N. R and shiny/my_app/my_app_v16/tableauCGvendor.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
#write.table(subset(lasttable,Product.Line=="VES"),file = "X:/Nskep/extractVES.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)

## Create a file with quantity

tableauCGvendorQty<-subset(
  lasttable[,list("month_-24_tot"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_"))),na.rm=TRUE),
                  "month_-23_tot"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_"))),na.rm=TRUE),
                  "month_-22_tot"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_"))),na.rm=TRUE),
                  "month_-21_tot"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_"))),na.rm=TRUE),
                  "month_-20_tot"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_"))),na.rm=TRUE),
                  "month_-19_tot"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_"))),na.rm=TRUE),
                  "month_-18_tot"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_"))),na.rm=TRUE),
                  "month_-17_tot"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_"))),na.rm=TRUE),
                  "month_-16_tot"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_"))),na.rm=TRUE),
                  "month_-15_tot"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_"))),na.rm=TRUE),
                  "month_-14_tot"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_"))),na.rm=TRUE),
                  "month_-13_tot"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_"))),na.rm=TRUE),
                  "month_-12_tot"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_"))),na.rm=TRUE),
                  "month_-11_tot"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_"))),na.rm=TRUE),
                  "month_-10_tot"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_"))),na.rm=TRUE),
                  "month_-9_tot"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_"))),na.rm=TRUE),
                  "month_-8_tot"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_"))),na.rm=TRUE),
                  "month_-7_tot"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_"))),na.rm=TRUE),
                  "month_-6_tot"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_"))),na.rm=TRUE),
                  "month_-5_tot"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_"))),na.rm=TRUE),
                  "month_-4_tot"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_"))),na.rm=TRUE),
                  "month_-3_tot"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_"))),na.rm=TRUE),
                  "month_-2_tot"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_"))),na.rm=TRUE),
                  "month_-1_tot"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_"))),na.rm=TRUE),
                  "month_-24_os"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_"))-get(paste("HDD",last_24m[1],sep="_"))),na.rm=TRUE),
                  "month_-23_os"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_"))-get(paste("HDD",last_24m[2],sep="_"))),na.rm=TRUE),
                  "month_-22_os"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_"))-get(paste("HDD",last_24m[3],sep="_"))),na.rm=TRUE),
                  "month_-21_os"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_"))-get(paste("HDD",last_24m[4],sep="_"))),na.rm=TRUE),
                  "month_-20_os"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_"))-get(paste("HDD",last_24m[5],sep="_"))),na.rm=TRUE),
                  "month_-19_os"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_"))-get(paste("HDD",last_24m[6],sep="_"))),na.rm=TRUE),
                  "month_-18_os"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_"))-get(paste("HDD",last_24m[7],sep="_"))),na.rm=TRUE),
                  "month_-17_os"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_"))-get(paste("HDD",last_24m[8],sep="_"))),na.rm=TRUE),
                  "month_-16_os"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_"))-get(paste("HDD",last_24m[9],sep="_"))),na.rm=TRUE),
                  "month_-15_os"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_"))-get(paste("HDD",last_24m[10],sep="_"))),na.rm=TRUE),
                  "month_-14_os"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_"))-get(paste("HDD",last_24m[11],sep="_"))),na.rm=TRUE),
                  "month_-13_os"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_"))-get(paste("HDD",last_24m[12],sep="_"))),na.rm=TRUE),
                  "month_-12_os"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_"))-get(paste("HDD",last_24m[13],sep="_"))),na.rm=TRUE),
                  "month_-11_os"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_"))-get(paste("HDD",last_24m[14],sep="_"))),na.rm=TRUE),
                  "month_-10_os"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_"))-get(paste("HDD",last_24m[15],sep="_"))),na.rm=TRUE),
                  "month_-9_os"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_"))-get(paste("HDD",last_24m[16],sep="_"))),na.rm=TRUE),
                  "month_-8_os"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_"))-get(paste("HDD",last_24m[17],sep="_"))),na.rm=TRUE),
                  "month_-7_os"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_"))-get(paste("HDD",last_24m[18],sep="_"))),na.rm=TRUE),
                  "month_-6_os"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_"))-get(paste("HDD",last_24m[19],sep="_"))),na.rm=TRUE),
                  "month_-5_os"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_"))-get(paste("HDD",last_24m[20],sep="_"))),na.rm=TRUE),
                  "month_-4_os"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_"))-get(paste("HDD",last_24m[21],sep="_"))),na.rm=TRUE),
                  "month_-3_os"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_"))-get(paste("HDD",last_24m[22],sep="_"))),na.rm=TRUE),
                  "month_-2_os"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_"))-get(paste("HDD",last_24m[23],sep="_"))),na.rm=TRUE),
                  "month_-1_os"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_"))-get(paste("HDD",last_24m[24],sep="_"))),na.rm=TRUE),
                  "month_-24_dd"=sum((get(paste("HDD",last_24m[1],sep="_"))),na.rm=TRUE),
                  "month_-23_dd"=sum((get(paste("HDD",last_24m[2],sep="_"))),na.rm=TRUE),
                  "month_-22_dd"=sum((get(paste("HDD",last_24m[3],sep="_"))),na.rm=TRUE),
                  "month_-21_dd"=sum((get(paste("HDD",last_24m[4],sep="_"))),na.rm=TRUE),
                  "month_-20_dd"=sum((get(paste("HDD",last_24m[5],sep="_"))),na.rm=TRUE),
                  "month_-19_dd"=sum((get(paste("HDD",last_24m[6],sep="_"))),na.rm=TRUE),
                  "month_-18_dd"=sum((get(paste("HDD",last_24m[7],sep="_"))),na.rm=TRUE),
                  "month_-17_dd"=sum((get(paste("HDD",last_24m[8],sep="_"))),na.rm=TRUE),
                  "month_-16_dd"=sum((get(paste("HDD",last_24m[9],sep="_"))),na.rm=TRUE),
                  "month_-15_dd"=sum((get(paste("HDD",last_24m[10],sep="_"))),na.rm=TRUE),
                  "month_-14_dd"=sum((get(paste("HDD",last_24m[11],sep="_"))),na.rm=TRUE),
                  "month_-13_dd"=sum((get(paste("HDD",last_24m[12],sep="_"))),na.rm=TRUE),
                  "month_-12_dd"=sum((get(paste("HDD",last_24m[13],sep="_"))),na.rm=TRUE),
                  "month_-11_dd"=sum((get(paste("HDD",last_24m[14],sep="_"))),na.rm=TRUE),
                  "month_-10_dd"=sum((get(paste("HDD",last_24m[15],sep="_"))),na.rm=TRUE),
                  "month_-9_dd"=sum((get(paste("HDD",last_24m[16],sep="_"))),na.rm=TRUE),
                  "month_-8_dd"=sum((get(paste("HDD",last_24m[17],sep="_"))),na.rm=TRUE),
                  "month_-7_dd"=sum((get(paste("HDD",last_24m[18],sep="_"))),na.rm=TRUE),
                  "month_-6_dd"=sum((get(paste("HDD",last_24m[19],sep="_"))),na.rm=TRUE),
                  "month_-5_dd"=sum((get(paste("HDD",last_24m[20],sep="_"))),na.rm=TRUE),
                  "month_-4_dd"=sum((get(paste("HDD",last_24m[21],sep="_"))),na.rm=TRUE),
                  "month_-3_dd"=sum((get(paste("HDD",last_24m[22],sep="_"))),na.rm=TRUE),
                  "month_-2_dd"=sum((get(paste("HDD",last_24m[23],sep="_"))),na.rm=TRUE),
                  "month_-1_dd"=sum((get(paste("HDD",last_24m[24],sep="_"))),na.rm=TRUE),
                  "month_1_DP_os"=sum(get(paste("DP",thirteen_months[1],sep="_"))*(1-get(paste("DD.",thirteen_months[1],sep="_"))),na.rm=TRUE),
                  "month_2_DP_os"=sum(get(paste("DP",thirteen_months[2],sep="_"))*(1-get(paste("DD.",thirteen_months[2],sep="_"))),na.rm=TRUE),
                  "month_3_DP_os"=sum(get(paste("DP",thirteen_months[3],sep="_"))*(1-get(paste("DD.",thirteen_months[3],sep="_"))),na.rm=TRUE),
                  "month_4_DP_os"=sum(get(paste("DP",thirteen_months[4],sep="_"))*(1-get(paste("DD.",thirteen_months[4],sep="_"))),na.rm=TRUE),
                  "month_5_DP_os"=sum(get(paste("DP",thirteen_months[5],sep="_"))*(1-get(paste("DD.",thirteen_months[5],sep="_"))),na.rm=TRUE),
                  "month_6_DP_os"=sum(get(paste("DP",thirteen_months[6],sep="_"))*(1-get(paste("DD.",thirteen_months[6],sep="_"))),na.rm=TRUE),
                  "month_7_DP_os"=sum(get(paste("DP",thirteen_months[7],sep="_"))*(1-get(paste("DD.",thirteen_months[7],sep="_"))),na.rm=TRUE),
                  "month_8_DP_os"=sum(get(paste("DP",thirteen_months[8],sep="_"))*(1-get(paste("DD.",thirteen_months[8],sep="_"))),na.rm=TRUE),
                  "month_9_DP_os"=sum(get(paste("DP",thirteen_months[9],sep="_"))*(1-get(paste("DD.",thirteen_months[9],sep="_"))),na.rm=TRUE),
                  "month_10_DP_os"=sum(get(paste("DP",thirteen_months[10],sep="_"))*(1-get(paste("DD.",thirteen_months[10],sep="_"))),na.rm=TRUE),
                  "month_11_DP_os"=sum(get(paste("DP",thirteen_months[11],sep="_"))*(1-get(paste("DD.",thirteen_months[11],sep="_"))),na.rm=TRUE),
                  "month_12_DP_os"=sum(get(paste("DP",thirteen_months[12],sep="_"))*(1-get(paste("DD.",thirteen_months[12],sep="_"))),na.rm=TRUE),
                  "month_1_DP_dd"=sum(get(paste("DP",thirteen_months[1],sep="_"))*get(paste("DD.",thirteen_months[1],sep="_")),na.rm=TRUE),
                  "month_2_DP_dd"=sum(get(paste("DP",thirteen_months[2],sep="_"))*get(paste("DD.",thirteen_months[2],sep="_")),na.rm=TRUE),
                  "month_3_DP_dd"=sum(get(paste("DP",thirteen_months[3],sep="_"))*get(paste("DD.",thirteen_months[3],sep="_")),na.rm=TRUE),
                  "month_4_DP_dd"=sum(get(paste("DP",thirteen_months[4],sep="_"))*get(paste("DD.",thirteen_months[4],sep="_")),na.rm=TRUE),
                  "month_5_DP_dd"=sum(get(paste("DP",thirteen_months[5],sep="_"))*get(paste("DD.",thirteen_months[5],sep="_")),na.rm=TRUE),
                  "month_6_DP_dd"=sum(get(paste("DP",thirteen_months[6],sep="_"))*get(paste("DD.",thirteen_months[6],sep="_")),na.rm=TRUE),
                  "month_7_DP_dd"=sum(get(paste("DP",thirteen_months[7],sep="_"))*get(paste("DD.",thirteen_months[7],sep="_")),na.rm=TRUE),
                  "month_8_DP_dd"=sum(get(paste("DP",thirteen_months[8],sep="_"))*get(paste("DD.",thirteen_months[8],sep="_")),na.rm=TRUE),
                  "month_9_DP_dd"=sum(get(paste("DP",thirteen_months[9],sep="_"))*get(paste("DD.",thirteen_months[9],sep="_")),na.rm=TRUE),
                  "month_10_DP_dd"=sum(get(paste("DP",thirteen_months[10],sep="_"))*get(paste("DD.",thirteen_months[10],sep="_")),na.rm=TRUE),
                  "month_11_DP_dd"=sum(get(paste("DP",thirteen_months[11],sep="_"))*get(paste("DD.",thirteen_months[11],sep="_")),na.rm=TRUE),
                  "month_12_DP_dd"=sum(get(paste("DP",thirteen_months[12],sep="_"))*get(paste("DD.",thirteen_months[12],sep="_")),na.rm=TRUE),
                  "month_1_DP"=sum(get(paste("DP",thirteen_months[1],sep="_")),na.rm=TRUE),
                  "month_2_DP"=sum(get(paste("DP",thirteen_months[2],sep="_")),na.rm=TRUE),
                  "month_3_DP"=sum(get(paste("DP",thirteen_months[3],sep="_")),na.rm=TRUE),
                  "month_4_DP"=sum(get(paste("DP",thirteen_months[4],sep="_")),na.rm=TRUE),
                  "month_5_DP"=sum(get(paste("DP",thirteen_months[5],sep="_")),na.rm=TRUE),
                  "month_6_DP"=sum(get(paste("DP",thirteen_months[6],sep="_")),na.rm=TRUE),
                  "month_7_DP"=sum(get(paste("DP",thirteen_months[7],sep="_")),na.rm=TRUE),
                  "month_8_DP"=sum(get(paste("DP",thirteen_months[8],sep="_")),na.rm=TRUE),
                  "month_9_DP"=sum(get(paste("DP",thirteen_months[9],sep="_")),na.rm=TRUE),
                  "month_10_DP"=sum(get(paste("DP",thirteen_months[10],sep="_")),na.rm=TRUE),
                  "month_11_DP"=sum(get(paste("DP",thirteen_months[11],sep="_")),na.rm=TRUE),
                  "month_12_DP"=sum(get(paste("DP",thirteen_months[12],sep="_")),na.rm=TRUE),
                  "month_1_lastDP"=sum(get(paste("lastDP",thirteen_months[1],sep="_")),na.rm=TRUE),
                  "month_2_lastDP"=sum(get(paste("lastDP",thirteen_months[2],sep="_")),na.rm=TRUE),
                  "month_3_lastDP"=sum(get(paste("lastDP",thirteen_months[3],sep="_")),na.rm=TRUE),
                  "month_4_lastDP"=sum(get(paste("lastDP",thirteen_months[4],sep="_")),na.rm=TRUE),
                  "month_5_lastDP"=sum(get(paste("lastDP",thirteen_months[5],sep="_")),na.rm=TRUE),
                  "month_6_lastDP"=sum(get(paste("lastDP",thirteen_months[6],sep="_")),na.rm=TRUE),
                  "month_7_lastDP"=sum(get(paste("lastDP",thirteen_months[7],sep="_")),na.rm=TRUE),
                  "month_8_lastDP"=sum(get(paste("lastDP",thirteen_months[8],sep="_")),na.rm=TRUE),
                  "month_9_lastDP"=sum(get(paste("lastDP",thirteen_months[9],sep="_")),na.rm=TRUE),
                  "month_10_lastDP"=sum(get(paste("lastDP",thirteen_months[10],sep="_")),na.rm=TRUE),
                  "month_11_lastDP"=sum(get(paste("lastDP",thirteen_months[11],sep="_")),na.rm=TRUE),
                  "month_12_lastDP"=sum(get(paste("lastDP",thirteen_months[12],sep="_")),na.rm=TRUE)
  ),by=list(Product.Line,DP.Family,Div,CustGr,Supplier)]
  ,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))

tableauCGvendorQty<-tableauCGvendorQty[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]
write.table(tableauCGvendorQty,file = ".././my_app/my_app_v16/tableauCGvendorQty.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendorQty,file = "X:/Nskep/Shiny_up_to_date/tableauCGvendorQty.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendorQty,file = "S:/Procurement/Commun/SIOP/Central org SIOP/N. R and shiny/my_app/my_app_v16/tableauCGvendorQty.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)


## Create a file with Semi Net Price

tableauCGvendorSemNet<-subset(
  lasttable[,list("month_-24_tot"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-23_tot"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-22_tot"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-21_tot"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-20_tot"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-19_tot"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-18_tot"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-17_tot"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-16_tot"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-15_tot"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-14_tot"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-13_tot"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-12_tot"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-11_tot"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-10_tot"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-9_tot"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-8_tot"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-7_tot"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-6_tot"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-5_tot"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-4_tot"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-3_tot"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-2_tot"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-1_tot"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-24_os"=sum((get(paste("HDN",last_24m[1],sep="_"))+get(paste("HDE",last_24m[1],sep="_"))-get(paste("HDD",last_24m[1],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-23_os"=sum((get(paste("HDN",last_24m[2],sep="_"))+get(paste("HDE",last_24m[2],sep="_"))-get(paste("HDD",last_24m[2],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-22_os"=sum((get(paste("HDN",last_24m[3],sep="_"))+get(paste("HDE",last_24m[3],sep="_"))-get(paste("HDD",last_24m[3],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-21_os"=sum((get(paste("HDN",last_24m[4],sep="_"))+get(paste("HDE",last_24m[4],sep="_"))-get(paste("HDD",last_24m[4],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-20_os"=sum((get(paste("HDN",last_24m[5],sep="_"))+get(paste("HDE",last_24m[5],sep="_"))-get(paste("HDD",last_24m[5],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-19_os"=sum((get(paste("HDN",last_24m[6],sep="_"))+get(paste("HDE",last_24m[6],sep="_"))-get(paste("HDD",last_24m[6],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-18_os"=sum((get(paste("HDN",last_24m[7],sep="_"))+get(paste("HDE",last_24m[7],sep="_"))-get(paste("HDD",last_24m[7],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-17_os"=sum((get(paste("HDN",last_24m[8],sep="_"))+get(paste("HDE",last_24m[8],sep="_"))-get(paste("HDD",last_24m[8],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-16_os"=sum((get(paste("HDN",last_24m[9],sep="_"))+get(paste("HDE",last_24m[9],sep="_"))-get(paste("HDD",last_24m[9],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-15_os"=sum((get(paste("HDN",last_24m[10],sep="_"))+get(paste("HDE",last_24m[10],sep="_"))-get(paste("HDD",last_24m[10],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-14_os"=sum((get(paste("HDN",last_24m[11],sep="_"))+get(paste("HDE",last_24m[11],sep="_"))-get(paste("HDD",last_24m[11],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-13_os"=sum((get(paste("HDN",last_24m[12],sep="_"))+get(paste("HDE",last_24m[12],sep="_"))-get(paste("HDD",last_24m[12],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-12_os"=sum((get(paste("HDN",last_24m[13],sep="_"))+get(paste("HDE",last_24m[13],sep="_"))-get(paste("HDD",last_24m[13],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-11_os"=sum((get(paste("HDN",last_24m[14],sep="_"))+get(paste("HDE",last_24m[14],sep="_"))-get(paste("HDD",last_24m[14],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-10_os"=sum((get(paste("HDN",last_24m[15],sep="_"))+get(paste("HDE",last_24m[15],sep="_"))-get(paste("HDD",last_24m[15],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-9_os"=sum((get(paste("HDN",last_24m[16],sep="_"))+get(paste("HDE",last_24m[16],sep="_"))-get(paste("HDD",last_24m[16],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-8_os"=sum((get(paste("HDN",last_24m[17],sep="_"))+get(paste("HDE",last_24m[17],sep="_"))-get(paste("HDD",last_24m[17],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-7_os"=sum((get(paste("HDN",last_24m[18],sep="_"))+get(paste("HDE",last_24m[18],sep="_"))-get(paste("HDD",last_24m[18],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-6_os"=sum((get(paste("HDN",last_24m[19],sep="_"))+get(paste("HDE",last_24m[19],sep="_"))-get(paste("HDD",last_24m[19],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-5_os"=sum((get(paste("HDN",last_24m[20],sep="_"))+get(paste("HDE",last_24m[20],sep="_"))-get(paste("HDD",last_24m[20],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-4_os"=sum((get(paste("HDN",last_24m[21],sep="_"))+get(paste("HDE",last_24m[21],sep="_"))-get(paste("HDD",last_24m[21],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-3_os"=sum((get(paste("HDN",last_24m[22],sep="_"))+get(paste("HDE",last_24m[22],sep="_"))-get(paste("HDD",last_24m[22],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-2_os"=sum((get(paste("HDN",last_24m[23],sep="_"))+get(paste("HDE",last_24m[23],sep="_"))-get(paste("HDD",last_24m[23],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-1_os"=sum((get(paste("HDN",last_24m[24],sep="_"))+get(paste("HDE",last_24m[24],sep="_"))-get(paste("HDD",last_24m[24],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-24_dd"=sum((get(paste("HDD",last_24m[1],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-23_dd"=sum((get(paste("HDD",last_24m[2],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-22_dd"=sum((get(paste("HDD",last_24m[3],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-21_dd"=sum((get(paste("HDD",last_24m[4],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-20_dd"=sum((get(paste("HDD",last_24m[5],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-19_dd"=sum((get(paste("HDD",last_24m[6],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-18_dd"=sum((get(paste("HDD",last_24m[7],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-17_dd"=sum((get(paste("HDD",last_24m[8],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-16_dd"=sum((get(paste("HDD",last_24m[9],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-15_dd"=sum((get(paste("HDD",last_24m[10],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-14_dd"=sum((get(paste("HDD",last_24m[11],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-13_dd"=sum((get(paste("HDD",last_24m[12],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-12_dd"=sum((get(paste("HDD",last_24m[13],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-11_dd"=sum((get(paste("HDD",last_24m[14],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-10_dd"=sum((get(paste("HDD",last_24m[15],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-9_dd"=sum((get(paste("HDD",last_24m[16],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-8_dd"=sum((get(paste("HDD",last_24m[17],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-7_dd"=sum((get(paste("HDD",last_24m[18],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-6_dd"=sum((get(paste("HDD",last_24m[19],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-5_dd"=sum((get(paste("HDD",last_24m[20],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-4_dd"=sum((get(paste("HDD",last_24m[21],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-3_dd"=sum((get(paste("HDD",last_24m[22],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-2_dd"=sum((get(paste("HDD",last_24m[23],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_-1_dd"=sum((get(paste("HDD",last_24m[24],sep="_")))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_1_DP_os"=sum(get(paste("DP",thirteen_months[1],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[1],sep="_"))),na.rm=TRUE),
                  "month_2_DP_os"=sum(get(paste("DP",thirteen_months[2],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[2],sep="_"))),na.rm=TRUE),
                  "month_3_DP_os"=sum(get(paste("DP",thirteen_months[3],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[3],sep="_"))),na.rm=TRUE),
                  "month_4_DP_os"=sum(get(paste("DP",thirteen_months[4],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[4],sep="_"))),na.rm=TRUE),
                  "month_5_DP_os"=sum(get(paste("DP",thirteen_months[5],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[5],sep="_"))),na.rm=TRUE),
                  "month_6_DP_os"=sum(get(paste("DP",thirteen_months[6],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[6],sep="_"))),na.rm=TRUE),
                  "month_7_DP_os"=sum(get(paste("DP",thirteen_months[7],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[7],sep="_"))),na.rm=TRUE),
                  "month_8_DP_os"=sum(get(paste("DP",thirteen_months[8],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[8],sep="_"))),na.rm=TRUE),
                  "month_9_DP_os"=sum(get(paste("DP",thirteen_months[9],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[9],sep="_"))),na.rm=TRUE),
                  "month_10_DP_os"=sum(get(paste("DP",thirteen_months[10],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[10],sep="_"))),na.rm=TRUE),
                  "month_11_DP_os"=sum(get(paste("DP",thirteen_months[11],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[11],sep="_"))),na.rm=TRUE),
                  "month_12_DP_os"=sum(get(paste("DP",thirteen_months[12],sep="_"))*SemNet_EUR/1000*(1-get(paste("DD.",thirteen_months[12],sep="_"))),na.rm=TRUE),
                  "month_1_DP_dd"=sum(get(paste("DP",thirteen_months[1],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[1],sep="_")),na.rm=TRUE),
                  "month_2_DP_dd"=sum(get(paste("DP",thirteen_months[2],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[2],sep="_")),na.rm=TRUE),
                  "month_3_DP_dd"=sum(get(paste("DP",thirteen_months[3],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[3],sep="_")),na.rm=TRUE),
                  "month_4_DP_dd"=sum(get(paste("DP",thirteen_months[4],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[4],sep="_")),na.rm=TRUE),
                  "month_5_DP_dd"=sum(get(paste("DP",thirteen_months[5],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[5],sep="_")),na.rm=TRUE),
                  "month_6_DP_dd"=sum(get(paste("DP",thirteen_months[6],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[6],sep="_")),na.rm=TRUE),
                  "month_7_DP_dd"=sum(get(paste("DP",thirteen_months[7],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[7],sep="_")),na.rm=TRUE),
                  "month_8_DP_dd"=sum(get(paste("DP",thirteen_months[8],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[8],sep="_")),na.rm=TRUE),
                  "month_9_DP_dd"=sum(get(paste("DP",thirteen_months[9],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[9],sep="_")),na.rm=TRUE),
                  "month_10_DP_dd"=sum(get(paste("DP",thirteen_months[10],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[10],sep="_")),na.rm=TRUE),
                  "month_11_DP_dd"=sum(get(paste("DP",thirteen_months[11],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[11],sep="_")),na.rm=TRUE),
                  "month_12_DP_dd"=sum(get(paste("DP",thirteen_months[12],sep="_"))*SemNet_EUR/1000*get(paste("DD.",thirteen_months[12],sep="_")),na.rm=TRUE),
                  "month_1_DP"=sum(get(paste("DP",thirteen_months[1],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_2_DP"=sum(get(paste("DP",thirteen_months[2],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_3_DP"=sum(get(paste("DP",thirteen_months[3],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_4_DP"=sum(get(paste("DP",thirteen_months[4],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_5_DP"=sum(get(paste("DP",thirteen_months[5],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_6_DP"=sum(get(paste("DP",thirteen_months[6],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_7_DP"=sum(get(paste("DP",thirteen_months[7],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_8_DP"=sum(get(paste("DP",thirteen_months[8],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_9_DP"=sum(get(paste("DP",thirteen_months[9],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_10_DP"=sum(get(paste("DP",thirteen_months[10],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_11_DP"=sum(get(paste("DP",thirteen_months[11],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_12_DP"=sum(get(paste("DP",thirteen_months[12],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_1_lastDP"=sum(get(paste("lastDP",thirteen_months[1],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_2_lastDP"=sum(get(paste("lastDP",thirteen_months[2],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_3_lastDP"=sum(get(paste("lastDP",thirteen_months[3],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_4_lastDP"=sum(get(paste("lastDP",thirteen_months[4],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_5_lastDP"=sum(get(paste("lastDP",thirteen_months[5],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_6_lastDP"=sum(get(paste("lastDP",thirteen_months[6],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_7_lastDP"=sum(get(paste("lastDP",thirteen_months[7],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_8_lastDP"=sum(get(paste("lastDP",thirteen_months[8],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_9_lastDP"=sum(get(paste("lastDP",thirteen_months[9],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_10_lastDP"=sum(get(paste("lastDP",thirteen_months[10],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_11_lastDP"=sum(get(paste("lastDP",thirteen_months[11],sep="_"))*SemNet_EUR/1000,na.rm=TRUE),
                  "month_12_lastDP"=sum(get(paste("lastDP",thirteen_months[12],sep="_"))*SemNet_EUR/1000,na.rm=TRUE)
  ),by=list(Product.Line,DP.Family,Div,CustGr,Supplier)]
  ,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))

tableauCGvendorSemNet<-tableauCGvendorSemNet[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]
write.table(tableauCGvendorSemNet,file = ".././my_app/my_app_v16/tableauCGvendorSemNet.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendorSemNet,file = "X:/Nskep/Shiny_up_to_date/tableauCGvendorSemNet.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
write.table(tableauCGvendorSemNet,file = "S:/Procurement/Commun/SIOP/Central org SIOP/N. R and shiny/my_app/my_app_v16/tableauCGvendorSemNet.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
