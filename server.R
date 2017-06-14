#### Import libraries
library(shiny)
library(plyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(data.table)
library(TeachingDemos)

####In case it is not launched from the batch, specify the working directory
#setwd("C:\\Users\\glaboure\\Documents\\R\\my_app\\my_app_v15")

##### Import data
tableau<-fread("tableauCGvendor.csv",dec=",",colClasses =c("integer",rep("factor",5),rep("numeric",120)),stringsAsFactors=T,drop=1)
tableauqty<-fread("tableauCGvendorQty.csv",dec=",",colClasses =c("integer",rep("factor",5),rep("numeric",120)),stringsAsFactors=T,drop=1)
tableauSemNet<-fread("tableauCGvendorSemnet.csv",dec=",",colClasses =c("integer",rep("factor",5),rep("numeric",120)),stringsAsFactors=T,drop=1)
DPlist<-levels(tableau$DP.Family)
vendorlist<-fread("SupplierName.txt",colClasses =c("integer",rep("character",2)))
options(scipen=3) ### don't use science number format


###### Start of function to apply group by and filter and sort data
####
apply_group_by<-function(data,input_grpby,input_num,input_ProdLine,input_division,input_Supplier,input_DPfamily,input_customer_group,input_IVSexcl)
{
  ####aggregate data at chosen level - note that IVS is excluded in case none of Div, Supplier and CustGr is chosen or tickbox is ticked
  if((sum(grep("Div", input_grpby))==0 & sum(grep("Supplier", input_grpby))==0 & sum(grep("CustGr", input_grpby))==0)|sum(grep("IVSexcl", input_IVSexcl))!=0){data<-subset(tableau,!(substr(CustGr,4,6) %in% c("BRT","GEE","SAA","HED","REH","ISL","OCW","MOW")))}else{data<-tableau} 
  data<-data[,lapply(.SD,sum), by=eval(input_grpby), .SDcols=
              c(paste0("month_-",24:1,"_tot"),paste0("month_-",24:1,"_os"),paste0("month_-",24:1,"_dd"),
                paste0("month_",1:12,"_DP_os"),paste0("month_",1:12,"_DP_dd"),paste0("month_",1:12,"_DP"),paste0("month_",1:12,"_lastDP")
              )]
 ####filter data
 try(if(sum(grep("Product.Line", input_grpby))!=0){if(input_ProdLine=="All"){}else{data<-subset(data,Product.Line==input_ProdLine)}},silent=TRUE)
 try(if(sum(grep("Div", input_grpby))!=0){if(input_division=="All"){}else{data<-subset(data,Div==input_division)}},silent=TRUE)
 try(if(sum(grep("Supplier", input_grpby))!=0){if(input_Supplier=="All"){}else{data<-subset(data,Supplier==input_Supplier)}},silent=TRUE)
 try(if(sum(grep("DP.Family", input_grpby))!=0){if(input_DPfamily=="All"){}else{data<-subset(data,DP.Family==input_DPfamily)}},silent=TRUE)
 try(if(input_customer_group=="All"){}else{data<-subset(data,CustGr==input_customer_group)},silent=TRUE)
 
 ####order data in descending order
 data[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]    
}

####
###### End of function to apply group by and filter and sort data

###### Start of function to apply group by and filter and sort data for quantity
####
apply_group_byQty<-function(dataqty,input_grpby,input_num,input_ProdLine,input_division,input_Supplier,input_DPfamily,input_customer_group,input_IVSexcl)
{
  ####aggregate dataqty at chosen level - note that IVS is excluded in case none of Div, Supplier and CustGr is chosen or tickbox is ticked
  if((sum(grep("Div", input_grpby))==0 & sum(grep("Supplier", input_grpby))==0 & sum(grep("CustGr", input_grpby))==0)|sum(grep("IVSexcl", input_IVSexcl))!=0){dataqty<-subset(tableauqty,!(substr(CustGr,4,6) %in% c("BRT","GEE","SAA","HED","REH","ISL","OCW","MOW")))}else{dataqty<-tableauqty} 
  dataqty<-dataqty[,lapply(.SD,sum), by=eval(input_grpby), .SDcols=
              c(paste0("month_-",24:1,"_tot"),paste0("month_-",24:1,"_os"),paste0("month_-",24:1,"_dd"),
                paste0("month_",1:12,"_DP_os"),paste0("month_",1:12,"_DP_dd"),paste0("month_",1:12,"_DP"),paste0("month_",1:12,"_lastDP")
              )]
 ####filter dataqty
 try(if(sum(grep("Product.Line", input_grpby))!=0){if(input_ProdLine=="All"){}else{dataqty<-subset(dataqty,Product.Line==input_ProdLine)}},silent=TRUE)
 try(if(sum(grep("Div", input_grpby))!=0){if(input_division=="All"){}else{dataqty<-subset(dataqty,Div==input_division)}},silent=TRUE)
 try(if(sum(grep("Supplier", input_grpby))!=0){if(input_Supplier=="All"){}else{dataqty<-subset(dataqty,Supplier==input_Supplier)}},silent=TRUE)
 try(if(sum(grep("DP.Family", input_grpby))!=0){if(input_DPfamily=="All"){}else{dataqty<-subset(dataqty,DP.Family==input_DPfamily)}},silent=TRUE)
 try(if(input_customer_group=="All"){}else{dataqty<-subset(dataqty,CustGr==input_customer_group)},silent=TRUE)
 
 ####order dataqty in descending order
 dataqty[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]    
}

####
###### End of function to apply group by and filter and sort data for quantity

###### Start of function to apply group by and filter and sort data for SemNet
####
apply_group_bySemNet<-function(dataSemNet,input_grpby,input_num,input_ProdLine,input_division,input_Supplier,input_DPfamily,input_customer_group,input_IVSexcl)
{
  ####aggregate dataSemNet at chosen level - note that IVS is excluded in case none of Div, Supplier and CustGr is chosen or tickbox is ticked
  if((sum(grep("Div", input_grpby))==0 & sum(grep("Supplier", input_grpby))==0 & sum(grep("CustGr", input_grpby))==0)|sum(grep("IVSexcl", input_IVSexcl))!=0){dataSemNet<-subset(tableauSemNet,!(substr(CustGr,4,6) %in% c("BRT","GEE","SAA","HED","REH","ISL","OCW","MOW")))}else{dataSemNet<-tableauSemNet} 
  dataSemNet<-dataSemNet[,lapply(.SD,sum), by=eval(input_grpby), .SDcols=
              c(paste0("month_-",24:1,"_tot"),paste0("month_-",24:1,"_os"),paste0("month_-",24:1,"_dd"),
                paste0("month_",1:12,"_DP_os"),paste0("month_",1:12,"_DP_dd"),paste0("month_",1:12,"_DP"),paste0("month_",1:12,"_lastDP")
              )]
 ####filter dataSemNet
 try(if(sum(grep("Product.Line", input_grpby))!=0){if(input_ProdLine=="All"){}else{dataSemNet<-subset(dataSemNet,Product.Line==input_ProdLine)}},silent=TRUE)
 try(if(sum(grep("Div", input_grpby))!=0){if(input_division=="All"){}else{dataSemNet<-subset(dataSemNet,Div==input_division)}},silent=TRUE)
 try(if(sum(grep("Supplier", input_grpby))!=0){if(input_Supplier=="All"){}else{dataSemNet<-subset(dataSemNet,Supplier==input_Supplier)}},silent=TRUE)
 try(if(sum(grep("DP.Family", input_grpby))!=0){if(input_DPfamily=="All"){}else{dataSemNet<-subset(dataSemNet,DP.Family==input_DPfamily)}},silent=TRUE)
 try(if(input_customer_group=="All"){}else{dataSemNet<-subset(dataSemNet,CustGr==input_customer_group)},silent=TRUE)
 
 ####order dataSemNet in descending order
 dataSemNet[order(-month_1_DP-month_2_DP-month_3_DP-month_4_DP-month_5_DP-month_6_DP-month_1_lastDP-month_2_lastDP-month_3_lastDP-month_4_lastDP-month_5_lastDP-month_6_lastDP),]    
}

####
###### End of function to apply group by and filter and sort data for SemNet

##### detemine 13 next months and 24 last months
#####
date<-Sys.Date()
# date character string containing POSIXct date
date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
mon<-list()
year<-list()
thirteen_months<-list()
## IF YOU WANT TO USE LAST MONTH DATA ADD -1 : mon[1] <- date.lt$mon-1
mon[1] <- date.lt$mon
## IF YOU WANT TO USE LAST MONTH DATA ADD -1 : mon[1] <- date.lt$mon-1
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
## IF YOU WANT TO USE LAST MONTH DATA REMOVE +1 : mon[1] <- date.lt$mon
mon[1] <- date.lt$mon+1
## IF YOU WANT TO USE LAST MONTH DATA REMOVE +1 : mon[1] <- date.lt$mon
year[1] <- date.lt$year
for (i in 1:24){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  last_24m[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
last_24m<-unlist(rev(last_24m))

#####


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

####Uncomment to have datatable below the graphes (and uncomment corresponding line in ui.r)
#    output$data0 <- renderDataTable({
#    data00<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
#    data00 
#    })
#  
  
  ####Plot 1st graph
  output$distPlot <- renderPlot({

    data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    ####prepare data1 for matplot + round value for labels
    data1<-cbind(c(as.character(data[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(data[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F])),c(rep(NA,24),as.character(data[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F])),c(rep(NA,24),as.character(data[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F])))
    data1round<-cbind(c(as.character(round(data[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(data[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(data[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(data[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F]))))
    #define y plot range to normalise label above point
    y_range<-min(max(as.numeric(data1)*1.15,na.rm=T),ranges$y[2])-max(0,ranges$y[1])
    x_range<-min(36,ranges$x[2])-max(1,ranges$x[1])
    data1y3<-as.numeric(data1[,3])
    data1y4<-as.numeric(data1[,4])
    data1y3[data1y3>data1y4&!is.na(data1y3)]<-data1y3[data1y3>data1y4&!is.na(data1y3)]+y_range/20
    data1y4[data1y4>data1y3&!is.na(data1y4)]<-data1y4[data1y4>data1y3&!is.na(data1y4)]+y_range/20
    
    #define data source for cumulative data calculation
    data1C2<-as.numeric(c(rep(NA,12),data1[,1][1:24]))
    data1C3<-as.numeric(c(rep(NA,12),data1[,1][13:24],data1[,3][25:36]))
    data1C4<-as.numeric(c(rep(NA,12),data1[,1][13:24],data1[,4][25:36]))
    
    ####plot data1
    par(mar=c(2.5,4,1,1))  
    matplot(NA,xaxt = "n",yaxt = "n",ylab="Value (Keuro)",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(data1)*1.15,na.rm=T),ranges$y[2])))
    #matplot(data1,xaxt = "n",yaxt = "n",type="bbbb",pch=c(19,1,19,19),lty=c(1,3,2,2),col=c("#000000","#555555","#CC79A7","#009E73"),ylab="Value (Keuro)",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(data1)*1.15,na.rm=T),ranges$y[2])))
    try(if(sum(grep("last_year", input$lines))!=0){matpoints(data1[,2],type="b",pch=1,lty=3,col="#555555")})
    try(if(sum(grep("hist_tot", input$lines))!=0){matpoints(data1[,1],type="b",pch=19,lty=1,col="#000000")})
    try(if(sum(grep("DPM1", input$lines))!=0){matpoints(data1[,4],type="b",pch=19,lty=2,col="#009E73")})
    try(if(sum(grep("new_DP", input$lines))!=0){matpoints(data1[,3],type="b",pch=19,lty=2,col="#CC79A7")})
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist tot","last year", "new DP", "DP M-1"), pch=c(19,1,19,19), col=c("#000000","#555555","#CC79A7","#009E73"),horiz=TRUE)
    
    #####data labels on points
    try(if(sum(grep("hist_tot", input$labels))!=0){shadowtext(data1[,1], labels = data1round[,1],pos=1,col="#000000",r=0.2,bg = "white")})
    try(if(sum(grep("last_year", input$labels))!=0){shadowtext(data1[,2], labels = data1round[,2],pos=1,col="#555555",r=0.2,bg = "white")})
    try(if(sum(grep("DPM1", input$labels))!=0){shadowtext(data1y4, labels = data1round[,4],pos=1,col="#009E73",r=0.2,bg = "white")})
    try(if(sum(grep("new_DP", input$labels))!=0){shadowtext(data1y3, labels = data1round[,3],pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    
    try(if(min(floor(input$plot_brush$xmax),36)>12){
    
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-y_range/20,max(ranges$y[1],0)+2.5*y_range/20), 
                                                           labels = formatC(sum(data1C3[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#CC79A7",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-2*y_range/20,max(ranges$y[1],0)+1.5*y_range/20), 
                                                           labels = formatC(sum(data1C4[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#009E73",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-3*y_range/20,max(ranges$y[1],0)+0.5*y_range/20), 
                                                           labels = formatC(sum(data1C2[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#555555",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/12), y=max(input$plot_brush$ymin,max(ranges$y[1],0)+3.5*y_range/20), 
                                                           labels = paste(paste(substr(c(last_24m,thirteen_months[1:12])[c(max(ceiling(input$plot_brush$xmin),13),min(floor(input$plot_brush$xmax),36))],1,6),collapse=" to "),":"),
                                                           pos=1,col="#000000",r=0.2,bg = "white")},silent=T)
    },silent=T)
    
    #try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=5,y=y_range/2, labels = str(input$plot_brush$xmin),pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    par(omi=c(0,10,0,0))
    
  })

  ####Plot 2nd graph (same logic as 1st graph)
  output$distPlot2 <- renderPlot({
    
    data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
        
    data2<-cbind(c(as.character(data[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F]),rep(NA,12)),c(as.character(data[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(data[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F])),c(rep(NA,24),as.character(data[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F])))
    data2round<-cbind(c(as.character(round(data[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F])),rep(NA,12)),c(as.character(round(data[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(data[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(data[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F]))))
    
    
    par(mar=c(2.5,4,2,1))  
    matplot(data2,xaxt = "n",yaxt = "n",type="bbbb",pch=19, lty=c(1,1,2,2), col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),ylab="Value (Keuro)",xlim=c(max(1,ranges2$x[1]),min(36,ranges2$x[2])),ylim=c(max(0,ranges2$y[1]),min(max(as.numeric(data2)*1.15,na.rm=T),ranges2$y[2])))
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist OS", "hist DD", "DP OS", "DP DD"), pch=19, col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),horiz=TRUE)
    
    #####data labels on points
    try(if(sum(grep("hist_OS", input$labels))!=0){shadowtext(data2[,1], labels=data2round[,1],pos=1,col="#56B4E9",r=0.2,bg = "white")})
    try(if(sum(grep("hist_DD", input$labels))!=0){shadowtext(data2[,2], labels = data2round[,2],pos=1,col="#E69F00",r=0.2,bg = "white")})
    try(if(sum(grep("DP_OS", input$labels))!=0){shadowtext(data2[,3], labels = data2round[,3],pos=1,col="#0072B2",r=0.2,bg = "white")})
    try(if(sum(grep("DP_DD", input$labels))!=0){shadowtext(data2[,4], labels = data2round[,4],pos=1,col="#D55E00",r=0.2,bg = "white")})
    
      
  })
  
 #### Plot the graph for quantity####
 
   ####Plot 3rd graph
    output$distPlot3 <- renderPlot({

    dataqty<-apply_group_byQty(dataqty,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    ####prepare dataqty3 for matplot + round value for labels
    dataqty3<-cbind(c(as.character(dataqty[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(dataqty[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataqty[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataqty[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F])))
    dataqty3round<-cbind(c(as.character(round(dataqty[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(dataqty[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataqty[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataqty[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F]))))
    #define y plot range to normalise label above point
    y_range<-min(max(as.numeric(dataqty3)*1.15,na.rm=T),ranges$y[2])-max(0,ranges$y[1])
    x_range<-min(36,ranges$x[2])-max(1,ranges$x[1])
    dataqty3y3<-as.numeric(dataqty3[,3])
    dataqty3y4<-as.numeric(dataqty3[,4])
    dataqty3y3[dataqty3y3>dataqty3y4&!is.na(dataqty3y3)]<-dataqty3y3[dataqty3y3>dataqty3y4&!is.na(dataqty3y3)]+y_range/20
    dataqty3y4[dataqty3y4>dataqty3y3&!is.na(dataqty3y4)]<-dataqty3y4[dataqty3y4>dataqty3y3&!is.na(dataqty3y4)]+y_range/20
    
    #define dataqty source for cumulative dataqty calculation
    dataqty3C2<-as.numeric(c(rep(NA,12),dataqty3[,1][1:24]))
    dataqty3C3<-as.numeric(c(rep(NA,12),dataqty3[,1][13:24],dataqty3[,3][25:36]))
    dataqty3C4<-as.numeric(c(rep(NA,12),dataqty3[,1][13:24],dataqty3[,4][25:36]))
    
    ####plot dataqty3
    par(mar=c(2.5,4,1,1))  
    matplot(NA,xaxt = "n",yaxt = "n",ylab="Quantity",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(dataqty3)*1.15,na.rm=T),ranges$y[2])))
    #matplot(dataqty3,xaxt = "n",yaxt = "n",type="bbbb",pch=c(19,1,19,19),lty=c(1,3,2,2),col=c("#000000","#555555","#CC79A7","#009E73"),ylab="Quantity",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(dataqty3),na.rm=T),ranges$y[2])))
    try(if(sum(grep("last_year", input$lines))!=0){matpoints(dataqty3[,2],type="b",pch=1,lty=3,col="#555555")})
    try(if(sum(grep("hist_tot", input$lines))!=0){matpoints(dataqty3[,1],type="b",pch=19,lty=1,col="#000000")})
    try(if(sum(grep("DPM1", input$lines))!=0){matpoints(dataqty3[,4],type="b",pch=19,lty=2,col="#009E73")})
    try(if(sum(grep("new_DP", input$lines))!=0){matpoints(dataqty3[,3],type="b",pch=19,lty=2,col="#CC79A7")})
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist tot","last year", "new DP", "DP M-1"), pch=c(19,1,19,19), col=c("#000000","#555555","#CC79A7","#009E73"),horiz=TRUE)
    
    #####dataqty labels on points
    try(if(sum(grep("hist_tot", input$labels))!=0){shadowtext(dataqty3[,1], labels = dataqty3round[,1],pos=1,col="#000000",r=0.2,bg = "white")})
    try(if(sum(grep("last_year", input$labels))!=0){shadowtext(dataqty3[,2], labels = dataqty3round[,2],pos=1,col="#555555",r=0.2,bg = "white")})
    try(if(sum(grep("DPM1", input$labels))!=0){shadowtext(dataqty3y4, labels = dataqty3round[,4],pos=1,col="#009E73",r=0.2,bg = "white")})
    try(if(sum(grep("new_DP", input$labels))!=0){shadowtext(dataqty3y3, labels = dataqty3round[,3],pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    
    try(if(min(floor(input$plot_brush$xmax),36)>12){
    
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-y_range/20,max(ranges$y[1],0)+2.5*y_range/20), 
                                                           labels = formatC(sum(dataqty3C3[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#CC79A7",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-2*y_range/20,max(ranges$y[1],0)+1.5*y_range/20), 
                                                           labels = formatC(sum(dataqty3C4[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#009E73",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-3*y_range/20,max(ranges$y[1],0)+0.5*y_range/20), 
                                                           labels = formatC(sum(dataqty3C2[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#555555",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/12), y=max(input$plot_brush$ymin,max(ranges$y[1],0)+3.5*y_range/20), 
                                                           labels = paste(paste(substr(c(last_24m,thirteen_months[1:12])[c(max(ceiling(input$plot_brush$xmin),13),min(floor(input$plot_brush$xmax),36))],1,6),collapse=" to "),":"),
                                                           pos=1,col="#000000",r=0.2,bg = "white")},silent=T)
    },silent=T)
    
    #try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=5,y=y_range/2, labels = str(input$plot_brush$xmin),pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    par(omi=c(0,10,0,0))
    
  })

  ####Plot 4th graph (same logic as 1st graph)
  output$distPlot4 <- renderPlot({
    
    dataqty<-apply_group_byQty(dataqty,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
        
    dataqty4<-cbind(c(as.character(dataqty[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F]),rep(NA,12)),c(as.character(dataqty[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(dataqty[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataqty[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F])))
    dataqty4round<-cbind(c(as.character(round(dataqty[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F])),rep(NA,12)),c(as.character(round(dataqty[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(dataqty[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataqty[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F]))))
    
    
    par(mar=c(2.5,4,2,1))  
    matplot(dataqty4,xaxt = "n",yaxt = "n",type="bbbb",pch=19, lty=c(1,1,2,2), col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),ylab="Quantity",xlim=c(max(1,ranges2$x[1]),min(36,ranges2$x[2])),ylim=c(max(0,ranges2$y[1]),min(max(as.numeric(dataqty4),na.rm=T),ranges2$y[2])))
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist OS", "hist DD", "DP OS", "DP DD"), pch=19, col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),horiz=TRUE)
    
    #####dataqty labels on points
    try(if(sum(grep("hist_OS", input$labels))!=0){shadowtext(dataqty4[,1], labels=dataqty4round[,1],pos=1,col="#56B4E9",r=0.2,bg = "white")})
    try(if(sum(grep("hist_DD", input$labels))!=0){shadowtext(dataqty4[,2], labels = dataqty4round[,2],pos=1,col="#E69F00",r=0.2,bg = "white")})
    try(if(sum(grep("DP_OS", input$labels))!=0){shadowtext(dataqty4[,3], labels = dataqty4round[,3],pos=1,col="#0072B2",r=0.2,bg = "white")})
    try(if(sum(grep("DP_DD", input$labels))!=0){shadowtext(dataqty4[,4], labels = dataqty4round[,4],pos=1,col="#D55E00",r=0.2,bg = "white")})
    
      
  })


	###### Plots for SemNet
	
	####Plot 1rd graph
    output$distPlot1sn <- renderPlot({

    dataSemNet<-apply_group_bySemNet(dataSemNet,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    ####prepare dataSemNet3 for matplot + round value for labels
    dataSemNet3<-cbind(c(as.character(dataSemNet[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(dataSemNet[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataSemNet[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataSemNet[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F])))
    dataSemNet3round<-cbind(c(as.character(round(dataSemNet[input$num,(1+length(input$grpby)):(24+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(dataSemNet[input$num,(13+length(input$grpby)):(24+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataSemNet[input$num,(97+length(input$grpby)):(108+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataSemNet[input$num,(109+length(input$grpby)):(120+length(input$grpby)),with=F]))))
    #define y plot range to normalise label above point
    y_range<-min(max(as.numeric(dataSemNet3)*1.15,na.rm=T),ranges$y[2])-max(0,ranges$y[1])
    x_range<-min(36,ranges$x[2])-max(1,ranges$x[1])
    dataSemNet3y3<-as.numeric(dataSemNet3[,3])
    dataSemNet3y4<-as.numeric(dataSemNet3[,4])
    dataSemNet3y3[dataSemNet3y3>dataSemNet3y4&!is.na(dataSemNet3y3)]<-dataSemNet3y3[dataSemNet3y3>dataSemNet3y4&!is.na(dataSemNet3y3)]+y_range/20
    dataSemNet3y4[dataSemNet3y4>dataSemNet3y3&!is.na(dataSemNet3y4)]<-dataSemNet3y4[dataSemNet3y4>dataSemNet3y3&!is.na(dataSemNet3y4)]+y_range/20
    
    #define dataSemNet source for cumulative dataSemNet calculation
    dataSemNet3C2<-as.numeric(c(rep(NA,12),dataSemNet3[,1][1:24]))
    dataSemNet3C3<-as.numeric(c(rep(NA,12),dataSemNet3[,1][13:24],dataSemNet3[,3][25:36]))
    dataSemNet3C4<-as.numeric(c(rep(NA,12),dataSemNet3[,1][13:24],dataSemNet3[,4][25:36]))
    
    ####plot dataSemNet3
    par(mar=c(2.5,4,1,1))  
    matplot(NA,xaxt = "n",yaxt = "n",ylab="SemNet (Keuro)",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(dataSemNet3)*1.15,na.rm=T),ranges$y[2])))
    #matplot(dataSemNet3,xaxt = "n",yaxt = "n",type="bbbb",pch=c(19,1,19,19),lty=c(1,3,2,2),col=c("#000000","#555555","#CC79A7","#009E73"),ylab="SemNet (Keuro)",xlim=c(max(1,ranges$x[1]),min(36,ranges$x[2])),ylim=c(max(0,ranges$y[1]),min(max(as.numeric(dataSemNet3),na.rm=T),ranges$y[2])))
    try(if(sum(grep("last_year", input$lines))!=0){matpoints(dataSemNet3[,2],type="b",pch=1,lty=3,col="#555555")})
    try(if(sum(grep("hist_tot", input$lines))!=0){matpoints(dataSemNet3[,1],type="b",pch=19,lty=1,col="#000000")})
    try(if(sum(grep("DPM1", input$lines))!=0){matpoints(dataSemNet3[,4],type="b",pch=19,lty=2,col="#009E73")})
    try(if(sum(grep("new_DP", input$lines))!=0){matpoints(dataSemNet3[,3],type="b",pch=19,lty=2,col="#CC79A7")})
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist tot","last year", "new DP", "DP M-1"), pch=c(19,1,19,19), col=c("#000000","#555555","#CC79A7","#009E73"),horiz=TRUE)
    
    #####dataSemNet labels on points
    try(if(sum(grep("hist_tot", input$labels))!=0){shadowtext(dataSemNet3[,1], labels = dataSemNet3round[,1],pos=1,col="#000000",r=0.2,bg = "white")})
    try(if(sum(grep("last_year", input$labels))!=0){shadowtext(dataSemNet3[,2], labels = dataSemNet3round[,2],pos=1,col="#555555",r=0.2,bg = "white")})
    try(if(sum(grep("DPM1", input$labels))!=0){shadowtext(dataSemNet3y4, labels = dataSemNet3round[,4],pos=1,col="#009E73",r=0.2,bg = "white")})
    try(if(sum(grep("new_DP", input$labels))!=0){shadowtext(dataSemNet3y3, labels = dataSemNet3round[,3],pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    
    try(if(min(floor(input$plot_brush$xmax),36)>12){
    
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-y_range/20,max(ranges$y[1],0)+2.5*y_range/20), 
                                                           labels = formatC(sum(dataSemNet3C3[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#CC79A7",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-2*y_range/20,max(ranges$y[1],0)+1.5*y_range/20), 
                                                           labels = formatC(sum(dataSemNet3C4[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#009E73",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/30), y=max(input$plot_brush$ymin-3*y_range/20,max(ranges$y[1],0)+0.5*y_range/20), 
                                                           labels = formatC(sum(dataSemNet3C2[ceiling(input$plot_brush$xmin):min(floor(input$plot_brush$xmax),36)],na.rm=T),big.mark=" ",format = "f",digits=0),
                                                           pos=1,col="#555555",r=0.2,bg = "white")},silent=T)
    try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=max(input$plot_brush$xmin,max(ranges$x[1],0)+x_range/12), y=max(input$plot_brush$ymin,max(ranges$y[1],0)+3.5*y_range/20), 
                                                           labels = paste(paste(substr(c(last_24m,thirteen_months[1:12])[c(max(ceiling(input$plot_brush$xmin),13),min(floor(input$plot_brush$xmax),36))],1,6),collapse=" to "),":"),
                                                           pos=1,col="#000000",r=0.2,bg = "white")},silent=T)
    },silent=T)
    
    #try(if(sum(grep("cumul", input$labels))!=0){shadowtext(x=5,y=y_range/2, labels = str(input$plot_brush$xmin),pos=1,col="#CC79A7",r=0.2,bg = "white")})
    
    par(omi=c(0,10,0,0))
    
  })

  ####Plot 4th graph (same logic as 1st graph)
  output$distPlot2sn <- renderPlot({
    
    dataSemNet<-apply_group_bySemNet(dataSemNet,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
        
    dataSemNet4<-cbind(c(as.character(dataSemNet[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F]),rep(NA,12)),c(as.character(dataSemNet[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F]),rep(NA,12)),c(rep(NA,24),as.character(dataSemNet[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F])),c(rep(NA,24),as.character(dataSemNet[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F])))
    dataSemNet4round<-cbind(c(as.character(round(dataSemNet[input$num,(25+length(input$grpby)):(48+length(input$grpby)),with=F])),rep(NA,12)),c(as.character(round(dataSemNet[input$num,(49+length(input$grpby)):(72+length(input$grpby)),with=F])),rep(NA,12)),c(rep(NA,24),as.character(round(dataSemNet[input$num,(73+length(input$grpby)):(84+length(input$grpby)),with=F]))),c(rep(NA,24),as.character(round(dataSemNet[input$num,(85+length(input$grpby)):(96+length(input$grpby)),with=F]))))
    
    
    par(mar=c(2.5,4,2,1))  
    matplot(dataSemNet4,xaxt = "n",yaxt = "n",type="bbbb",pch=19, lty=c(1,1,2,2), col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),ylab="SemNet (Keuro)",xlim=c(max(1,ranges2$x[1]),min(36,ranges2$x[2])),ylim=c(max(0,ranges2$y[1]),min(max(as.numeric(dataSemNet4),na.rm=T),ranges2$y[2])))
    axis(1, at = 1:36, labels = c(last_24m,thirteen_months[1:12]), cex.axis = 0.7)
    axis(2, las=2,cex.axis = 0.7)
    grid(nx=0,ny=NULL,col=rgb(0, 0, 0, 0.2))
    abline(v=c(1,7,13,19,25,31),lty=c(4,3),col=c(rgb(0, 0, 0, 0.4),rgb(0, 0, 0, 0.2)))#col=c("#777777","#CCCCCC")
    legend("top", legend=c("hist OS", "hist DD", "DP OS", "DP DD"), pch=19, col=c("#56B4E9","#E69F00", "#0072B2", "#D55E00"),horiz=TRUE)
    
    #####dataSemNet labels on points
    try(if(sum(grep("hist_OS", input$labels))!=0){shadowtext(dataSemNet4[,1], labels=dataSemNet4round[,1],pos=1,col="#56B4E9",r=0.2,bg = "white")})
    try(if(sum(grep("hist_DD", input$labels))!=0){shadowtext(dataSemNet4[,2], labels = dataSemNet4round[,2],pos=1,col="#E69F00",r=0.2,bg = "white")})
    try(if(sum(grep("DP_OS", input$labels))!=0){shadowtext(dataSemNet4[,3], labels = dataSemNet4round[,3],pos=1,col="#0072B2",r=0.2,bg = "white")})
    try(if(sum(grep("DP_DD", input$labels))!=0){shadowtext(dataSemNet4[,4], labels = dataSemNet4round[,4],pos=1,col="#D55E00",r=0.2,bg = "white")})
    
      
  })
	
  

####Example of dynamic choices in chooser (propose only consistent values)  
# #Dynamic supplier selector  
#   
#   output$Supplier <- renderUI({
#     
#     data<-tableau[,lapply(.SD,sum), by=eval(input$grpby), .SDcols=
#                     c(paste0("month_-",24:1,"_tot"),paste0("month_-",24:1,"_os"),paste0("month_-",24:1,"_dd"),
#                       paste0("month_",1:12,"_DP_os"),paste0("month_",1:12,"_DP_dd"),paste0("month_",1:12,"_DP"),paste0("month_",1:12,"_lastDP")
#                     )]
#     
#     if(input$ProdLine=="All"){}else{data<-subset(data,Product.Line==input$ProdLine)}
#     if(input$division=="All"){}else{data<-subset(data,Div==input$division)}
#     choices<-c("All",levels(factor(data$Supplier)))
#     ### note that customer group should not be filtered
#     selectInput("Supplier", "Supplier", as.list(choices))
#   })
  
  
  
#Dynamic customer group selector  
  
  output$CustGroup <- renderUI({
    
    data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    
    choices<-c("All",levels(factor(data$CustGr)))
    
    ### appears only if CustGr is selected
    if(sum(grep("CustGr", input$grpby))!=0){
    selectInput("customer_group", "CustGr", as.list(choices),selected=CustGr_filter$x)
    }
  })

#Dynamic DP family selector  

output$DPSelector <- renderUI({
  
  data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
  
  
  choices<-c("All",levels(factor(data$DP.Family)))
  
  ### appears only if CustGr is selected
  if(sum(grep("DP.Family", input$grpby))!=0){
    selectInput("DPfamily", label=("DPfamily"),as.list(choices),selected=DPfam_filter$x)
  }
})

# Type selector
output$TypeSelector <- renderUI({
    selectInput("type", label=("Type"),c("Value_PAP", "Value_SemNet","Quantity"),selected="Value_PAP")
})

####Dynamic selectors appearance : selector appears only if level is chosen (ticked)

output$DivSelector <- renderUI({
  if(sum(grep("Div", input$grpby))!=0){
  selectInput("division", label=("Division"),c("All"="All","VSF"="D02","VSAO"="D03","VSE"="D05","VSI"="D06","VSG"="D07","VSUK"="D08","VSBE"="D09","VST"="D10","VSEE"="D13","VSR"="D20"),selected=div_filter$x)
  }
})

output$PLSelector <- renderUI({
  if(sum(grep("Product.Line", input$grpby))!=0){
    selectInput("ProdLine", label=("PL"),c("All"="All","BRA"="BRA","ELE"="ELE","FLT"="FLT","IGN"="IGN","POP"="POP","VCC"="VCC","VEC"="VEC","VES"="VES","VLS"="VLS","VSDS"="VSD","VSS"="VSS","VTR"="VTR","VWS"="VWS"),selected=PL_filter$x)
  }
})

# output$DPSelector <- renderUI({
#   if(sum(grep("DP.Family", input$grpby))!=0){
#     selectInput("DPfamily", label=("DPfamily"),c("All",DPlist),selected=DPfam_filter$x)  
#   }
# })

output$SupplierSelector <- renderUI({
  if(sum(grep("Supplier", input$grpby))!=0){
    selectInput("Supplier", label=("Supplier"),c("All",vendorlist$Supplier),selected=Vendor_filter$x) 
  }
})
  
####Show the name of the selected data combination (to which the graphs refers) on the left panel
output$divname<-renderText({
    
    data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    
    #as.character(unlist(data[input$num,1:length(input$grpby),with=F]))
    tryCatch(as.character(unlist(data[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total")})
})

output$divnameqty2<-renderText({
    
    dataqty<-apply_group_byQty(dataqty,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    
    #as.character(unlist(dataqty[input$num,1:length(input$grpby),with=F]))
    tryCatch(as.character(unlist(dataqty[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total")})
})

output$divnameSemLeft<-renderText({
    
    dataSemNet<-apply_group_bySemNet(dataSemNet,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
    
    #as.character(unlist(dataSemNet[input$num,1:length(input$grpby),with=F]))
    tryCatch(as.character(unlist(dataSemNet[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total")})
})

####Show the name of the selected data combination (to which the graphs refers) between the graphs
output$divname2<-renderText({
  
  data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
  #as.character(unlist(data[input$num,1:length(input$grpby),with=F]))
  tryCatch(as.character(unlist(data[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total all divs all PLs")})
})

output$divnameqty<-renderText({
  
	dataqty<-apply_group_byQty(dataqty,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)

	#as.character(unlist(dataqty[input$num,1:length(input$grpby),with=F]))
	tryCatch(as.character(unlist(dataqty[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total all divs all PLs")})
})

output$divnameSemBw<-renderText({
  
	dataSemNet<-apply_group_bySemNet(dataSemNet,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)

	#as.character(unlist(dataSemNet[input$num,1:length(input$grpby),with=F]))
	tryCatch(as.character(unlist(dataSemNet[input$num,1:length(input$grpby),with=F])),error = function(e) {print("Total all divs all PLs")})
})

####Show the names of the 9 combinations following the selected data combination (that will be accessed by increasing value in numericInput)
output$next9 <- renderUI({
  
	data<-apply_group_by(data,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
	tryCatch(
	HTML(paste(paste(as.character(unlist(data[input$num+1,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+2,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+3,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+4,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+5,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+6,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+7,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+8,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(data[input$num+9,1:length(input$grpby),with=F])),collapse=" "),"<br/>","______", sep = ""))
	,error = function(e) {print("NA")})
})

output$nextqty <- renderUI({
  
	dataqty<-apply_group_byQty(dataqty,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
	tryCatch(
	HTML(paste(paste(as.character(unlist(dataqty[input$num+1,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+2,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+3,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+4,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+5,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+6,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+7,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+8,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
            paste(as.character(unlist(dataqty[input$num+9,1:length(input$grpby),with=F])),collapse=" "),"<br/>","______", sep = ""))
  ,error = function(e) {print("NA")})
})

output$nextSem <- renderUI({
  
	dataSemNet<-apply_group_bySemNet(dataSemNet,input_grpby=input$grpby,input_num=input$num,input_ProdLine=input$ProdLine,input_division=input$division,input_Supplier=input$Supplier,input_DPfamily=input$DPfamily,input_customer_group=input$customer_group,input_IVSexcl=input$IVSexcl)
    
	tryCatch(
	HTML(paste(paste(as.character(unlist(dataSemNet[input$num+1,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+2,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+3,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+4,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+5,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+6,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+7,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+8,1:length(input$grpby),with=F])),collapse=" "),"<br/>",
             paste(as.character(unlist(dataSemNet[input$num+9,1:length(input$grpby),with=F])),collapse=" "),"<br/>","______", sep = ""))
  ,error = function(e) {print("NA")})
})

####Add interactive y values for graph 1
output$DynamicInfo <- renderUI({
  if(sum(grep("display", input$infos))!=0){
    xy_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("y=", round(e$y, 0), " kE   ")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("ymin=", round(e$ymin, 0), " kE ,", " ymax=", round(e$ymax, 0), " kE ,", " delta=", round(e$ymax-e$ymin,0), " kE")
    }
    
    HTML(paste0("<br/>",
      "hover: ", xy_str(input$plot_hover), " |  click: ", xy_str(input$plot_click), " |  brush: ", xy_range_str(input$plot_brush),"<br/>","______"
    ))  
  }
})

output$DynamicInfo2 <- renderUI({
  if(sum(grep("display", input$infos))!=0){
    xy_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("y=", round(e$y, 0), " kE   ")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("ymin=", round(e$ymin, 0), " kE ,", " ymax=", round(e$ymax, 0), " kE ,", " delta=", round(e$ymax-e$ymin,0), " kE")
    }
    
    HTML(paste0("<br/>",
                "hover: ", xy_str(input$plot_hover2), " |  click: ", xy_str(input$plot_click2), " |  brush: ", xy_range_str(input$plot_brush2),"<br/>","______"
    ))  
  }
})

####Add interactive y values for graph of Qty
output$DynamicInfo3 <- renderUI({
  if(sum(grep("display", input$infos))!=0){
    xy_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("y=", round(e$y, 0), "  parts   ")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("ymin=", round(e$ymin, 0), "  parts ,", " ymax=", round(e$ymax, 0), "  parts ,", " delta=", round(e$ymax-e$ymin,0), "  parts")
    }
    
    HTML(paste0("<br/>",
      "hover: ", xy_str(input$plot_hover), " |  click: ", xy_str(input$plot_click), " |  brush: ", xy_range_str(input$plot_brush),"<br/>","______"
    ))  
  }
})

output$DynamicInfo4 <- renderUI({
  if(sum(grep("display", input$infos))!=0){
    xy_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("y=", round(e$y, 0), "  parts   ")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL   ")
      paste0("ymin=", round(e$ymin, 0), "  parts ,", " ymax=", round(e$ymax, 0), "  parts ,", " delta=", round(e$ymax-e$ymin,0), "  parts")
    }
    
    HTML(paste0("<br/>",
                "hover: ", xy_str(input$plot_hover2), " |  click: ", xy_str(input$plot_click2), " |  brush: ", xy_range_str(input$plot_brush2),"<br/>","______"
    ))  
  }
})

output$info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL   ")
    paste0("y=", round(e$y, 0), " kE   ")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL   ")
    paste0("ymin=", round(e$ymin, 0), " kE ,", " ymax=", round(e$ymax, 0), " kE ,", " delta=", round(e$ymax-e$ymin,0), " kE")
  }
  
  paste0(
    "hover: ", xy_str(input$plot_hover), "click: ", xy_str(input$plot_click), "brush: ", xy_range_str(input$plot_brush)
  )
})

####Add interactive y values for graph 2
output$info2 <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL   ")
    paste0("y=", round(e$y, 0), " kE   ")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL   ")
    paste0("ymin=", round(e$ymin, 0), " kE ,", " ymax=", round(e$ymax, 0), " kE ,", " delta=", round(e$ymax-e$ymin,0), " kE")
  }
  
  paste0(
    "hover: ", xy_str(input$plot_hover2), "click: ", xy_str(input$plot_click2), "brush: ", xy_range_str(input$plot_brush2)
  )
})

### Display the plots by type

output$showplot1 <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		plotOutput("distPlot",height="500px",
		click = "plot_click",
		hover = "plot_hover",
		brush = "plot_brush",
		dblclick = "plot_dblclick")
	}else 
		if (sum(grep("Value_SemNet", input$type))!=0){
			plotOutput("distPlot1sn",height="500px",
			click = "plot_click",
			hover = "plot_hover",
			brush = "plot_brush",
			dblclick = "plot_dblclick")
		}else
			if (sum(grep("Quantity", input$type))!=0){
					plotOutput("distPlot3",height="500px",
					click = "plot_click",
					hover = "plot_hover",
					brush = "plot_brush",
					dblclick = "plot_dblclick")
			}
})

output$showplot2 <- renderUI({
		if (sum(grep("Value_PAP", input$type))!=0){
		   plotOutput("distPlot2",height="500px",
                    click = "plot_click",
                    hover = "plot_hover",
                    brush = "plot_brush",
                    dblclick = "plot_dblclick")
		}else
			if (sum(grep("Value_SemNet", input$type))!=0){
				plotOutput("distPlot2sn",height="500px",
				click = "plot_click",
				hover = "plot_hover",
				brush = "plot_brush",
				dblclick = "plot_dblclick")
			}else
				if (sum(grep("Quantity", input$type))!=0){
					plotOutput("distPlot4",height="500px",
					click = "plot_click",
					hover = "plot_hover",
					brush = "plot_brush",
					dblclick = "plot_dblclick")
				}
})

###Display the DynamicInfo by type

output$infotype1 <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		uiOutput("DynamicInfo")
	} else if (sum(grep("Value_SemNet", input$type))!=0){
				uiOutput("DynamicInfo")
			} else if (sum(grep("Quantity", input$type))!=0){
						uiOutput("DynamicInfo3")
					}
})

output$infotype2 <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		uiOutput("DynamicInfo2")
	} else if (sum(grep("Value_SemNet", input$type))!=0){
				uiOutput("DynamicInfo2")
			} else if (sum(grep("Quantity", input$type))!=0){
						uiOutput("DynamicInfo4")
					}
}
)

### the output of next 9 divs on left panel
output$htmlinfo <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		htmlOutput("next9")
	} else if (sum(grep("Value_SemNet", input$type))!=0){
				htmlOutput("nextSem")
			} else if (sum(grep("Quantity", input$type))!=0){
						htmlOutput("nextqty")
					}	
})

### the output of the div names bewteen 2 grahs
output$divinfo <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		textOutput("divname2")
	} else if (sum(grep("Value_SemNet", input$type))!=0){
				textOutput("divnameSemBw")
			} else if (sum(grep("Quantity", input$type))!=0){
						textOutput("divnameqty")
					}	
})

### the output info of div names on left panel
output$divnameinfo <- renderUI({
	if (sum(grep("Value_PAP", input$type))!=0){
		textOutput("divname")
	} else if (sum(grep("Value_SemNet", input$type))!=0){
				textOutput("divnameSemLeft")
			} else if (sum(grep("Quantity", input$type))!=0){
						textOutput("divnameqty2")
					}	
})

####Define reactives values for zoom and filters
ranges <- reactiveValues(x = NULL, y = NULL)
ranges2 <- reactiveValues(x = NULL, y = NULL)
div_filter <- reactiveValues(x = "All")
PL_filter <- reactiveValues(x = "All")
CustGr_filter <- reactiveValues(x = "All")
DPfam_filter <- reactiveValues(x = "All")
Vendor_filter <- reactiveValues(x = "All")


####Update reactive values for zoom when dblclick
observeEvent(input$plot_dblclick, {
  brush <- input$plot_brush
  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    
  } else {
    ranges$x <- NULL
    ranges$y <- NULL
  }
})

observeEvent(input$plot_dblclick2, {
  brush2 <- input$plot_brush2
  if (!is.null(brush2)) {
    ranges2$x <- c(brush2$xmin, brush2$xmax)
    ranges2$y <- c(brush2$ymin, brush2$ymax)
    
  } else {
    ranges2$x <- NULL
    ranges2$y <- NULL
  }
})

observeEvent(input$division,{div_filter$x<-input$division})
observeEvent(input$ProdLine,{PL_filter$x<-input$ProdLine})
observeEvent(input$customer_group,{CustGr_filter$x<-input$customer_group})
observeEvent(input$DPfamily,{DPfam_filter$x<-input$DPfamily})
observeEvent(input$Supplier,{Vendor_filter$x<-input$Supplier})
           
})