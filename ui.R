#### Import libraries
library(shiny)
library(plyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(data.table)

####Suppliers and DP family list in case we want to show them permanently (if dynamic, will be managed in server.r)
#vendorlist<-fread("SupplierName.txt",colClasses =c("integer",rep("character",2)))
#DPlist<-levels(fread("tableauCGvendor.csv",dec=",",colClasses =c("integer",rep("factor",5),rep("numeric",120)),stringsAsFactors=T,drop=1)$DP.Family)


#### Define UI for the application
#### fluidPage is a predefined model
shinyUI(
  
  fluidPage(
    #tags$head(tags$link(rel = "icon", href = paste0("//calendar.google.com/googlecalendar/images/favicon_v2014_",substr(file.info(list.files(pattern="tableauCGvendor"))$mtime,9,10),".ico"))
    if (sum(grep("mgollety",getwd()))==1){tags$head(tags$link(rel = "icon", href = "//static8.viadeo-static.com/QIi8iNTEPkLurTWxUPblTgvKV6I=/40x40/smart/member/00224jofzgj9hzxy?ts=1410727408000"))
    }else{
      tags$head(tags$link(rel = "icon", href = paste0("//calendar.google.com/googlecalendar/images/favicon_v2014_",substr(file.info(list.files(pattern="tableauCGvendor"))$mtime,9,10),".ico"))
      )},
    
  HTML(paste0("<TITLE>Shiny DP tool - last data update : ",format(file.info(list.files(pattern="tableauCGvendor"))$mtime,"%a %d %b %Y %X"),"</TITLE>")),  
    
  # Application title
  #titlePanel("Line selector"),
  
  # Sidebar for selections or user inputs
  sidebarLayout(
    sidebarPanel(
      ####Title for the window
      ####User inputs
	  uiOutput("TypeSelector"),
	  #selectInput("type", label=("Type"),choices=c("Value","Quantity"),selected = "Value"),
      uiOutput("DivSelector"),
      uiOutput("PLSelector"),
      uiOutput("DPSelector"),
      uiOutput("CustGroup"),
      uiOutput("SupplierSelector"),
      ####Inputs in case we want to show them permanently (if dynamic, will be managed in server.r)
      #selectInput("division", label=("Division"),c("All"="All","VSF"="D02","VSAO"="D03","VSE"="D05","VSI"="D06","VSG"="D07","VSUK"="D08","VSBE"="D09","VST"="D10","VSEE"="D13","VSR"="D20")),
      #selectInput("ProdLine", label=("PL"),c("All"="All","BRA"="BRA","ELE"="ELE","FLT"="FLT","IGN"="IGN","POP"="POP","VCC"="VCC","VEC"="VEC","VES"="VES","VLS"="VLS","VSDS"="VSD","VSS"="VSS","VTR"="VTR","VWS"="VWS")),
      #selectInput("DPfamily", label=("DPfamily"),c("All",DPlist)),
      #selectInput("DP_PL", label=("DP/PL"),c("DP"="DP","PL"="PL","AlertDP"="ADP","AlertCG"="ACG")),
      #selectInput("Supplier", label=("Supplier"),c("All",vendorlist$Supplier)),
      ####
      ####this numeric selector enables to select the line in a sorted aggregated data table (1 is the biggest 12m TO)
      numericInput("num", label =("Numeric input"), value = 1),
      ####Checkboxes to define aggregation level
      checkboxGroupInput('grpby', 'Group by:',c("Div"="Div","PL"="Product.Line","DP family"="DP.Family","CustGr"="CustGr","Supplier"="Supplier"), selected = c("Product.Line","Div")),
      ####Text output to anticipate following lines in the numeric input
      uiOutput("divnameinfo"),
      uiOutput("htmlinfo"),     
      ####Checkboxes to add data labels on the graph
      checkboxGroupInput('labels', 'Labels:',c("hist tot"="hist_tot","last year"="last_year","new DP"="new_DP","DP M-1"="DPM1",
                                               "hist OS"="hist_OS","hist DD"="hist_DD","DP OS"="DP_OS","DP DD"="DP_DD","cumul"="cumul"),selected=c("cumul")),
      checkboxGroupInput('lines', 'Lines:',c("hist tot"="hist_tot","last year"="last_year","new DP"="new_DP","DP M-1"="DPM1")
                                          ,selected=c("hist_tot","last_year","new_DP","DPM1")),
      checkboxGroupInput('infos', 'Infos:',c("Display ?"="display")),
      checkboxGroupInput('IVSexcl', 'Excl IVS ?:',c("Excl. IVS"="IVSexcl"),selected=c("IVSexcl")),
      ####define style for current chosen value (output divname and divname2)
      tags$head(tags$style("#divname{color: red;
                                 font-style: italic;
                                 }","#divnameqty2{color: red;
                                 font-style: italic;
                                 }","#divnameSemLeft{color: red;
                                 font-style: italic;
                                 }","#divname2{
                                 color: #48ca3b;
                                 font-size: 110%;
                                 font-weight: bold;
                                 text-align: center;
                                 }","#divnameqty{
                                 color: #48ca3b;
                                 font-size: 110%;
                                 font-weight: bold;
                                 text-align: center;
                                 }","#divnameSemBw{
                                 color: #48ca3b;
                                 font-size: 110%;
                                 font-weight: bold;
                                 text-align: center;
                                 }"
      ))
    ,width = 2),
    
    #### Show the 2 plots and generate hover, click and brush inputs
	mainPanel(
	  uiOutput("showplot1")
	  #,verbatimTextOutput("info")
      ,uiOutput("infotype1")
      ,uiOutput("divinfo")
	  ,uiOutput("showplot2")
	  #,verbatimTextOutput("info2")
      ,uiOutput("infotype2")
      ####Uncomment to show the data below graphs
      #,dataTableOutput("data0")
      ,width = 10
    )

)))