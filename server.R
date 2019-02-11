server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)  #max Upload size jacked to 100MB
  options(stringsAsFactors = F)
  plotHeight = 490
  plotCols = 3
  qcprompt = "QC Comment (optional)"
  debug = FALSE
  Mar.JoyOfQC =  scan("version.txt",quiet = T)
  
  ovalues <- reactiveValues(
    thisCxn = NULL,
    OUser = NA,
    OPW = NA,
    ODsn = NA,
    Oschema = "None",
    Otable = "None"
  )
  values <- reactiveValues(
    currentdataObj = NULL,
    currentdataObjFilt = NULL,
    currentdataObjFields = NULL,
    limitPlots = 20
  )
  
  #### YO MIKE THIS IS A COMMENT TEST. THIS APP SPARKS JOY :) 
  
  output$plotchk <- renderUI(NULL)
  output$hugeDataWarn <- renderUI(NULL)
  output$facetOptions <- renderUI(NULL)
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  Mar.JoyOfQCRemote  <- tryCatch({
    scan('https://raw.githubusercontent.com/Maritimes/Mar.JoyofQC/master/version.txt',quiet = T)
  },
  error = function(cond) {
  })
  
  updMsg = paste0(a("JoyofQC", href = "https://github.com/Maritimes/Mar.JoyofQC"), " <br><span style='font-size:0.5em;'>v.",Mar.JoyOfQC," </span>")
  if (is.null(Mar.JoyOfQCRemote)){
    updMsg = paste0(updMsg,"<br> Can't reach git to check version")
    #can't contact github repo
  }else if (Mar.JoyOfQC > Mar.JoyOfQCRemote){
    updMsg =paste0(updMsg,"<br><span style='font-size:0.5em;color:red;'><b>&lt;Push to Github!&gt;</b></span>")
  }else if (Mar.JoyOfQC < Mar.JoyOfQCRemote){
    updMsg =paste0(updMsg,"<span style='font-size:0.5em;color:red;'>&lt;Outdated!&gt;</span>")
  }else{
    updMsg =paste0(updMsg,"<span style='font-size:0.5em;color:blue;'>&lt;Current&gt;</span>")
  }
  output$versionCheck <- renderUI(HTML(updMsg))
  
  source("getHelp.R")
  
  output$getHelp<-renderUI({
    getHelp()
  })
  
  #these tags are here instead of UI because they're dynamic
  output$selDet = renderUI(textInput(inputId = "selDetInput", label = "",value = qcprompt))
  output$unhideBox = renderUI("")
  
  observeEvent(input$fBrowse,{
    if (debug) print("input$fBrowse")
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$fBrowse)
  })
  observeEvent(input$rObjSel,{
    if (debug) print("input$rObjSel")
    #this ensures that if an r object is picked, it gets handled
    handleData("local", input$dataSel, input$rObjSel)
  })
  observeEvent(input$OcredSubmit,{
    if (debug) print("input$OcredSubmit")
    if (!is.null(ovalues$thisCxn)){
      ovalues$thisCxn<-NULL
      output$Oschema = renderUI({NULL})
      output$Otable = renderUI({NULL})
    }
    handleData(input$dataSel, NULL, NULL)
  })
  observeEvent(input$Oschema,{
    if (input$Oschema == "None")return(NULL)
    handleData(input$dataSel, input$Oschema, NULL)
  })
  observeEvent(input$Otable,{
    if (input$Otable == "None")return(NULL)
    handleData(input$dataSel,input$Oschema,input$Otable)
  })
  observeEvent(input$dataSel,{
    values$currentdataObj = NULL
    values$currentdataObjFields = NULL
    output$xaxis = renderUI({NULL})
    output$yaxis = renderUI({NULL})
    output$facet = renderUI({NULL})
    output$loadstatus =renderUI({NULL})
    output$facetText =renderUI({NULL})
    output$fBrowse = renderUI({NULL})
    output$rObjSel = renderUI({NULL})
    output$OcredUsePkg = renderUI(NULL)
    output$OcredName = renderUI({NULL})
    output$OcredPass = renderUI({NULL})
    output$OcredDSN = renderUI({NULL})
    output$OcredSubmit = renderUI({NULL})
    output$Oschema = renderUI({NULL})
    output$Otable = renderUI({NULL})
    output$filtFields = renderUI({NULL})
    output$filtFieldVals = renderUI({NULL})
    output$filtApply = renderUI({NULL})
    output$filtRem = renderUI({NULL})
    ranges$x <- NULL
    ranges$y <- NULL
    output$resetZoom = renderUI(NULL)
   
    output$saveMsg = renderText("Nothing to report")
    if (is.null(values$currentDataObj)) output$saveMsg = renderText("No data loaded")
    
    if (input$dataSel %in% c("faithful","mtcars","pressure")){
      handleData("sample",input$dataSel, NULL)
    } else if (input$dataSel %in% c("robject","csvobject","qcobject","rdata","rdsobject")){
      handleData("local", input$dataSel, NULL)
    }else if (input$dataSel %in% c("oracle")){
      handleData("oracle", NULL, NULL)
    }
  })
  observeEvent(input$handleSelected,{
    applyQC()
  })
  observeEvent(input$handleUnselected,{
    applyQC("inverted")
  })
  observeEvent(input$plotchk,{
    if (debug) print("input$plotchk")
  })
  observeEvent(input$unhide, {
    thisData = values$currentdataObj
    thisData[thisData$QC_HIDDEN ==TRUE,"QC_HIDDEN"]<-FALSE
    values$currentdataObj= thisData
    output$unhideBox = renderUI({NULL})
  })
  observeEvent(input$saveDet,{
    #This saves the data so that you can use it to fix the original source
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    fn = paste0("qcResults_",ts,".csv")
    
    thisData = rbind(values$currentdataObj,values$currentdataObjFilt)
    nullrows <- nullXRows<- nullYRows <- nullFRows<- nullXYFRows <- nullXYRows <- NULL
    if (input$includeNULLS) {
      nullXRows = thisData[is.na(thisData[input$xaxis]),!names(thisData) %in% c("QC_COMMENT")]
      nullYRows = thisData[is.na(thisData[input$yaxis]),!names(thisData) %in% c("QC_COMMENT")]
      nullXYRows <- merge(nullXRows, nullYRows) 
      Fonly = NULL
      if (input$facet != "None") {
        nullFRows = thisData[is.na(thisData[input$facet]),!names(thisData) %in% c("QC_COMMENT")]
        nullXYFRows <- merge(nullXYRows, nullFRows) 
        fs <- rbind(nullXYFRows,nullFRows)
        Fonly = fs[! duplicated(fs, fromLast=TRUE) & seq(nrow(fs)) <= nrow(nullFRows), ]
        if (nrow(nullXYFRows)>0){
          nullXYFRows$QC_COMMENT<-paste0("missing values for ", input$xaxis, ", ", input$yaxis, ", ", input$facet)
        }
        if (nrow(Fonly)>0){
          Fonly$QC_COMMENT<-paste0("missing values for ", input$facet)
        }
      }
      
      xs <- rbind(nullXYRows, nullXRows)
      ys <- rbind(nullXYRows, nullYRows)
      Xonly = xs[! duplicated(xs, fromLast=TRUE) & seq(nrow(xs)) <= nrow(nullXRows), ]
      Yonly = ys[! duplicated(ys, fromLast=TRUE) & seq(nrow(ys)) <= nrow(nullYRows), ]
      
      if (nrow(nullXYRows)>0){
        nullXYRows$QC_COMMENT<-paste0("missing values for ", input$xaxis, ", ", input$yaxis)
      }
      if (nrow(Xonly)>0){
        Xonly$QC_COMMENT<-paste0("missing values for ", input$xaxis)
      }
      if (nrow(Yonly)>0){
        Yonly$QC_COMMENT<-paste0("missing values for ", input$yaxis)
      }
      nullrows = rbind(nullXYRows, Xonly)
      nullrows = rbind(nullrows, Yonly)
      if (!is.null(Fonly)){
        if (nrow(Fonly)>0){
          nullrows = rbind(nullrows, Fonly)
        }
      }
      
      if (nrow(nullrows)>0){
        nullrows$QC_STATUS<-"BAD"
        nullrows$QC_HIDDEN<-"FALSE"
      }
    }
    
    thisData=thisData[thisData$QC_STATUS != "UNASSESSED",]
    thisData=rbind(thisData,nullrows)
    assign(paste0("qcResults_",ts), thisData,envir = .GlobalEnv)
    write.csv(thisData,fn, row.names = FALSE)
    output$saveMsg <- renderText(paste0("Output available in R as ",paste0("qcResults_",ts)," and saved as ", paste0(getwd(),"/ ",fn)))
  })
  
  observeEvent(input$saveSess,{
    #This saves the data so that you can open it up again and keep working
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    fn = paste0("qcSession_",ts,".joy")
    thisData = rbind(values$currentdataObj,values$currentdataObjFilt)
    write.csv(thisData, fn, row.names = FALSE)
    output$saveMsg <- renderText(paste0("Session saved as ", paste0(getwd(),"/",fn)))
  })
  observeEvent(input$filtFields,{
    
    if (debug) print("input$filtFields")
    if (input$filtFields =="None"){
      return()
    }else{
      thisData    <- values$currentdataObj
      availVals <- sort(unique(thisData[[input$filtFields]]))
      output$filtFieldVals = renderUI(selectInput("filtFieldVals", "Select values you want to retain ", choices = availVals, multiple = TRUE))
      output$filtApply = renderUI(actionButton('filtApply', label = "Apply filter", icon('filter')))
    }
  })
  observeEvent(input$filtApply,{
    if (debug) print("input$filtApply")
    thisData    <- values$currentdataObj
    thisDataFilt <- values$currentdataObjFilt
    thisDataFilt <- rbind(thisDataFilt,thisData[thisData[[input$filtFields]] %in% input$filtFieldVals,])
    x=rbind(thisData, thisDataFilt)
    thisDataUnfilt = x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(thisData), ]
    values$currentdataObj = thisDataFilt
    values$currentdataObjFilt = thisDataUnfilt
    output$saveMsg <- renderText(paste0("Filtered data to only include values where ",input$filtFields," have values like ",paste0(input$filtFieldVals, collapse = ",")))
    output$filtApply = renderUI(actionButton('filtApply', label = "Apply filter", icon('filter')))
    output$filtRem = renderUI(actionButton('filtRem', label = "Remove filter", icon('filter')))
  })
  observeEvent(input$filtRem,{
    if (debug) print("input$filtRem")
    thisData    <- values$currentdataObj
    values$currentdataObj = NULL
    thisDataUnfilt <- values$currentdataObjFilt
    all = rbind(thisData, thisDataUnfilt)
    values$currentdataObj = all
    print(paste0("In filtRem, dataObj has: ",nrow(values$currentdataObj)))
    values$currentdataObjFilt = NULL
    output$filtRem = renderUI(NULL)   
    output$filtFields = renderUI(selectInput("filtFields", "Select a field by which to filter (optional)", choices = c("None",values$currentdataObjFields), selected = "None", multiple = FALSE))
    output$filtFieldVals = renderUI(NULL)
    output$filtApply = renderUI(NULL)
    output$saveMsg <- renderText("Filter removed")
    
  })
  observeEvent(input$facetOverride,{
    if (values$limitPlots ==20){
      values$limitPlots <- 500
    }else{
      values$limitPlots <- 20
    }
  })
  observeEvent(input$facetRemover,{
    values$limitPlots <- 20
    output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",values$currentdataObjFields), selected = "None", multiple = FALSE))
  })
  observeEvent(input$showHelp, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Help")
  })
  observeEvent(input$distPlot_dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      output$resetZoom = renderUI(actionButton('resetZoom', label = "Return to full extent of data", icon('search-minus')))
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  observeEvent(input$resetZoom, {
    ranges$x <- NULL
    ranges$y <- NULL
    output$resetZoom = renderUI(NULL)
  })
  selected <- reactive({
    input$brush
    invert = FALSE
    thisData = values$currentdataObj
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    brushed=brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                          input$brush,
                          xvar = input$xaxis,
                          yvar = input$yaxis)
    return(brushed)
  })
  
  
  
  
  populateDrops<-function(currentdataObjFields){
    if (debug) print("populateDrops")
    if (!is.null(currentdataObjFields)){
      if (length(currentdataObjFields)<2){
        output$loadstatus <- renderText(paste0("Insufficient plottable columns!"))
        return()
      }
      output$xaxis = renderUI(selectInput("xaxis", "Select your x-axis", choices = currentdataObjFields, selected = currentdataObjFields[1], multiple = FALSE))
      output$yaxis = renderUI(selectInput("yaxis", "Select your y-axis", choices = currentdataObjFields, selected = currentdataObjFields[2], multiple = FALSE))
      if (length(currentdataObjFields)>2){
        output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",currentdataObjFields), multiple = FALSE))
      }else{
        output$facet = renderUI({NULL})
      }
      output$filtFields = renderUI(selectInput("filtFields", "Select a field by which to filter (optional)", choices = c("None",currentdataObjFields), selected = "None", multiple = FALSE))
    }
  }
  handleData<-function(type=NULL, specific=NULL, theobj=NULL){
    if (debug) print("handleData")
    if (debug) print(paste0("type:",type,"   specific:",specific,"   theobj", theobj))    
    dataSelToLoad = NULL
    if (type == "sample"){
      dataSelToLoad = get(specific)
      output$saveMsg <- renderText("Sample data loaded")
    } else if (type=="local"){
      
      
      if (specific %in% c("csvobject","rdsobject","qcobject","rdata")){
        if (is.null(theobj)){
          output$fBrowse = renderUI(fileInput("fBrowse", "Choose *.csv file",
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv","csv",
                                                "joy",".joy",
                                                "rds",".rds",
                                                "rdata",".rdata","rda",".rda"),
                                              multiple = FALSE))
          if (is.null(input$fBrowse))return()
        }else{
          ft = tools::file_ext(theobj$datapath)
          if (ft %in% c('csv','joy')){
            dataSelToLoad = read.csv(theobj$datapath)
            output$saveMsg <- renderText("Local csv object loaded")
          }else if (ft %in% 'rds'){
            dataSelToLoad <- tryCatch({
                    readRDS(theobj$datapath)
                  },
                  error = function(cond){
                  })
                  if (is.null(dataSelToLoad)){
                    output$saveMsg <- renderText("Can't find data in this object")
                    return()
                  }
                  output$saveMsg <- renderText("rds file loaded - if this isn't what you expected, try to create a simpler object (e.g. a df)")
          }else if (ft %in% c("rdata","rda")){
            loadRData <- function(fileName){
                    #function to load rdata to known variable name
                    load(fileName)
                    get(ls()[ls() != "fileName"])
                  }
                  dataSelToLoad <- tryCatch({
                    loadRData(theobj$datapath)
                  },
                  error = function(cond) {
                  })
                  if (is.null(dataSelToLoad)){
                    output$saveMsg <- renderText("Can't find data in this object")
                    return()
                  }

                  output$saveMsg <- renderText("Rdata loaded - if this isn't what you expected, try to create a simpler object (e.g. a df)")
          }
        }
                                                     
        
        
      }else if (specific == "robject" ){
        if (is.null(theobj)){
          localObjsDFs= names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
          localObjsMats= names(which(unlist(eapply(.GlobalEnv,is.matrix))))
          localObjsLists= names(which(unlist(eapply(.GlobalEnv,is.list))))
          localObjs = sort(c(localObjsMats,localObjsDFs, localObjsLists))
          if (length(localObjs)<1){
            output$rObjSel = renderUI({NULL})
          }else{
            output$rObjSel= renderUI(selectInput("rObjSel","Choose a local R object", c("None",localObjs)))
            if (is.null(input$rObjSel))return()
          }
          dataSelToLoad = renderUI({NULL})
        }else{
          if (theobj == "None")return()
          dataSelToLoad = get(theobj)
          output$saveMsg <- renderText("Local r object loaded")
        }
      }
    }else if (type == "oracle"){
      getOracleCreds<-function(){
        OcredNameValue = ifelse(!is.na(ovalues$OUser),ovalues$OUser,ifelse(exists("oracle.username"),oracle.username, ""))
        OcredPassValue = ifelse(!is.na(ovalues$OPW),ovalues$OPW,ifelse(exists("oracle.password"),oracle.password, ""))
        OcredDSNValue = ifelse(!is.na(ovalues$ODsn),ovalues$ODsn,ifelse(exists("oracle.dsn"),oracle.dsn, ""))
        output$OcredUsePkg <- renderUI(selectInput("OcredUsePkg",
                                                   label = "Select how you normally connect R to Oracle ",
                                                   choices = c('rodbc','roracle'),
                                                   selected = 'rodbc'))
        output$OcredName = renderUI(textInput("OcredName","Enter your Oracle username", value = OcredNameValue))
        output$OcredPass = renderUI(passwordInput("OcredPass","Enter your Oracle password", value = OcredPassValue))
        output$OcredDSN = renderUI(textInput("OcredDSN","Oracle DSN", value = OcredDSNValue))
        output$OcredSubmit = renderUI(actionButton("OcredSubmit", "Go"))
      }
      makeOracleCxn<-function(){
        thisCxn <- Mar.utils::make_oracle_cxn(usepkg = input$OcredUsePkg, fn.oracle.username = input$OcredName,fn.oracle.password = input$OcredPass, fn.oracle.dsn = input$OcredDSN)
        if (class(thisCxn)=="list"){
          ovalues$thisCxn = thisCxn
          ovalues$OUser = toupper(input$OcredName)
          ovalues$OPW = toupper(input$OcredPass)
          ovalues$ODsn = toupper(input$OcredDSN)
          output$OcredName = renderUI(textInput("OcredName","Enter your Oracle username", value = toupper(input$OcredName)))
          output$OcredPass = renderUI(passwordInput("OcredPass","Enter your Oracle password", value = toupper(input$OcredPass)))
          output$OcredDSN = renderUI(textInput("OcredDSN","Oracle DSN", value = toupper(input$OcredDSN)))
          output$OcredSubmit = renderUI(actionButton("OcredSubmit", "Go"))
        }else{
          ovalues$thisCxn = NULL
        }
        return(ovalues$thisCxn)
      }
      showSchemaPick<-function(){
        schemas = ovalues$thisCxn$thecmd(ovalues$thisCxn$channel,paste0("select distinct(table_schema) OWNER from all_tab_privs WHERE GRANTEE = '",ovalues$OUser,"' ORDER BY OWNER"))
        output$Oschema <- renderUI(selectInput("Oschema",
                                               label = "Select a schema",
                                               choices = c("None",schemas$OWNER),
                                               selected = "None"))
        return(schemas)
      }
      showTablePick<-function(){
        tbls = ovalues$thisCxn$thecmd(ovalues$thisCxn$channel,paste0("select TABLE_NAME from all_tab_privs WHERE table_schema = '",ovalues$Oschema,"' AND GRANTEE = '",ovalues$OUser,"' ORDER BY TABLE_NAME"))
        output$Otable = renderUI(selectInput("Otable",
                                             label = "Select a table",
                                             choices =c("None",tbls$TABLE_NAME),
                                             selected = "None"))
        return(tbls)
      }
      #End of Oracle-specific functions
      #######
      if (is.null(specific) & is.null(theobj)){
        #no schema or table
        getOracleCreds()
        req(input$OcredSubmit)
        if (is.null(ovalues$thisCxn)) {
          cxn = makeOracleCxn()
          if (is.null(cxn)){
            output$saveMsg <- renderText("cxn failed")
            return(NULL)
          }
        }
        showSchemaPick()
        
        return(NULL)
      }else if (is.null(theobj)){
        ovalues$Oschema<-toupper(specific)
        #no table
        showTablePick()
      }else{
        ovalues$Otable<-toupper(theobj)
        dataSelToLoad <- ovalues$thisCxn$thecmd(ovalues$thisCxn$channel,paste0("SELECT * FROM ",ovalues$Oschema,".",ovalues$Otable))
        output$saveMsg <- renderText("Oracle data loaded")
      }
    }
    
    if (is.null(dataSelToLoad)) {
      output$saveMsg <- renderText("No data found")
      return()
    }
    
    dataclass = class(dataSelToLoad)
    if (dataclass == "data.frame"){
      #types that work fine
    }else if (dataclass %in% c("SpatialPolygonsDataFrame","SpatialLinesDataFrame","SpatialPointsDataFrame","SpatialMultiPointsDataFrame")){
      #sp objects can have dataframe we can look at
      dataSelToLoad=dataSelToLoad@data
      output$saveMsg <- renderText("file was sp:: object - loading dataframe from that object")
    } else if (dataclass == "list"){
      dataSelToLoad <- tryCatch({
        as.data.frame(dataSelToLoad)
      },
      error = function(cond) {
      })
      if (is.null(dataSelToLoad)){
        output$saveMsg <- renderText("Data not accessible (is it a list?)")
        return()
      }
      output$saveMsg <- renderText("Coerced list data to dataframe)")
    }else if (dataclass =="matrix") {
      dataSelToLoad=data.frame(dataSelToLoad)
      output$saveMsg <- renderText("Matrix converted to df and loaded")
    }else{
      output$saveMsg <- renderText(paste0("This app does not know how to deal with this class of object (",dataclass,").  Please create a simpler object (e.g. a df)"))
      return()
    }
    
    if (!all(c("QC_STATUS","QC_COMMENT","QC_HIDDEN") %in% colnames(dataSelToLoad))){
      #will overwrite values if data only has a couple of these
      dataSelToLoad$QC_STATUS <-"UNASSESSED"
      dataSelToLoad$QC_COMMENT <-NA
      dataSelToLoad$QC_HIDDEN<-FALSE
    }
    if (nrow(dataSelToLoad[dataSelToLoad$QC_HIDDEN ==TRUE,])>0){
      output$unhideBox = renderUI(actionButton(inputId = 'unhide', label = 'Unhide All', icon = icon('eye')))
    }else{
      output$unhideBox = renderUI({NULL})
    }
    
    values$currentdataObj = dataSelToLoad
    values$currentdataObjFields = colnames(dataSelToLoad)
    output$plotchk = renderUI(div(radioButtons("plotchk", label = a("Show 95% confidence intervals?    ",href="https://ggplot2.tidyverse.org/reference/geom_smooth.html", target="_blank"), 
                                               choices = list("none" = "none", "auto"="auto", "lm"="lm", "glm"="glm", "gam"="gam", "loess"="loess"),
                                               selected = "none")))
    populateDrops(values$currentdataObjFields)
  }
  applyQC<-function(sel="normal"){
    if (debug) print("applyQC")
    thisData = values$currentdataObj
    if(is.null(thisData))return(NULL)
    newcomm = ifelse(input$selDetInput==qcprompt,"", input$selDetInput)
    selected <- selected()
    if (sel != "normal"){
      selectedInvert = thisData[!duplicated(rbind(selected, thisData))[-seq_len(nrow(selected))], ]
      selected = selectedInvert
    }
    if (input$qcAction == "good"){
      thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"GOOD"
      output$saveMsg <- renderText("Records set to 'GOOD'")
    }else if (input$qcAction == "bad"){
      thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"BAD"
      output$saveMsg <- renderText("Records set to 'BAD'")
    }else if (input$qcAction == "undo"){
      thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"UNASSESSED"
      thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]<-NA
      output$saveMsg <- renderText("QC info removed from records")
    }
    
    if (input$qcAction == "ugly" | input$hideHandled ==T){
      thisData[rownames(thisData) %in% rownames(selected),"QC_HIDDEN"]<-TRUE
    }
    
    
    thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]<-paste0(ifelse(is.na(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]) | nchar(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"])<1,"",paste0(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"],",")),newcomm)
    updateTextInput(session, "selDetInput", label = NULL, value = qcprompt)
    
    if (nrow(thisData[thisData$QC_HIDDEN ==TRUE,])>0){
      output$unhideBox = renderUI(actionButton(inputId = 'unhide', label = 'Unhide All', icon = icon('eye')))
    }else{
      output$unhideBox = renderUI({NULL})
    }
    
    values$currentdataObj = thisData
    session$resetBrush("brush")
  }
  dataSizeChecker<-function(df){
    if (debug) print("dataSizeChecker")
    # if (is.null(df))return(NULL)
    res = "OK"
    nrowData = nrow(df)
    dataSizeMB = as.numeric(sub(pattern = " Mb",x = format(object.size(df),units = "Mb"),replacement = ""))
    if (is.null(nrowData)){
      output$hugeDataWarn <- renderUI({NULL})
    }else if (dataSizeMB > 100){
      #limit exceeded
      output$hugeDataWarn <-
        renderUI({
          #these tags are here instead of UI so they can be on one line
          div(id="visFail", paste("Your data selection has ",nrowData," rows (i.e. ",dataSizeMB," Mb).  That's too much for this app. Please filter the data in r to create a smaller r object")
          )
        })
      res = "fail"
    }else if (dataSizeMB > 20){
      #upper limit warning
      output$hugeDataWarn <-
        renderUI({
          #these tags are here instead of UI so they can be on one line
          div(id="visWarn", paste("Your data selection has ",nrowData," rows (i.e. ",dataSizeMB," Mb).  That's a lot for this app.  Consider filtering the data in r to create a smaller r object.")
          )
        })
      
      res = "warn"
    }else{
      output$hugeDataWarn <- renderUI({NULL})
    }
    return(res)
  }
  facetChecker<-function(nfacets = 1){
    if (debug) print("facetChecker")
    res = "OK"
    if (nfacets == 1){
      #      output$facetOptions <- renderUI({NULL})
      output$facetOptions <- NULL
    }else if (nfacets<=values$limitPlots){
      output$facetOptions <-
        renderUI({
          actionButton(inputId = 'facetRemover', label = paste0("Remove faceting"), icon = icon('layer-group'))
        })
      
    } else if (nfacets>values$limitPlots){
      output$facetOptions <-
        renderUI({
          div(id="facetBlock",
              paste0(input$facet, ' has ',nfacets,' unique values.  Perhaps too many to usefully facet by and QC?')
              ,actionButton(inputId = 'facetOverride', label = paste0("Thanks, 'Dad', show all ",nfacets,"(!) plots"), icon = icon('layer-group'))
          )
        })
      res = "warn"
    }
    return(res)
  }
  
  getPlotHeight <- reactive({
    #need to send n facet
    input$facetOverride
    input$facet
    if (is.null(input$facet))return(plotHeight)
    thisData    <- values$currentdataObj
    if (is.null(thisData))return(1)
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    nfacets = ifelse(input$facet == 'None',1,length(unique(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$facet])))
    facetCheck = facetChecker(nfacets)
    if (facetCheck=="warn")return(1)
    plotRows = ceiling(nfacets/plotCols )
    plotHeight = plotRows*plotHeight
    if (nfacets>values$limitPlots){
      if (is.null(input$facetOverride)){
        plotHeight = 1
      }
    }
    return(plotHeight)
  })
  
  makeSelDataTable<-function(){
    if (debug) print("makeSelDataTable")
    input$handleSelected
    input$handleUnselected
    # input$unhide
    thisData<-values$currentdataObj 
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    tableData = brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                              input$brush,
                              xvar = input$xaxis,
                              yvar = input$yaxis)
    tableData = tableData[, !names(tableData) %in% c("QC_HIDDEN")] 
    tableData = Mar.utils::drop_NA_cols(tableData)
    return(tableData)
  }
  
  makePlot<-function(thedf = NULL, conf = NULL, facet=NULL){
    conf = ifelse(is.null(conf),"none", conf)
    plottableData <-thedf[which(!(thedf$QC_HIDDEN %in% TRUE) & !is.na(thedf[,input$xaxis]) & !is.na(thedf[,input$yaxis])),]
    
    thePlot <- ggplot(data = plottableData, 
                      aes(color = QC_STATUS,shape = QC_STATUS,
                          x=plottableData[,input$xaxis], 
                          y=plottableData[,input$yaxis])) + 
      geom_point() + xlab(input$xaxis) + ylab(input$yaxis)  + 
      scale_shape_manual(name = "QC_STATUS", values =c("UNASSESSED" = 16, "GOOD"=17, "BAD"=15)) + 
      scale_color_manual(name = "QC_STATUS", values =c("UNASSESSED" = "black", "GOOD"="blue", "BAD"="RED"))
    if (conf != 'none'){
      thePlot <- thePlot + geom_smooth(method=conf, fill="red", color="red")
    }
    
    if(!facet == 'None') {
      thePlot <- thePlot + facet_wrap(as.formula(paste('~', facet)),ncol=plotCols)
    }
    thePlot <- thePlot + theme(legend.position="top",text = element_text(size=15)) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
    return(thePlot)
  }

  output$distPlot <- renderPlot(width = "auto", height =function(){getPlotHeight()},{
    req(input$xaxis, input$yaxis)
    thisData <- values$currentdataObj
    if(is.null(thisData))return(NULL)
    input$plotchk
    input$facet
    input$facetOverride
    input$handleSelected
    input$handleUnselected
    input$unhide
    input$filtRem
    input$filtApply
    if (dataSizeChecker(thisData)=="fail")return(NULL)
    nfacets = ifelse(input$facet == 'None',1,length(unique(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$facet])))
    if (facetChecker(nfacets)=="warn")return(NULL)
    makePlot(thedf =thisData,  conf = input$plotchk,  facet= input$facet)
  })
  
  output$selTable <- renderDataTable({
    req(input$xaxis, input$yaxis)
    if (is.null(values$currentdataObj))return()
    res = makeSelDataTable()
    return(res)
  })
}
