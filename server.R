server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)  #max Upload size jacked to 50MB
  options(stringsAsFactors = F)
  .GlobalEnv$dataObj = NULL
  .GlobalEnv$dataObjFields = NULL
  .GlobalEnv$thisCxn = NULL
  .GlobalEnv$limitPlots = 20
  .GlobalEnv$OUser <- NA
  .GlobalEnv$OPW <- NA
  .GlobalEnv$ODsn <- NA
  .GlobalEnv$Oschema <- "None"
  .GlobalEnv$Otable <- "None"
  .GlobalEnv$qcprompt = "QC Comment (optional)"
  .GlobalEnv$debug = FALSE
  .GlobalEnv$Mar.JoyOfQC =  scan("version.txt",quiet = T)
  
  output$plotchk <- renderUI(NULL)
  output$hugeDataWarn <- renderUI(NULL)
  output$facetOptions <- renderUI(NULL)
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  Mar.JoyOfQCRemote  <- tryCatch({
    scan('https://raw.githubusercontent.com/Maritimes/Mar.JoyofQC/master/version.txt',quiet = T)
  },
  error = function(cond) {
  })
  
  updMsg = paste0(a("Mar.QCJoy", href = "https://github.com/Maritimes/Mar.JoyofQC"), " <br><span style='font-size:0.5em;'>v.",.GlobalEnv$Mar.JoyOfQC," </span>")
  if (is.null(Mar.JoyOfQCRemote)){
    updMsg = paste0(updMsg,"<br> Can't reach git to check version")
    #can't contact github repo
  }else if (.GlobalEnv$Mar.JoyOfQC > Mar.JoyOfQCRemote){
    updMsg =paste0(updMsg,"<br><span style='font-size:0.5em;color:red;'><b>&lt;Push to Github!&gt;</b></span>")
  }else if (.GlobalEnv$Mar.JoyOfQC > Mar.JoyOfQCRemote){
    updMsg =paste0(updMsg,"<span style='font-size:0.5em;color:red;'>&lt;Outdated!&gt;</span>")
  }else{
    updMsg =paste0(updMsg,"<span style='font-size:0.5em;color:blue;'>&lt;Current&gt;</span>")
  }
  
  output$versionCheck <- renderUI(HTML(updMsg))
  ####
  
  source("getHelp.R")
  
  output$getHelp<-renderUI({
    getHelp()
  })
  
  #these tags are here instead of UI because they're dynamic
  output$selDet = renderUI(textInput(inputId = "selDetInput", label = "",value = .GlobalEnv$qcprompt))
  output$unhide = renderUI("")

  populateDrops<-function(){
    
    if (.GlobalEnv$debug) print("populateDrops")
    if (!is.null(.GlobalEnv$dataObjFields)){
      if (length(.GlobalEnv$dataObjFields)<2){
        output$loadstatus <- renderText(paste0("Insufficient plottable columns!"))
        return()
      }
      output$xaxis = renderUI(selectInput("xaxis", "Select your x-axis", choices = .GlobalEnv$dataObjFields, selected = .GlobalEnv$dataObjFields[1], multiple = FALSE))
      output$yaxis = renderUI(selectInput("yaxis", "Select your y-axis", choices = .GlobalEnv$dataObjFields, selected = .GlobalEnv$dataObjFields[2], multiple = FALSE))
      if (.GlobalEnv$debug) print("axes rendered")
      if (length(.GlobalEnv$dataObjFields)>2){
        output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",.GlobalEnv$dataObjFields), multiple = FALSE))
      }else{
        output$facet = renderUI({NULL})
      }
    }
  }
  handleData<-function(type=NULL, specific=NULL, theobj=NULL){
    #if (.GlobalEnv$debug) print("handleData")
    if (.GlobalEnv$debug) print(paste0("type:",type,"   specific:",specific,"   theobj", theobj))    
    dataSelToLoad = NULL
    if (type == "sample"){
      dataSelToLoad = get(specific)
      output$saveMsg <- renderText("Sample data loaded")
    } else if (type=="local"){
      if (specific == "csvobject"){
        if (is.null(theobj)){
          output$fBrowse = renderUI(fileInput("fBrowse", "Choose *.csv file",
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv","csv"),
                                              multiple = FALSE))
          # browser()
          if (is.null(input$fBrowse))return()
        }else{
          dataSelToLoad = read.csv(theobj$datapath)
          output$saveMsg <- renderText("Local csv object loaded")
        }
      }else if (specific == "robject" ){
        if (is.null(theobj)){
          localObjs= sort(names(which(unlist(eapply(.GlobalEnv,is.data.frame)))))
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
      }else if (specific == "qcobject" ){
        if (is.null(theobj)){
          output$jBrowse = renderUI(fileInput("jBrowse", "Choose a *.joy file",
                                              accept = c("joy",".joy"),
                                              multiple = FALSE))
          if (is.null(input$jBrowse))return()
        }else{
          dataSelToLoad = read.csv(theobj$datapath)
          output$saveMsg <- renderText("Saved qc session loaded")
        } 
      }else if (specific == "rdsobject" ){
      
        if (is.null(theobj)){
          output$rdsBrowse = renderUI(fileInput("rdsBrowse", "Choose an *.rds file",
                                              accept = c("rds",".rds"),
                                              multiple = FALSE))
          if (is.null(input$rdsBrowse))return()
        }else{
          dataSelToLoad = readRDS(theobj$datapath)
          output$saveMsg <- renderText("rds file loaded")
        } 
      }else if (specific == "rdata" ){
        if (is.null(theobj)){
          output$rBrowse = renderUI(fileInput("rBrowse", "Choose an *.rdata/*.rda file",
                                              accept = c("rdata",".rdata","rda",".rda"),
                                              multiple = FALSE))
          if (is.null(input$rBrowse))return()
        }else{
          
          loadRData <- function(fileName){
            #weird function to load rdata to known name
            load(fileName)
            get(ls()[ls() != "fileName"])
          }
          dataSelToLoad <- loadRData(theobj$datapath)
          
          output$saveMsg <- renderText("Rdata loaded")
        } 
      }
    }else if (type == "oracle"){
      getOracleCreds<-function(){
        OcredNameValue = ifelse(!is.na(.GlobalEnv$OUser),.GlobalEnv$OUser,ifelse(exists("oracle.username"),oracle.username, ""))
        OcredPassValue = ifelse(!is.na(.GlobalEnv$OPW),.GlobalEnv$OPW,ifelse(exists("oracle.password"),oracle.password, ""))
        OcredDSNValue = ifelse(!is.na(.GlobalEnv$ODsn),.GlobalEnv$ODsn,ifelse(exists("oracle.dsn"),oracle.dsn, ""))
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
          .GlobalEnv$thisCxn = thisCxn
          .GlobalEnv$OUser = toupper(input$OcredName)
          .GlobalEnv$OPW = toupper(input$OcredPass)
          .GlobalEnv$ODsn = toupper(input$OcredDSN)
          output$OcredName = renderUI(textInput("OcredName","Enter your Oracle username", value = toupper(input$OcredName)))
          output$OcredPass = renderUI(passwordInput("OcredPass","Enter your Oracle password", value = toupper(input$OcredPass)))
          output$OcredDSN = renderUI(textInput("OcredDSN","Oracle DSN", value = toupper(input$OcredDSN)))
          output$OcredSubmit = renderUI(actionButton("OcredSubmit", "Go"))
        }else{
          .GlobalEnv$thisCxn = NULL
        }
        return(.GlobalEnv$thisCxn)
      }
      showSchemaPick<-function(){
        schemas = .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("select distinct(table_schema) OWNER from all_tab_privs WHERE GRANTEE = '",.GlobalEnv$OUser,"' ORDER BY OWNER"))
        output$Oschema <- renderUI(selectInput("Oschema",
                                               label = "Select a schema",
                                               choices = c("None",schemas$OWNER),
                                               selected = "None"))
        return(schemas)
      }
      showTablePick<-function(){
        tbls = .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("select TABLE_NAME from all_tab_privs WHERE table_schema = '",.GlobalEnv$Oschema,"' AND GRANTEE = '",.GlobalEnv$OUser,"' ORDER BY TABLE_NAME"))
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
        if (is.null(.GlobalEnv$thisCxn)) {
          cxn = makeOracleCxn()
          if (is.null(cxn)){
            print("cxn failed")
            return(NULL)
          }
        }
        showSchemaPick()
        
        return(NULL)
      }else if (is.null(theobj)){
        
        assign("Oschema", toupper(specific), envir = .GlobalEnv)
        #no table
        showTablePick()
      }else{
        assign("Otable", toupper(theobj), envir = .GlobalEnv)
        dataSelToLoad <- .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("SELECT * FROM ",.GlobalEnv$Oschema,".",.GlobalEnv$Otable))
        output$saveMsg <- renderText("Oracle data loaded")
      }
    }
    
    if (is.null(dataSelToLoad)) {return()}
    if (attr(class(dataSelToLoad),"package")=="sp"){
      dataSelToLoad=dataSelToLoad@data
      output$saveMsg <- renderText("file was sp:: object - loading dataframe from that object")
    }
    if (specific != "qcobject"){
      dataSelToLoad$QC_STATUS <-"UNASSESSED"
      dataSelToLoad$QC_COMMENT <-NA
      dataSelToLoad$QC_HIDDEN<-FALSE
    }
    if (.GlobalEnv$debug) print(head(dataSelToLoad))
    assign("dataObj",dataSelToLoad,envir = .GlobalEnv)
    assign("dataObjFields",colnames(dataSelToLoad),envir = .GlobalEnv)
    # paste0(a("Mar.QCJoy", href = "https://github.com/Maritimes/Mar.JoyofQC"), " <br><span style='font-size:0.5em;'>v.",.GlobalEnv$Mar.JoyOfQC," </span>")
    output$plotchk = renderUI(div(radioButtons("plotchk", label = a("Show 95% confidence intervals?    ",href="https://ggplot2.tidyverse.org/reference/geom_smooth.html", target="_blank"), 
                       choices = list("none" = "none", "auto"="auto", "lm"="lm", "glm"="glm", "gam"="gam", "loess"="loess"),
                       selected = "none")))
    # output$plotchk = renderUI(div(outputid = "plotchk",checkboxInput("showLm", label = "Show 95% confidence intervals (linear)?",value = FALSE),
    #                           checkboxInput("showLoess", label = "Show 95% confidence intervals (loess)?",value = FALSE)))
    populateDrops()
  }
  applyQC<-function(sel="normal"){
    if (.GlobalEnv$debug) print("applyQC")
    thisData = .GlobalEnv$dataObj
    if(is.null(thisData))return(NULL)
    newcomm = ifelse(input$selDetInput==.GlobalEnv$qcprompt,"", input$selDetInput)
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
    updateTextInput(session, "selDetInput", label = NULL, value = .GlobalEnv$qcprompt)
    
    if (nrow(thisData[thisData$QC_HIDDEN ==TRUE,])>0){
      output$unhide = renderUI(tagList(actionButton(inputId = 'unhide', label = 'Unhide All', icon = icon('eye'))),tags$style("#unhide","{text-align: center; vertical-align: middle;clear:both}"))
    }else{
      output$unhide = renderUI({NULL})
    }
    
    assign("dataObj",thisData,envir = .GlobalEnv)
    session$resetBrush("brush")
  }
  dataSizeChecker<-function(df){
    if (.GlobalEnv$debug) print("dataSizeChecker")
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
          div(div(style="display:inline-block;float:both", paste("Your data selection has ",nrowData," rows (i.e. ",dataSizeMB," Mb).  That's too much for this app. Please filter the data in r to create a smaller r object"))
              ,div(style="clear:both")
          )
        })
      res = "fail"
    }else if (dataSizeMB > 20){
      #upper limit warning
      output$hugeDataWarn <-
        renderUI({
          #these tags are here instead of UI so they can be on one line
          div(div(style="display:inline-block;float:both", paste("Your data selection has ",nrowData," rows (i.e. ",dataSizeMB," Mb).  That's a lot for this app.  Consider filtering the data in r to create a smaller r object."))
              ,div(style="clear:both")
          )
        })
      
      res = "warn"
    }else{
      output$hugeDataWarn <- renderUI({NULL})
    }
    return(res)
  }
  facetChecker<-function(nfacets = 1){
    res = "OK"
    if (nfacets == 1){
      #      output$facetOptions <- renderUI({NULL})
      output$facetOptions <- NULL
    }else if (nfacets<=.GlobalEnv$limitPlots){
      output$facetOptions <-
        renderUI({
          actionButton(inputId = 'facetRemover', label = paste0("Remove faceting"), icon = icon('layer-group'))
        })
      
    } else if (nfacets>.GlobalEnv$limitPlots){
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
  makePlot<-function(){
    if (.GlobalEnv$debug) print("makeplot")
    input$facetOverride
    input$handleSelected
    input$handleUnselected
    input$unhide
    input$showLm
    input$showLoess
    thisData    <- .GlobalEnv$dataObj
    dataSizeCheck = dataSizeChecker(thisData)
    if (dataSizeCheck=="fail")return(NULL)
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    
    x <- thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][!is.na(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$xaxis]),input$xaxis]
    y <- thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][!is.na(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$yaxis]),input$yaxis]
    nfacets = ifelse(input$facet == 'None',1,length(unique(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$facet])))
    facetCheck = facetChecker(nfacets)
    if (facetCheck=="warn")return(NULL)
    
    thePlot <- ggplot(data = thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),], 
                      aes(color = QC_STATUS,shape = QC_STATUS,
                          x=thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$xaxis], 
                          y=thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$yaxis])) + geom_point() + xlab(input$xaxis) + ylab(input$yaxis) + scale_shape_manual(name = "QC_STATUS", values =c("UNASSESSED" = 16, "GOOD"=17, "BAD"=15)) + scale_color_manual(name = "QC_STATUS", values =c("UNASSESSED" = "black", "GOOD"="blue", "BAD"="RED")) 
      if (!is.null(input$plotchk)){
        if (input$plotchk != 'none'){
          thePlot <- thePlot + geom_smooth(method=input$plotchk, fill="red", color="red")
        }
      }
    if(!input$facet == 'None') {
      thePlot <- thePlot + facet_wrap(as.formula(paste('~', input$facet)),ncol=3)
    }
    thePlot <- thePlot + theme(legend.position="top") + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
    return(thePlot)
  }
  makeSelDataTable<-function(){
    
    if (.GlobalEnv$debug) print("makeSelDataTable")
    input$handleSelected
    input$handleUnselected
    input$unhide
    thisData<-.GlobalEnv$dataObj 
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    tableData = brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                              input$brush,
                              xvar = input$xaxis,
                              yvar = input$yaxis)
    tableData = tableData[, !names(tableData) %in% c("QC_HIDDEN")] 
    tableData = Mar.utils::drop_NA_cols(tableData)
    return(tableData)
  }
  
  observeEvent(input$fBrowse,{
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$fBrowse)
  })
  observeEvent(input$jBrowse,{
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$jBrowse)
  })
  observeEvent(input$rBrowse,{
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$rBrowse)
  })
  observeEvent(input$rdsBrowse,{
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$rdsBrowse)
  })
  observeEvent(input$rObjSel,{
    #this ensures that if an r object is picked, it gets handled
    handleData("local", input$dataSel, input$rObjSel)
  })
  observeEvent(input$OcredSubmit,{
    if (!is.null(.GlobalEnv$thisCxn)){
      assign("thisCxn", NULL, envir = .GlobalEnv)
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
    assign("dataObj",NULL, .GlobalEnv)
    assign("dataObjFields",NULL, .GlobalEnv)
    output$xaxis = renderUI({NULL})
    output$yaxis = renderUI({NULL})
    output$facet = renderUI({NULL})
    output$saveMsg = renderText("Nothing to report")
    output$loadstatus =renderUI({NULL})
    output$facetText =renderUI({NULL})
    output$fBrowse = renderUI({NULL})
    output$jBrowse = renderUI({NULL})
    output$rBrowse = renderUI({NULL})
    
    output$rdsBrowse = renderUI({NULL})
    output$rObjSel = renderUI({NULL})
    output$OcredUsePkg = renderUI(NULL)
    output$OcredName = renderUI({NULL})
    output$OcredPass = renderUI({NULL})
    output$OcredDSN = renderUI({NULL})
    output$OcredSubmit = renderUI({NULL})
    output$Oschema = renderUI({NULL})
    output$Otable = renderUI({NULL})
    ranges$x <- NULL
    ranges$y <- NULL
    output$resetZoom = renderUI(NULL)
    
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
  })
  # observeEvent(input$showLm,{})
  # observeEvent(input$showLoess,{
  #  # assign("showLoess", input$showLoess, envir = .GlobalEnv)
  # })
  observeEvent(input$unhide, {
    thisData = .GlobalEnv$dataObj
    thisData[thisData$QC_HIDDEN ==TRUE,"QC_HIDDEN"]<-FALSE
    assign("dataObj",thisData,envir = .GlobalEnv)
    output$unhide = renderUI({NULL})
  })
  observeEvent(input$saveDet,{
    #This saves the data so that you can use it to fix the original source
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    fn = paste0("qcResults_",ts,".csv")
    thisData = .GlobalEnv$dataObj
    thisData=thisData[thisData$QC_STATUS != "UNASSESSED",]
    assign(paste0("qcResults_",ts), thisData,envir = .GlobalEnv)
    write.csv(thisData,fn, row.names = FALSE)
    output$saveMsg <- renderText(paste0("Output available in R as ",paste0("qcResults_",ts)," and saved as ", paste0(getwd(),"/",fn)))
  })
  observeEvent(input$saveSess,{
    #This saves the data so that you can open it up again and keep working
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    fn = paste0("qcSession_",ts,".joy")
    thisData = .GlobalEnv$dataObj
    #assign(paste0("qcSession_",ts), thisData,envir = .GlobalEnv)
    write.csv(thisData, fn, row.names = FALSE)
    output$saveMsg <- renderText(paste0("Session saved as ", paste0(getwd(),"/",fn)))
  })
  observeEvent(input$unhide,{
    thisData    <- .GlobalEnv$dataObj
    thisData$QC_HIDDEN <-FALSE
    assign("dataObj",thisData,envir = .GlobalEnv)
  })
  observeEvent(input$facetOverride,{
    if (.GlobalEnv$limitPlots ==20){
      assign("limitPlots", 500, envir = .GlobalEnv)
    }else{
      assign("limitPlots", 20, envir = .GlobalEnv)
    }
  })
  observeEvent(input$facetRemover,{
    assign("limitPlots", 20, envir = .GlobalEnv)
    output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",.GlobalEnv$dataObjFields), selected = "None", multiple = FALSE))
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
    thisData = .GlobalEnv$dataObj
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    brushed=brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                          input$brush,
                          xvar = input$xaxis,
                          yvar = input$yaxis)
    return(brushed)
  })
  getPlotHeight <- reactive({
    input$facetOverride
    input$facet
    if (is.null(input$facet))return(300)
    thisData    <- .GlobalEnv$dataObj
    if (is.null(thisData))return(1)
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    nfacets = ifelse(input$facet == 'None',1,length(unique(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$facet])))
    facetCheck = facetChecker(nfacets)
    if (facetCheck=="warn")return(1)
    
    plotRows = ceiling(nfacets/3)
    plotHeight = plotRows*300
    if (nfacets>.GlobalEnv$limitPlots){
      if (is.null(input$facetOverride)){
        plotHeight = 1
      }
    }
    return(plotHeight)
  })  

#height =function(){getPlotHeight()}
  output$distPlot <- renderPlot(height =function(){getPlotHeight()},{
    req(input$xaxis, input$yaxis)
   
    if (is.null(.GlobalEnv$dataObj)){
      return()
    }
    if (input$xaxis == input$yaxis){
      return()
    }
    
    makePlot()
  })
  output$selTable <- renderDataTable({
    req(input$xaxis, input$yaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    res = makeSelDataTable()
    return(res)
  })
}
