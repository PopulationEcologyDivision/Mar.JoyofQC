server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)  #max Upload size jacked to 50MB
  options(stringsAsFactors = F)
  .GlobalEnv$dataObj = NULL
  .GlobalEnv$dataObjFields = NULL
  .GlobalEnv$thisCxn = NULL
  .GlobalEnv$limitPlots = 20
  .GlobalEnv$Oschema <- "None"
  .GlobalEnv$Otable <- "None"
  .GlobalEnv$qcprompt = "QC Comment (optional)"
  
  source("getHelp.R")
  
  output$getHelp<-renderUI({
    getHelp()
  })
  
  output$dataSel<-renderUI({
    selectInput("dataSel", label ="Select some data",
                choices=c("None"="None",
                          "local csv" = "csvobject",
                          "local r object" = "robject",
                          "oracle" = "oracle",
                          "sample data"="faithful"))
  })
  
  #these tags are here instead of UI because they're dynamic
  output$selDet = renderUI(textInput(inputId = "selDet", label = "",value = .GlobalEnv$qcprompt))
  output$unhide = renderUI("")
  output$saveMsg = renderUI(textOutput('saveMsg'))
  
  populateDrops<-function(){
    if (!is.null(.GlobalEnv$dataObjFields)){
      if (length(.GlobalEnv$dataObjFields)<2){
        output$loadstatus <- renderText(paste0("Insufficient plottable columns!"))
        return()
      }
      output$xaxis = renderUI(selectInput("xaxis", "Select your x-axis", choices = .GlobalEnv$dataObjFields, selected = .GlobalEnv$dataObjFields[1], multiple = FALSE))
      output$yaxis = renderUI(selectInput("yaxis", "Select your y-axis", choices = .GlobalEnv$dataObjFields, selected = .GlobalEnv$dataObjFields[2], multiple = FALSE))
      if (length(.GlobalEnv$dataObjFields)>2){
        output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",.GlobalEnv$dataObjFields), multiple = FALSE))
      }else{
        output$facet = NULL
      }
    }
  }
  handleData<-function(type=NULL, specific=NULL, theobj=NULL){
    
    ##########
    #this area is just for setting ui boxes to NUll - other logic is below
    output$xaxis = NULL
    output$yaxis = NULL
    output$facet = NULL
    output$saveMsg = NULL
    
    if (type != "oracle"){
      output$OcredUsePkg = NULL
      output$OcredName = NULL
      output$OcredPass = NULL
      output$OcredDSN = NULL
      output$OcredSubmit = NULL
      output$Oschema = NULL
      output$Otable = NULL
    }
    if (exists("specific")){
      if (type != "local"){
        output$fBrowse = NULL
        output$rObjSel = NULL
      }else if (specific != "csvobject"){
        output$fBrowse = NULL
      }else if (specific != "robject"){
        output$rObjSel = NULL
      }
    }
    ##########
    
    dataSelToLoad = NULL
    if (type == "sample"){
      dataSelToLoad = get(specific)
    }
    
    if (type == "local"){
      if (specific=="robject"){
        if (is.null(theobj)){
          localObjs= names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
          if (length(localObjs)<1){
            output$rObjSel = NULL
          }else{
            output$rObjSel= renderUI(selectInput("rObjSel","Local R Object", c("None",localObjs)))
          }
          dataSelToLoad = NULL
        }else{
          if (theobj == "None")return()
          dataSelToLoad = get(theobj)
        }
      } else if (specific == "csvobject"){
        if (is.null(theobj)){
          output$fBrowse = renderUI(fileInput("fBrowse", "Choose CSV File",
                                              accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"),
                                              multiple = FALSE))
          if (is.null(input$fBrowse))return()
        }else{
          dataSelToLoad = read.csv(theobj$datapath)
        }
      }
    }
    if (type == "oracle"){
      dataSelToLoad <-NULL
      #######
      #start of Oracle-specific functions    
      getOracleCreds<-function(){
        OcredNameValue = ifelse(exists("oracle.username"),oracle.username, "")
        OcredPassValue = ifelse(exists("oracle.password"),oracle.password, "")
        OcredDSNValue = ifelse(exists("oracle.dsn"),oracle.dsn, "")
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
        }else{
          .GlobalEnv$thisCxn = NULL
        }
        return(.GlobalEnv$thisCxn)
      }
      showSchemaPick<-function(){
        schemas = .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("select distinct(table_schema) OWNER from all_tab_privs WHERE GRANTEE = '",toupper(input$OcredName),"' ORDER BY OWNER"))
        output$Oschema <- renderUI(selectInput("Oschema",
                                               label = "Select a schema",
                                               choices = c("None",schemas$OWNER),
                                               selected = "None"))
        return(NULL)
      }
      showTablePick<-function(){
        tbls = .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("select TABLE_NAME from all_tab_privs WHERE table_schema = '",input$Oschema,"' AND GRANTEE = '",toupper(input$OcredName),"' ORDER BY TABLE_NAME"))
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
        }
        showSchemaPick()
        return(NULL)
      }else if (is.null(theobj)){
        #no table
        showTablePick()
      }else{
        dataSelToLoad <- .GlobalEnv$thisCxn$thecmd(.GlobalEnv$thisCxn$channel,paste0("SELECT * FROM ",input$Oschema,".",input$Otable))
      }
    }
    
    if (is.null(dataSelToLoad)) {return()}
    dataSelToLoad$QC_STATUS <-"UNASSESSED"
    dataSelToLoad$QC_COMMENT <-NA
    dataSelToLoad$QC_HIDDEN<-FALSE
    assign("dataObj",dataSelToLoad,envir = .GlobalEnv)
    assign("dataObjFields",colnames(dataSelToLoad),envir = .GlobalEnv)
    
    output$xaxis = NULL
    output$yaxis = NULL
    output$facet = NULL
    output$loadstatus = NULL
    output$facetText = NULL
    if (!is.null(theobj)){
      if (specific =="csvobject") output$rObjSel = NULL
      if (specific =="robject") output$fBrowse = NULL
    }
    if (type !="oracle"){
      output$OcredUsePkg = NULL
      output$OcredName = NULL
      output$OcredPass = NULL
      output$OcredDSN = NULL
      output$OcredSubmit = NULL
      output$Oschema = NULL
      output$Otable = NULL
    }
    populateDrops()
  }
  
  
  observeEvent(input$fBrowse,{
    #this ensures that if a file is input, it gets handled
    handleData("local", input$dataSel, input$fBrowse)
  })
  observeEvent(input$rObjSel,{
    #this ensures that if an r object is picked, it gets handled
    handleData("local", input$dataSel, input$rObjSel)
  })
  
  observeEvent(input$OcredSubmit,{
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
    if (input$dataSel %in% c("faithful","mtcars","pressure")){
      handleData("sample",input$dataSel, NULL)
    } else if (input$dataSel %in% c("robject","csvobject")){
      handleData("local", input$dataSel, NULL)
    }else if (input$dataSel %in% c("oracle")){
      handleData("oracle", NULL, NULL)
    }
  })
  
  observeEvent(
    input$handleSelect
    , {
      thisData = .GlobalEnv$dataObj
      comm = ifelse(input$selDet==.GlobalEnv$qcprompt,NA, input$selDet)
      selected <- selected()
      if (input$qcAction == "good"){
        thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"GOOD"
        output$saveMsg <- renderText("")
      }else if (input$qcAction == "bad"){
        thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"BAD"
        output$saveMsg <- renderText("")
      }else if (input$qcAction == "ugly"){
        thisData[rownames(thisData) %in% rownames(selected),"QC_HIDDEN"]<-TRUE
      }
      thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]<-comm
      updateTextInput(session, "selDet", label = NULL, value = .GlobalEnv$qcprompt)
      
      if (nrow(thisData[thisData$QC_HIDDEN ==TRUE,])>0){
        output$unhide = renderUI(actionButton(inputId = 'unhide', label = 'Unhide All', icon = icon('eye')))
      }else{
        output$unhide = renderUI("")
      }
      
      assign("dataObj",thisData,envir = .GlobalEnv)
      session$resetBrush("brush")
    })
  
  
  observeEvent(
    input$unhide
    , {
      thisData = .GlobalEnv$dataObj
      thisData[thisData$QC_HIDDEN ==TRUE,"QC_HIDDEN"]<-FALSE
      assign("dataObj",thisData,envir = .GlobalEnv)
      output$unhide = renderUI("")
    })
  
  
  observeEvent(
    input$saveDet,{
      browser()
      ts = format(Sys.time(), "%Y%m%d_%H%M")
      thisData = .GlobalEnv$dataObj
      thisData = thisData[, !names(thisData) %in% c("QC_HIDDEN")] 
      if (input$saveAction == "saveFlagged"){
        thisData=thisData[thisData$QC_STATUS != "UNASSESSED",]
      }
      #if we find a column that is unique for each row - keep that
      intcols = lapply(thisData, function(x) unique(x))
      unique_cols = names(intcols)[lengths(intcols) == nrow(thisData)]
      if (length(unique_cols)>0){
        fields = c(unique_cols,"QC_STATUS","QC_COMMENT")
      }else{
        fields = colnames(thisData)
      }
      thisData = thisData[with(thisData, order(QC_STATUS, QC_COMMENT, thisData[fields[1]])), fields]
      assign(paste0("qcResults_",ts), thisData,envir = .GlobalEnv)
      write.csv(thisData, paste0("qcResults_",ts,".csv"), row.names = FALSE)
      output$saveMsg <- renderText("Output Saved")
    })
  
  observeEvent(input$unhide,{
    thisData    <- .GlobalEnv$dataObj
    thisData$QC_HIDDEN <-FALSE
    assign("dataObj",thisData,envir = .GlobalEnv)
  })
  
  observeEvent(
    input$facetOverride,{
      #browser()
      if (.GlobalEnv$limitPlots ==20){
        assign("limitPlots", 500, envir = .GlobalEnv)
      }else{
        assign("limitPlots", 20, envir = .GlobalEnv)
      }
    }
  )
  observeEvent(
    input$facetRemover,{
      assign("limitPlots", 20, envir = .GlobalEnv)
      output$facet = renderUI(selectInput("facet", "Select a field to facet by", choices = c("None",.GlobalEnv$dataObjFields), selected = "None", multiple = FALSE))
    }
  )
  
  observeEvent(input$showHelp, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Help")
  })
  
  selected <- reactive({
    input$brush
    thisData = .GlobalEnv$dataObj
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    brushed=brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                          input$brush,
                          xvar = input$xaxis,
                          yvar = input$yaxis)
    return(brushed)
  })
  
  facetChecker<-function(nfacets = 1){
    res = "OK"
    if (nfacets == 1){
      output$facetOptions <- NULL
    }else if (nfacets<=.GlobalEnv$limitPlots){
      output$facetOptions <-
        renderUI({
          #these tags are here instead of UI so they can be on one line
          div(div(style="display:inline-block;float:both",actionButton(inputId = 'facetRemover', label = paste0("Remove faceting"), icon = icon('layer-group')))
              ,div(style="clear:both")
          )
        })
    } else if (nfacets>.GlobalEnv$limitPlots){
      output$facetOptions <-
        renderUI({
          #these tags are here instead of UI so they can be on one line
          div(div(style="display:inline-block;float:both", paste(nfacets, ' has ',nfacets,' unique values.  Perhaps too many to usefully facet by and QC?'))
              , div(style="display:inline-block;float:both",actionButton(inputId = 'facetOverride', label = paste0("Thanks, 'Dad', show all ",nfacets,"(!) plots"), icon = icon('layer-group')))
              ,div(style="clear:both")
          )
        })
      res = "warn"
    }
    return(res)
  }
  makePlot<-function(){
    input$facetOverride
    input$handleSelect
    input$unhide
    thisData    <- .GlobalEnv$dataObj
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
    if(!input$facet == 'None') thePlot <- thePlot + facet_wrap(as.formula(paste('~', input$facet)))
    thePlot
  }
  makeSelDataTable<-function(){
    input$handleSelect
    input$unhide
    thisData<-.GlobalEnv$dataObj 
    thisData = thisData[!thisData$QC_HIDDEN %in% TRUE,]
    tableData = brushedPoints(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),],
                              input$brush,
                              xvar = input$xaxis,
                              yvar = input$yaxis)
    tableData = tableData[, !names(tableData) %in% c("QC_HIDDEN")] 
    return(tableData)
  }
  output$distPlot <- renderPlot({
    req(input$xaxis, input$yaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    if (input$xaxis == input$yaxis)return()
    makePlot()
  })
  output$selTable <- renderDataTable({
    req(input$xaxis, input$yaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    makeSelDataTable()
  })
}
