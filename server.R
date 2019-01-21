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
                          "sample data"="mtcars"))
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
          localObjs= sort(names(which(unlist(eapply(.GlobalEnv,is.data.frame)))))
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
    print("submitted")
    if (!is.null(.GlobalEnv$thisCxn)){
      assign("thisCxn", NULL, envir = .GlobalEnv)
      output$Oschema = NULL
      output$Otable = NULL
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
      if(is.null(thisData))return(NULL)
      newcomm = ifelse(input$selDet==.GlobalEnv$qcprompt,"", input$selDet)
      selected <- selected()
      #browser()
      if (input$qcAction == "good"){
        thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"GOOD"
        output$saveMsg <- renderText("")
      }else if (input$qcAction == "bad"){
        thisData[rownames(thisData) %in% rownames(selected),"QC_STATUS"]<-"BAD"
        output$saveMsg <- renderText("")
      }
      
      if (input$qcAction == "ugly" | input$hideHandled ==T){
        thisData[rownames(thisData) %in% rownames(selected),"QC_HIDDEN"]<-TRUE
      }
      
      
      thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]<-paste0(ifelse(is.na(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"]) | nchar(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"])<1,"",paste0(thisData[rownames(thisData) %in% rownames(selected),"QC_COMMENT"],",")),newcomm)
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
  
  # selectedMega <- reactive({
  #   input$brushMega
  #   thisData = getMegaData(coreField=input$xaxis)
  #   brushedMega=brushedPoints(thisData,
  #                 input$brushMega,
  #                 xvar = 'value',
  #                 yvar = input$xaxis)
  #   print("making sel")
  #   print(brushedMega)
  #   return(brushedMega)
  # })
  # 
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
          div(div(style="display:inline-block;float:both", paste(input$facet, ' has ',nfacets,' unique values.  Perhaps too many to usefully facet by and QC?'))
              , div(style="display:inline-block;float:both",actionButton(inputId = 'facetOverride', label = paste0("Thanks, 'Dad', show all ",nfacets,"(!) plots"), icon = icon('layer-group')))
              ,div(style="clear:both")
          )
        })
      res = "warn"
    }
    return(res)
  }
  
  getPlotHeight <- reactive({
    input$facet
    if (is.null(input$facet))return(300)
    thisData    <- .GlobalEnv$dataObj
    if (is.null(thisData))return(1)
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    nfacets = ifelse(input$facet == 'None',1,length(unique(thisData[which(!is.na(thisData[,input$xaxis]) & !is.na(thisData[,input$yaxis])),][,input$facet])))
    plotRows = ceiling(nfacets/3)
    plotHeight = plotRows*300
    if (nfacets>.GlobalEnv$limitPlots){
      if (is.null(input$facetOverride)){
        plotHeight = 1
      }
    }
    return(plotHeight)
  })  
  getMegaPlotHeight <- reactive({
    input$xaxis
    thisData<- .GlobalEnv$dataObj
    if (is.null(thisData))return(1)
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    thisData = thisData[, !names(thisData) %in% c("QC_COMMENT", "QC_HIDDEN", "QC_STATUS")] 
    nplots = ncol(thisData)-1
    plotRows = ceiling(nplots/3)
    plotHeight = plotRows*300
    # print(plotHeight)
    return(plotHeight)
  })
  
  getMegaData = function(coreField = NULL){
    if (is.null(.GlobalEnv$dataObj))return()
    thisData    <- .GlobalEnv$dataObj
    thisData <-thisData[!thisData$QC_HIDDEN %in% TRUE,]
    thisData = thisData[!is.na(thisData[coreField]),]
    thisData = thisData[, !names(thisData) %in% c("QC_COMMENT", "QC_HIDDEN", "QC_STATUS")] 
    thisData = thisData %>% gather(-coreField , key = "var", value = "value")  
    return(thisData)
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
    if(!input$facet == 'None') {
      thePlot <- thePlot + facet_wrap(as.formula(paste('~', input$facet)),ncol=3)
      
    }
    thePlot <- thePlot + theme(legend.position="top")
    return(thePlot)
  }

  
  makeMegaPlot<-function(coreField){
    input$xaxis
    thisData = getMegaData(coreField)
    thisMegaPlot =ggplot(data = thisData, aes(x = coreField, y = value )) + geom_point() + scale_shape_manual(name = "QC_STATUS", values =c("UNASSESSED" = 16, "GOOD"=17, "BAD"=15)) + scale_color_manual(name = "QC_STATUS", values =c("UNASSESSED" = "black", "GOOD"="blue", "BAD"="RED")) + facet_wrap(~ var,ncol=3) + theme(legend.position="top")
    return(thisMegaPlot)
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
    tableData = Mar.utils::drop_NA_cols(tableData)
    return(tableData)
  }
  makeSelDataTableMega<-function(coreField =NULL){
    input$handleSelect
    input$unhide
    thisData = getMegaData(coreField)

    tableData=brushedPoints(thisData,
                            input$brushMega,
                            xvar = 'value',
                            yvar = input$xaxis)
     print("making table")
    print(tableData)
    #if (nrow(tableData)==0)browser()
    return(tableData)
  }
  
  output$distPlot <- renderPlot(height =function(){getPlotHeight()},{
    req(input$xaxis, input$yaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    if (input$xaxis == input$yaxis)return()
    makePlot()
  })
  output$selTable <- renderDataTable({
    req(input$xaxis, input$yaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    res = makeSelDataTable()
    return(res)
  })
  
  
  output$selTableMega <- renderDataTable({
    req(input$xaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    res = makeSelDataTableMega(coreField = input$xaxis)
      return(res)
  })
  
  output$distPlotMega <- renderPlot(height =function(){getMegaPlotHeight()},{
    req(input$xaxis)
    if (is.null(.GlobalEnv$dataObj))return()
    makeMegaPlot(coreField = input$xaxis)
  })
}
