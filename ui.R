library(shiny)
library(ggplot2)

ui<-fluidPage(  #tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  theme = "styles.css",
  titlePanel(windowTitle = "Mar.JoyofQC"
             , div(id="logodiv",img(src="logo.PNG", height=100, style="float:left;") 
                   ,htmlOutput(outputId="versionCheck")
                   ,div(class="clearer")
             )
  )
       ,sidebarLayout(
         sidebarPanel(id = "sb",
                      tabsetPanel(id="inTabset1",
                                  tabPanel("Data",
                                           div(class = "sidebox",
                                               selectInput("dataSel", label ="Select data type",
                                                           choices=c("None"="None",
                                                                     "local r object" = "robject",
                                                                     "*.csv, *.rds, *.rdata or *.joy file" = "csvobject",
                                                                     "Oracle" = "oracle",
                                                                     "<sample data>"="mtcars")
                                               )
                                               ,htmlOutput("rObjSel")
                                               ,htmlOutput("fBrowse")
                                               ,htmlOutput("hugeDataWarn")
                                               ,htmlOutput("OcredUsePkg")
                                               ,htmlOutput("OcredName")
                                               ,htmlOutput("OcredPass")
                                               ,htmlOutput("OcredDSN")
                                               ,htmlOutput("OcredSubmit")
                                               ,htmlOutput("Oschema")
                                               ,htmlOutput("Otable")  
                                               ,htmlOutput("xaxis")
                                               ,htmlOutput("yaxis")
                                               ,htmlOutput("facet")
                                               ,htmlOutput("facetOptions")
                                               ,htmlOutput("plotchk")
                                           )
                                  ),
                                  tabPanel("Filter",
                                           div(class = "sidebox",
                                               div(id = "filtOptions"
                                                   ,htmlOutput("filtFields")
                                                   ,htmlOutput("filtFieldVals")
                                                   ,htmlOutput("filtApply")
                                                   ,htmlOutput("filtRem")
                                               )
                                           )
                                  ),
                                  tabPanel("QC",
                                           div(class = "sidebox",
                                               radioButtons(inputId = 'qcAction', label = "",
                                                            choiceNames = list(icon('undo'),icon('thumbs-up'),icon('thumbs-down'),icon('eye-slash')),
                                                            choiceValues = list("undo","good","bad","ugly"), selected = "bad", inline=T)
                                               ,htmlOutput("selDet")
                                               ,div(id = "chk", checkboxInput("hideHandled", label = "Hide QC'd data",value = FALSE))
                                               ,div(class="clearer")
                                               ,htmlOutput("unhideBox")
                                               ,div(class="clearer")
                                               ,actionButton('handleSelected', label = "Apply to Selected Points", icon('arrow-right'))
                                               ,actionButton('handleUnselected', label = "Apply to Unselected Points", icon('random'))
                                               ,div(class="clearer")
                                           )
                                  ),
                                  tabPanel("Save",
                                           div(class = "sidebox",
                                               div(class="Pseudolabel", span("Save your work"))
                                               ,actionButton(inputId = 'saveSess', label = 'Save QC session', icon = icon('coffee'))
                                               ,actionButton(inputId = 'saveDet', label = 'Save QC info', icon = icon('save'))
                                               ,div(id = "saveNULL", checkboxInput("includeNULLS", label = "Include records that had NULL for values of X,Y or facet fields in saved file",value = FALSE))
                                           )
                                           ,div(class="Pseudolabel", span("Notices")
                                           )
                                  )
                      )
                      
                      ,htmlOutput("resetZoom")
                      ,div(textOutput("saveMsg"))
                      ,div(class="clearer")
         )
         
         ,mainPanel(id = "mainPanel"
                    , tabsetPanel(id="inTabset2",
                                  tabPanel("Results"
                                           ,div(id = "editOptions"
                                                ,plotOutput(height="100%", outputId = "distPlot", 
                                                            dblclick = "distPlot_dblclick", 
                                                            brush = brushOpts(id = 'brush',
                                                                              resetOnNew = TRUE))
                                                
                                                ,dataTableOutput("selTable")
                                           )
                                  ) ,tabPanel("Help"
                                              ,htmlOutput("getHelp")
                                  )
                                  
                    )
                    
                    
         )
       )
)