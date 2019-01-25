library(shiny)
library(ggplot2)
library(tidyr)
ui<-fluidPage( #tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  theme = "styles.css"
  ,titlePanel(windowTitle = "Mar.QCJoy", div(id="logodiv",img(src="logo.PNG", height=100)))
  ,sidebarLayout(
    sidebarPanel(width = 3
                 ,selectInput("dataSel", label ="Select some data",
                              choices=c("None"="None",
                                        "*.csv" = "csvobject",
                                        "r object" = "robject",
                                        "*.joy (old qc session)" = "qcobject",
                                        "Oracle" = "oracle",
                                        "<sample data>"="mtcars"))
                 ,htmlOutput("rObjSel")
                 ,htmlOutput("fBrowse")
                 ,htmlOutput("jBrowse")
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
                 ,actionButton(inputId = 'saveDet', label = 'Save QC info', icon = icon('save'))
                 ,actionButton(inputId = 'saveSess', label = 'Save QC session', icon = icon('coffee'))
    )
    ,mainPanel(id = "mainPanel"
               ,div(id = "editOptions",style="display:inline-block;float:both"
                    ,div(id="qcOpts",
                         radioButtons(inputId = 'qcAction', label = "",
                                      choiceNames = list(icon('undo'),icon('thumbs-up'),icon('thumbs-down'),icon('eye-slash')),
                                      choiceValues = list("undo","good","bad","ugly"), selected = "bad", inline=T)
                         ,htmlOutput("selDet")
                         ,div(id = "chk", checkboxInput("hideHandled", label = "Hide QC'd data",value = FALSE))
                         ,htmlOutput("unhide")
                    )
                    ,div(class="clearer")
                    ,actionButton('handleSelected', label = "Apply to Selected Points", icon('arrow-right'))
                    ,actionButton('handleUnselected', label = "Apply to Unselected Points", icon('random'))
               )
               , tabsetPanel(id="inTabset2",
                             tabPanel("Results"
                                      ,htmlOutput("facetOptions")
                                      ,htmlOutput("hugeDataWarn")
                                      ,plotOutput(height="100%", outputId = "distPlot", brush = brushOpts(id = 'brush'))
                                      ,dataTableOutput("selTable")
                                      # ) , tabPanel("Plot-splosion"
                                      #              ,plotOutput(height="100%", outputId = "distPlotMega", brush = brushOpts(id = 'brushMega'))
                                      #              ,dataTableOutput("selTableMega")
                             ) ,tabPanel("Help"
                                         ,htmlOutput("getHelp")
                             )
               ) 
    )
  ),
  tagList(hr()
          ,textOutput("saveMsg")
          ,div(htmlOutput(outputId="versionCheck")))
)