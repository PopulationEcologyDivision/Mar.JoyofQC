library(shiny)
library(ggplot2)
library(tidyr)
library(shinyjs)
ui<-fluidPage(  #tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  theme = "styles.css"
  ,titlePanel(windowTitle = "Mar.QCJoy"
              , div(id="logodiv",img(src="logo.PNG", height=100, style="float:left;") 
                    ,htmlOutput(outputId="versionCheck"),div(class="clearer")))
  ,sidebarLayout(
    sidebarPanel(selectInput("dataSel", label ="Select data type",
                             choices=c("None"="None",
                                       "*.csv file" = "csvobject",
                                       "local r object" = "robject",
                                       "*.rda/*.rdata file" = "rdata",
                                       "*.rds file" = "rdsobject",
                                       "*.joy (old qc session)" = "qcobject",
                                       "Oracle" = "oracle",
                                       "<sample data>"="mtcars"))
                 ,htmlOutput("rObjSel")
                 ,htmlOutput("fBrowse")
                 ,htmlOutput("jBrowse")
                 ,htmlOutput("rBrowse")
                 ,htmlOutput("rdsBrowse")
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
                 ,div(class="clearer")
                 ,div(textOutput("saveMsg")
                 ))
    ,mainPanel(id = "mainPanel"
               , tabsetPanel(id="inTabset2",
                             tabPanel("Results"
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
                                           ,htmlOutput("plotchk")
                                           ,htmlOutput("hugeDataWarn")
                                           ,htmlOutput("facetOptions")
                                           ,htmlOutput("resetZoom")
                                           ,plotOutput(height="100%", outputId = "distPlot", 
                                                       dblclick = "distPlot_dblclick", 
                                                       brush = brushOpts(id = 'brush',
                                                                         resetOnNew = TRUE))
                                           ,dataTableOutput("selTable"))
                             ) ,tabPanel("Help"
                                         ,htmlOutput("getHelp")
                             )
               ) 
    )
  ),
  tagList(hr()
  )
)