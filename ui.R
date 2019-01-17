library(shiny)
library(ggplot2)
ui<-fluidPage(theme = "styles.css",
              titlePanel(div(id="logodiv",img(src="logo.PNG", height=100))),
              sidebarLayout(
                sidebarPanel(width = 3
                             ,htmlOutput("dataSel")
                             ,htmlOutput("rObjSel")
                             ,htmlOutput("fBrowse")
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
                             ,textOutput("loadstatus")
                ), 
                mainPanel(id = "mainPanel",
                          tabsetPanel(id="inTabset"
                                      , tabPanel("Results"
                                                 ,div(id = "editOptions",
                                                      div(style="display:inline-block;float:both",radioButtons(inputId = 'qcAction', label = "",
                                                                                                               choiceNames = list(icon('thumbs-up'),icon('thumbs-down'),icon('eye-slash')),
                                                                                                               choiceValues = list("good","bad","ugly"), inline=T))
                                                      ,div(style="display:inline-block;float:both",htmlOutput("selDet"))
                                                      ,div(style="display:inline-block;float:both",actionButton('handleSelect', 'Apply Options'))
                                                      ,div(style="display:inline-block;float:both",htmlOutput("unhide"))
                                                      ,div(style="clear:both")
                                                      ,div(style="display:inline-block;float:both",radioButtons(inputId = 'saveAction', label = "",
                                                                                                                choiceNames = list("All","Flagged Data Only"),
                                                                                                                choiceValues = list("saveAll","saveFlagged"), inline=T))
                                                      ,div(style="display:inline-block;float:both",actionButton(inputId = 'saveDet', label = 'Save Data', icon = icon('save')))
                                                      ,div(style="display:inline-block;float:both",htmlOutput("saveMsg"))
                                                 )
                                                 ,plotOutput(outputId = "distPlot", brush = brushOpts(id = 'brush'))
                                                 ,htmlOutput("facetOptions")
                                                 ,dataTableOutput("selTable")
                                      )
                                      , tabPanel("Help"
                                                 ,htmlOutput("getHelp")
                                      )
                          )
                )
              )
)