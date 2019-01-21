library(shiny)
library(ggplot2)
library(tidyr)
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
                )
                ,mainPanel(id = "mainPanel"
                           # tabsetPanel(id="inTabset"
                           #             , tabPanel("Results"
                           ,div(id = "editOptions",
                                div(style="display:inline-block;float:both",radioButtons(inputId = 'qcAction', label = "",
                                                                                         choiceNames = list(icon('thumbs-up'),icon('thumbs-down'),icon('eye-slash')),
                                                                                         choiceValues = list("good","bad","ugly"), selected = "bad", inline=T))
                                ,div(style="display:inline-block;",htmlOutput("selDet"))
                                ,div(style="display:inline-block;",checkboxInput("hideHandled", "Hide QC'd data",value = FALSE))
                                ,div(style="display:inline-block;",actionButton('handleSelect', 'Apply Options'))
                                ,div(style="display:inline-block;",htmlOutput("unhide"))
                                ,div(style="clear:both")
                                ,div(style="display:inline-block;",radioButtons(inputId = 'saveAction', label = "",
                                                                                choiceNames = list("All","Flagged Data Only"),
                                                                                choiceValues = list("saveAll","saveFlagged"), inline=T))
                                ,div(style="display:inline-block;",actionButton(inputId = 'saveDet', label = 'Save Data', icon = icon('save')))
                                ,div(style="display:inline-block;",htmlOutput("saveMsg"))
                           )
                           , tabsetPanel(id="inTabset2",
                                         tabPanel("Results"
                                                  ,htmlOutput("facetOptions")
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
              )
)