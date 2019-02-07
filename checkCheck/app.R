
library(shiny)
ui <- fluidPage(
   titlePanel("checkCheck"),
   sidebarLayout(
      sidebarPanel(
          checkboxInput("myCheck", FALSE,label = "testCheck")
      ),
      mainPanel(
         plotOutput("distPlot")
      )
   )
)
server <- function(input, output) {
  .GlobalEnv$nnn<-10
  observeEvent(input$myCheck,{
  assign('nnn',.GlobalEnv$nnn*2,.GlobalEnv)
  })
  
  makePlot<-function(nnn = .GlobalEnv$nnn){
    thedata = cbind(x=sort(runif(nnn, min=0, max=100)), y=sort(runif(nnn, min=0, max=100)))
  }
   output$distPlot <- renderPlot({
     input$myCheck
     plot(makePlot(.GlobalEnv$nnn))
   })
}
shinyApp(ui = ui, server = server)

