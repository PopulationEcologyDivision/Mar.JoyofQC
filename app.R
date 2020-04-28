setwd(".")
ui = source('./scripts/ui.R', local = TRUE)$value
server = source('./scripts/server.R', local = TRUE)$value

shinyApp(ui=ui, server=server)