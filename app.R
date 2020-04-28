ui = source('./scripts/ui.R', local = TRUE)$value

server = function(input, output) {
	  ## rwData
	  source('./scripts/server.R', local = TRUE)$value
	}


shinyApp(ui=ui, server=server)