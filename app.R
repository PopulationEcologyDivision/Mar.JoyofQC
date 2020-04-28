
ui = source('./ui.R', local = TRUE)$value

server = function(input, output, session) {
	  ## rwData
	  source('./server.R', local = TRUE)$value
	}
}

shinyApp(ui, server)