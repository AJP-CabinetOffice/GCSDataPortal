shiny::runApp(paste("webapp", "app.R", sep = "/"),
              host = "0.0.0.0",
              port = strtoi(Sys.getenv("PORT")))