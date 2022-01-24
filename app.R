library(magrittr)

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  # Application title
  shiny::titlePanel("RGCS Data Portal"),
  shiny::fileInput("file1",
                   "Upload your csv or xlsx file:"),
  shiny::textOutput("filename"),
  DT::dataTableOutput("datatable")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$filename <- shiny::renderText({
    shiny::req(is.character(input$file1$datapath))
    paste0("The name of the uploaded file is: ", input$file1$name)
  })
  output$datatable <- DT::renderDataTable({
    shiny::req(is.character(input$file1$datapath))
    datatable <-
      readxl::read_excel(input$file1$datapath) %>% 
      DT::datatable(options = list(paging = F, searching = F))
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
