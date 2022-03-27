library(magrittr)

# Load functions.
source("functions.R")

# Define variables
bucket = "ashley-poole-rgcs"
object_log = "rgcs_people_continuous/logs/submission_log.csv"
## This is used in functions that check the col_names.
col_check_prefix <- "check_passed_"

# Read in the configuration file.
col_config <- 
  readr::read_csv(
    "column_config.csv",
    col_types = readr::cols(.default = "c")
  )

# For use by quality checks
allowed_colnames <- 
  readr::read_csv("template_csvutf8.csv") %>% 
  colnames()

# Define UI
ui <- shiny::fluidPage(
  shiny::tabsetPanel(
    shiny::tabPanel(
      shiny::titlePanel("RGCS Data Portal"),
      shiny::fileInput("file1",
                       "Upload your file:"),

      shiny::uiOutput("panel_feedback"),
      # shiny::uiOutput("panel_filetype_error"),
      # shiny::uiOutput("panel_columnname_error"),
      DT::dataTableOutput("datatable")
    ),
    shiny::tabPanel(
      shiny::titlePanel("About")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  success <- shiny::reactiveVal(T)
  
  checkColumnContents <- function(
    col_name,
    col_type,
    allowed_length_per_entry = NULL
  ){
    
    
    full_result <- list(
      result = result,
      message = message
    )
    
    return(full_result)
  }
  
  readData <- shiny::reactive({
    shiny::req(input$file1$datapath)
    v <- try(readxl::read_excel(input$file1$datapath))
    if("try-error" %in% class(v)){
      v <- try(read.csv(input$file1$datapath) %>% tibble::tibble())
    }
    return(v)
  })
  
  checkAllColumnsContentsReactive <- shiny::reactive({  
    checkAllColumnsContents(readData(), col_config)
    })
  
  didAllColumnsContentsPassReactive <- shiny::reactive({
    df <- checkAllColumnsContentsReactive()
    didAllColumnsContentsPass(df, col_config, col_check_prefix)
  })
  
  wasSubmissionAcceptedReactive <- shiny::reactive({
    results <- 
      c(didAllColumnsContentsPassReactive())

    overall_result <- as.logical(prod(results))
  })
  
  output$panel_feedback <- shiny::renderUI({
    result <- wasSubmissionAcceptedReactive()
    if(result){
      shiny::HTML("Success! Thank you for your submission.")
    } else{
      shiny::HTML("Your submission was not accepted.<br>Items that require your attention are higlighted in yellow.<br>Please correct the highlighted data and resubmit.")
    }
  })
  
  output$datatable <- DT::renderDataTable({
    result <- wasSubmissionAcceptedReactive()
    shiny::req(!result)
    df_tested <- checkAllColumnsContentsReactive()

    col_names_raw_data <- 
      colnames(df_tested)[colnames(df_tested) %in% col_config$col_id]
    
    col_names_qa <-
      colnames(df_tested)[colnames(df_tested) %>% stringr::str_detect(pattern = paste0("^", col_check_prefix))]
    
    qa_col_indices <- 
      match(
        col_names_qa,
        colnames(df_tested)
      )
    
    df_tested %>% 
      DT::datatable(options = list(
        paging = F,
        searching = F,
        ## Make the qa columns invisible.
        columnDefs = list(list(
          visible = F, targets = qa_col_indices
        ))
      )) %>% 
      DT::formatStyle(
        col_names_raw_data,
        col_names_qa,
        backgroundColor = DT::styleEqual(c(T, F), c("white", "yellow")))
  })
  
  output$panel_filetype_error <- shiny::renderUI({
    ## If the data cannot be read in but a file has been uploaded, then print the message.
    if("try-error" %in% class(readData()) & !is.null(input$file1$name)){
      success(F)
      shiny::HTML("Error: The filetype was not valid. You must submit a file of type .csv, .xlsx or .xls")
    } else {
      success(T)
      NULL
      }
  })
  
  passesColNameCheck <- shiny::reactive({
    
    result <- 
      readData() %>% 
      colnames() %>% 
      purrr::map(.f = function(colname){
        res = F
        if(colname %in% allowed_colnames){
          res = T
        }
        return(res)
      })
    
    overall_result = prod(as.numeric(result))
    
    return(overall_result)
  })

  output$panel_columnname_error <- shiny::renderUI({
    ## If the data has been read in but the columns are incorrect, then print the message.
    if("tbl_df" %in% class(readData()) & !passesColNameCheck()){
      success(F)
      shiny::HTML(
        "Error: The column names were invalid. Ensure the column names are the same as those provided in the template. Additional columns are not allowed."
        )
    } else {
      success(T)
      NULL
      }
  })
  
  UpdateSubmissionLog <- shiny::reactive({
    entity_code = "TEST_TEST"
    server_datetime <- Sys.time()
    new_filename <- paste0(
      format(server_datetime, "%Y-%m-%d_%H-%M-%S_%Z "),
      entity_code,
      " OFF-SEN",
      ".xlsx"
    )
    
    submission_log <-
      aws.s3::s3read_using(readr::read_csv,
                           object = object_log,
                           bucket = bucket)
    
    new_submission_log <-
      submission_log %>%
      tibble::add_row(
        datetime = server_datetime,
        raw_filename = input$file1$name,
        new_filename = new_filename
      )
    
    aws.s3::s3write_using(new_submission_log,
                          readr::write_csv,
                          object = object_log,
                          bucket = bucket)
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)