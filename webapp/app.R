library(magrittr)

# Load functions.
source("functions.R")

## Set up AWS credentials and variables

# env_vcap_services <- Sys.getenv("VCAP_SERVICES") %>% rjson::fromJSON()
# 
# Sys.setenv("AWS_ACCESS_KEY_ID" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_access_key_id,
#            "AWS_SECRET_ACCESS_KEY" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_secret_access_key,
#            "AWS_DEFAULT_REGION" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_region)

# s3_bucket = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$bucket_name

object_log = "rgcs_people_continuous/logs/submission_log.csv"

## This is used in functions that check the col_names.
col_check_prefix <- "check_passed_"

# Read in the configuration file.
col_config <- 
  readr::read_csv(
    "2022-05-05 Column configuration OFFICIAL - Sheet 1.csv",
    col_types = readr::cols(.default = "c")
  ) %>% 
  dplyr::filter(status == "Included")

# For use by quality checks
allowed_colnames <- 
  col_config$col_id

# Define UI
ui <- shiny::fluidPage(
  shiny::column(1),
  shiny::column(10,
    shiny::br(),
    shiny::br(),
    shiny::titlePanel("GCS Data Portal"),
    shiny::br(),
    shiny::HTML("This data portal is for use in the GCS Data Audit."),
    shiny::br(),
    shiny::fileInput("file1",
                     "Upload your file:"),
    shiny::br(),
    shiny::uiOutput("panel_feedback"),
    shiny::br(),
    DT::dataTableOutput("datatable")
  ),
  shiny::column(1)
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
    
    if(input$file1$type == "text/csv"){
      data <- tryCatch(
        readr::read_csv(input$file1$datapath),
        error = function(e){return(F)}) 
    } else {
      data <- F
    }
    
    return(data)
  })
  
  didDataReadReactive <- shiny::reactive({
    if(tibble::is_tibble(readData())){
      T
    } else {
      F
    }
  })
  
  checkAllColumnsContentsReactive <- shiny::reactive({  
    checkAllColumnsContents(readData(), col_config)
    })
  
  didAllColumnsContentsPassReactive <- shiny::reactive({
    df <- checkAllColumnsContentsReactive()
    didAllColumnsContentsPass(df, col_config, col_check_prefix)
  })
  
  wasSubmissionAcceptedReactive <- shiny::reactive({
    results_cols <- 
      c(didAllColumnsContentsPassReactive())
    
    overall_result <- as.logical(prod(results_cols))
    return(overall_result)
  })
  
  output$panel_feedback <- shiny::renderUI({
    result_data_readin <- didDataReadReactive()
    
    result_cols <- if(result_data_readin){
      wasSubmissionAcceptedReactive()
    } else {
      F
    }
    
    
    # writeSubmissionToS3()
    if(result_cols){
      shiny::HTML("Success! Thank you for your submission.")
    } else if(!result_data_readin){
      shiny::HTML("Your submission was not accepted.<br>All submissions must be in CSV format. Please resubmit in the correct format.")
    } else if(!result_cols){
      shiny::HTML("Your submission was not accepted.<br>Items that require your attention are higlighted in yellow.<br>Please correct the highlighted data and resubmit.")
      
    }
  })
  
  output$datatable <- DT::renderDataTable({
    shiny::req(didDataReadReactive())
    result_cols <- wasSubmissionAcceptedReactive()
    shiny::req(!result_cols)
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
  
  passesColNameCheck <- shiny::reactive({
    
    result_cols <- 
      readData() %>% 
      colnames() %>% 
      purrr::map(.f = function(colname){
        res = F
        if(colname %in% allowed_colnames){
          res = T
        }
        return(res)
      })
    
    overall_result = prod(as.numeric(result_cols))
    
    return(overall_result)
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
  
  writeSubmissionToS3 <- shiny::reactive({
    shiny::req(input$file1$datapath)
    result_cols <- wasSubmissionAcceptedReactive()
    
    sysdatetime <- Sys.time() %>% 
      format("%Y-%m-%dT%H:%M:%S_%Z")
    
    folder_qa <- if(result_cols){
      "qa_passed"
    } else {
      "qa_failed"
    }
    
    file_uniqueness <- input$file1$datapath %>% 
      stringr::str_split(pattern = "/") %>% 
      unlist() %>% 
      tail(n = 2) %>% 
      head(n = 1)
    
    folder_name <- paste(sysdatetime, file_uniqueness)
    
    full_filename_raw <- 
      paste(folder_qa, folder_name, "RAW OFFICIAL-SENSITIVE.csv", sep = "/")
  
    aws.s3::put_object(
      input$file1$datapath,
      full_filename_raw,
      s3_bucket
    ) 
    
    analysis_result <- checkAllColumnsContentsReactive()
    
    full_filename_qa <- 
      paste(folder_qa, folder_name, "QA OFFICIAL-SENSITIVE.csv", sep = "/")
    
    aws.s3::s3write_using(
      analysis_result,
      readr::write_excel_csv,
      object = full_filename_qa,
      bucket = s3_bucket
    )
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)