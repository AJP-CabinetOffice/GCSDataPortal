library(magrittr)

# Define variables
bucket = "ashley-poole-rgcs"
object_log = "rgcs_people_continuous/logs/submission_log.csv"

# Load requisite data

# Define UI
ui <- shiny::fluidPage(
  # Application title
  shiny::titlePanel("RGCS Data Portal"),
  shiny::fileInput("file1",
                   "Upload your file:"),
  shiny::textOutput("filename"),
  DT::dataTableOutput("datatable")
)

# Define server logic
server <- function(input, output) {
  output$datatable <- DT::renderDataTable({
    shiny::req(is.character(input$file1$datapath))
    
    data <-
      readxl::read_excel(input$file1$datapath) %>%
      dplyr::mutate(
        passes_qa_organisation = T,
        passes_qa_teamname = T,
        passes_qa_roletitle = T,
        fte_clean = as.numeric(fte),
        passes_qa_fte = dplyr::case_when(is.na(as.numeric(fte)) ~ F,
                                         T ~ T),
        passes_qa_grade = dplyr::case_when(
          grade %in% c(
            "AA",
            "AO",
            "EO",
            "HEO",
            "SEO",
            "G7",
            "G6",
            "SCS1",
            "SCS2",
            "SCS3",
            "Other"
          ) ~ T,
          T ~ F,
        ),
        passes_qa_salary = dplyr::case_when(
          is.na(as.numeric(salary)) ~ F,
          T ~ T
        ),
        passes_qa_discipline = dplyr::case_when(
          discipline %in% c(
            "External affairs",
            "Marketing",
            "Internal communication",
            "Strategic communication",
            "Media"
          ) ~ T,
          T ~ F,
        ),
        passes_qa_job_location = T,
        passes_qa_vacant = T,
        passes_qa_contract_type = T
      )
    
    qa_col_indices <-
      match(
        c(
          "passes_qa_fte",
          "fte_clean", 
          "passes_qa_organisation",
          "passes_qa_teamname",
          "passes_qa_roletitle",
          "passes_qa_grade",
          "passes_qa_salary",
          "passes_qa_discipline",
          "passes_qa_job_location",
          "passes_qa_vacant",
          "passes_qa_contract_type"
          ), 
            colnames(data))
    
    datatable <-
      data %>%
      DT::datatable(options = list(
        paging = F,
        searching = F,
        columnDefs = list(list(
          visible = F, targets = qa_col_indices
        ))
      )) %>%
      DT::formatStyle(
        c(
          "organisation",
          "teamname",
          "roletitle",
          "fte",
          "grade",
          "salary",
          "discipline",
          "job_location",
          "vacant",
          "contract_type"
        ),
        c("passes_qa_organisation",
          "passes_qa_teamname",
          "passes_qa_roletitle",
          "passes_qa_fte",
          "passes_qa_grade",
          "passes_qa_salary",
          "passes_qa_discipline",
          "passes_qa_job_location",
          "passes_qa_vacant",
          "passes_qa_contract_type"
        ),
        backgroundColor = DT::styleEqual(c(T, F), c("lightblue", "yellow")))
    
    #UpdateSubmissionLog()
    
    return(datatable)
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