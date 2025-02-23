library(magrittr)

# Load functions.
source("functions.R")

debug = F
development_configuration = F

## Set up AWS credentials and variables
if(development_configuration){
  auth <- jsonlite::read_json("~/Codes/Analysis_RGCS/s3_auth_mmd.json")

  Sys.setenv("AWS_ACCESS_KEY_ID" = auth$aws_access_key_id,
             "AWS_SECRET_ACCESS_KEY" = auth$aws_secret_access_key,
             "AWS_DEFAULT_REGION" = auth$aws_region)

  s3_bucket = auth$bucket_name
} else {
  env_vcap_services <- Sys.getenv("VCAP_SERVICES") %>% rjson::fromJSON()

  Sys.setenv("AWS_ACCESS_KEY_ID" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_access_key_id,
             "AWS_SECRET_ACCESS_KEY" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_secret_access_key,
             "AWS_DEFAULT_REGION" = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$aws_region)

  s3_bucket = env_vcap_services$`aws-s3-bucket`[[1]]$credentials$bucket_name
}

## This is used in functions that check the col_names.
col_check_prefix <- "check_passed_"

# Read in the configuration file.
col_config_raw <-
  readr::read_csv(
    "2022-05-25 Column configuration OFFICIAL - Sheet 1.csv",
    col_types = readr::cols(.default = "c")
  ) %>%
  ## Remove withdrawn columns
  dplyr::filter(status == "Included")

## Generate regex for the discrete columns.
discrete_regexes <-
  makeDiscreteChoicesAllowedValues(col_config_raw) %>%
  makeRegexFromAllowedValues()

org_regex <-
  readxl::read_excel(
    "List of Organisations v1.23 - GCS Data Audit 2022 OFFICIAL.xlsx",
    sheet = "Organisations Table",
    col_types = "text",
    .name_repair = janitor::make_clean_names
  ) %>%
  dplyr::pull(organisation_code_gcs_data_audit) %>%
  c("99", "**") %>%
  makeRegexFromCharacterVector()

lad_other_values <-
  readr::read_csv(
    "2022-05-23_15-41-49_BST Other values LAD lookup OFFICIAL.csv",
    col_types = "c",
    name_repair = janitor::make_clean_names
  )

lad_regex <-
  readr::read_csv(
    "2022-05-24_11-25-05_BST Postcode to LAD lookup OFFICIAL.csv",
    col_types = "c",
    col_select = 3,
    name_repair = janitor::make_clean_names
  ) %>%
  dplyr::distinct() %>%
  tidyr::drop_na() %>%
  dplyr::pull(lad_code_gcs_data_audit) %>%
  c(lad_other_values$lad_code_gcs_data_audit) %>%
  as.character() %>%
  makeRegexFromCharacterVector()

col_config <-
  col_config_raw %>%
  tibble::add_column(discrete_regexes) %>%
  dplyr::mutate(discrete_regexes = as.character(discrete_regexes)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    allowed_regex = dplyr::case_when(
      (completion_mode == "discrete_choices_large") & (col_id == "LAD") ~ lad_regex,
      completion_mode == "discrete_choices_large" ~ org_regex,
      discrete_regexes != "NULL" ~ discrete_regexes,
      !is.na(allowed_regex_initial) ~ allowed_regex_initial,
      T ~ NA_character_
    )
  ) %>%
  dplyr::ungroup()

# For use by quality checks
allowed_colnames <-
  col_config$col_id

# Define UI
ui <- shiny::fluidPage(
  shiny::column(1),
  shiny::column(
    10,
    shiny::br(),
    shiny::br(),
    shiny::titlePanel("GCS Data Portal"),
    shiny::br(),
    shiny::HTML("This data portal is for use in the GCS Data Audit 2022."),
    shiny::br(),
    shiny::br(),
    shiny::textInput("email1", "Enter an email address - shared mailbox preferred"),
    shiny::uiOutput("panel_email"),
    shiny::br(),
    shiny::uiOutput("file_upload"),
    shiny::br(),
    shiny::uiOutput("panel_feedback"),
    shiny::br(),
    DT::dataTableOutput("datatable")
  ),
  shiny::column(1)
)

# Define server logic
server <- function(input, output) {

  checkEmailAddressIsValidReactive <- shiny::reactive({
    checkEmailAddressIsValid(input$email1, debug)
  })

  checkEmailAddressNotReshapingReactive <- shiny::reactive({
    checkEmailAddressNotReshaping(input$email1, debug)
  })

  output$file_upload <- shiny::renderUI({

    shiny::validate(
      shiny::need(
        checkEmailAddressIsValidReactive(),
        "Enter an email address to continue."
      ),
      shiny::need(
        checkEmailAddressNotReshapingReactive(),
        "You must use an email address from your organisation, not the reshaping GCS email."
      )
    )

    fileInput("file1",
              "Upload your file:")
  })


  readData <- shiny::reactive({
    shiny::req(input$file1$datapath)

    message("Reading uploaded data...")
    message("MIME type of file = ", input$file1$type)

    file_extension <-
      input$file1$name %>%
      stringr::str_split(pattern = "\\.") %>%
      unlist() %>%
      tail(n = 1)

    message("File extension = ", file_extension)

    message("File extension dtype = ", typeof(file_extension))

    if (input$file1$type == "text/csv" | file_extension %in% c("csv", "CSV", "Csv")) {
      data <- tryCatch(
        readr::read_csv(input$file1$datapath),
        error = function(e) {
          return(F)
        }
      )
    } else {
      data <- F
    }

    return(data)
  })

  didDataReadReactive <- shiny::reactive({
    if (tibble::is_tibble(readData())) {
      T
    } else {
      F
    }
  })

  detectNilReturnReactive <- shiny::reactive({
    detectNilReturn(readData(), debug)
  })

  checkNilReturnORGColumnReactive <- shiny::reactive({
    checkORGColumn(readData(), col_config, debug)
  })

  checkAllColumnsArePresentReactive <- shiny::reactive({
    checkAllColumnsArePresent(readData(), col_config, debug)
  })

  checkNoExtraColsReactive <- shiny::reactive({
    checkNoExtraCols(readData(), col_config)
  })

  checkAllColumnsContentsReactive <- shiny::reactive({
    checkAllColumnsContents(readData(), col_config, debug)
  })

  didAllColumnsContentsPassReactive <- shiny::reactive({
    df <- checkAllColumnsContentsReactive()
    didAllColumnsContentsPass(df, col_config, col_check_prefix)
  })

  wasSubmissionAcceptedReactive <- shiny::reactive({

    if (detectNilReturnReactive()) {
      results <-
        c(
          checkNilReturnORGColumnReactive()
        )
    } else {
      results <-
        c(
          checkAllColumnsArePresentReactive(),
          checkNoExtraColsReactive(),
          didAllColumnsContentsPassReactive()
        )
    }

    overall_result <- as.logical(prod(results))

    message("Was the submission accepted? ", overall_result)

    return(overall_result)

  })

  output$panel_feedback <- shiny::renderUI({
    result_data_readin <- didDataReadReactive()


    if(!result_data_readin){
      message("Data was not in the correct format.")
      return(shiny::HTML("Your submission was not accepted.<br>All submissions must be in CSV format.<br>Please resubmit in the correct format."))
    }

    if (debug) {
      message("Did data read in as a tibble? ", result_data_readin)
    }

    result_nil_return <- if (result_data_readin) {
      detectNilReturnReactive()
    } else {
      F
    }

    result_nil_return_valid_org <- if(result_nil_return){
      checkNilReturnORGColumnReactive()
    } else {
      F
    }

    if (debug) {
      message("Was it a nil return? ", result_nil_return)
      if(result_nil_return){
        (message("Was the ORG valid in the nil return?", result_nil_return_valid_org))
        }
    }

    result_all_cols_present <-
      if (result_data_readin & !result_nil_return) {
        checkAllColumnsArePresentReactive()
      } else {
        F
      }

    if (debug) {
      message("Were all expected columns present? ",
              result_all_cols_present)
    }

    result_no_extra_cols <-
      if (result_data_readin & !result_nil_return) {
        checkNoExtraColsReactive()
      } else {
        F
      }

    if (debug) {
      message("Were there no extra columns? ", result_no_extra_cols)
    }

    result_column_contents <-
      if (result_all_cols_present & result_no_extra_cols) {
        didAllColumnsContentsPassReactive()
      } else {
        F
      }

    if (debug) {
      message("Was the content of the cells acceptable? ",
              result_column_contents)
    }

    writeSubmissionToS3()

    if (result_data_readin &
        result_all_cols_present &
        result_no_extra_cols & result_column_contents) {
      shiny::HTML("Success!<br>Thank you for your submission.")
    } else if (result_data_readin & result_nil_return & result_nil_return_valid_org) {
      shiny::HTML("Success! Nil return received.<br>Thank you for your submission.")
    } else if (result_data_readin & result_nil_return & !result_nil_return_valid_org) {
        shiny::HTML("Your submission was not accepted.<br>The ORG column was not completed correctly.<br>Please resubmit using the code provided in the organisations list.")
    } else if (!result_data_readin) {
      shiny::HTML(
        "Your submission was not accepted.<br>All submissions must be in CSV format. Please resubmit in the correct format."
      )
    } else if (!result_all_cols_present) {
      shiny::HTML(
        "Your submission was not accepted.<br>Some data columns are missing.<br>Please add the missing data columns and resubmit."
      )
    } else if (!result_no_extra_cols) {
      shiny::HTML(
        "Your submission was not accepted.<br>Extra data columns were detected.<br>Please remove the extra columns and resubmit."
      )
    } else if (!result_column_contents) {
      shiny::HTML(
        "Your submission was not accepted.<br>Items that require your attention are higlighted in yellow.<br>Please correct the highlighted data and resubmit."
      )
    }
  })

  output$datatable <- DT::renderDataTable({
    shiny::req(didDataReadReactive())
    shiny::req(checkAllColumnsArePresentReactive())
    shiny::req(checkNoExtraColsReactive())
    shiny::req(!detectNilReturnReactive())

    result <- wasSubmissionAcceptedReactive()

    shiny::req(!result)

    df_tested <- checkAllColumnsContentsReactive()

    col_names_raw_data <-
      colnames(df_tested)[colnames(df_tested) %in% col_config$col_id]

    col_names_qa <-
      colnames(df_tested)[colnames(df_tested) %>% stringr::str_detect(pattern = paste0("^", col_check_prefix))]

    qa_col_indices <-
      match(col_names_qa,
            colnames(df_tested))

    df_tested %>%
      DT::datatable(options = list(
        paging = F,
        searching = F,
        ## Make the qa columns invisible.
        columnDefs = list(list(
          visible = F, targets = qa_col_indices
        ))
      )) %>%
      DT::formatStyle(col_names_raw_data,
                      col_names_qa,
                      backgroundColor = DT::styleEqual(c(T, F), c("white", "yellow")))
  })

  passesColNameCheck <- shiny::reactive({
    result <-
      readData() %>%
      colnames() %>%
      purrr::map(
        .f = function(colname) {
          res = F
          if (colname %in% allowed_colnames) {
            res = T
          }
          return(res)
        }
      )

    contents_result = prod(as.numeric(result))

    return(contents_result)
  })

  writeSubmissionToS3 <- shiny::reactive({
    shiny::req(input$file1$datapath)
    result <- wasSubmissionAcceptedReactive()

    message("Writing submission to S3...")

    sysdatetime <- Sys.time() %>%
      format("%Y-%m-%dT%H:%M:%S_%Z")

    folder_qa <- if (result) {
      "qa_passed"
    } else {
      "qa_failed"
    }

    message("Writing to folder ", folder_qa)

    file_uniqueness <- input$file1$datapath %>%
      stringr::str_split(pattern = "/") %>%
      unlist() %>%
      tail(n = 2) %>%
      head(n = 1)

    folder_name <- paste(sysdatetime, file_uniqueness)

    full_filename_raw <-
      paste(folder_qa, folder_name, "RAW OFFICIAL-SENSITIVE.csv", sep = "/")

    aws.s3::put_object(input$file1$datapath,
                       full_filename_raw,
                       s3_bucket)

    if(!detectNilReturnReactive()){
      analysis_result <- checkAllColumnsContentsReactive()

      full_filename_qa <-
        paste(folder_qa, folder_name, "QA OFFICIAL-SENSITIVE.csv", sep = "/")

      aws.s3::s3write_using(analysis_result,
                            readr::write_excel_csv,
                            object = full_filename_qa,
                            bucket = s3_bucket)
    }

    submitter_email <-
      input$email1 %>%
      as.data.frame()

    full_filename_email <-
      paste(folder_qa, folder_name, "CONTACT EMAIL OFFICIAL-SENSITIVE.csv", sep = "/")

    aws.s3::s3write_using(submitter_email,
                          readr::write_excel_csv,
                          object = full_filename_email,
                          bucket = s3_bucket)

  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
