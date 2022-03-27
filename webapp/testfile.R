library(magrittr)

test_data <- 
  readr::read_csv(
    "test_correct.csv",
    col_types = readr::cols(.default = "c"),
    na = character()
    )

checkAllColumnsContents <- function(
  df,
  col_config
){
  
  df_intermediate <- 
    df
  
  col_names <- 
    col_config$col_id
  
  col_mode <- 
    col_config$completion_mode
    
  regex_checks <- 
    col_config$allowed_regex
  
  ## These next two are only used for numeric columns
  range_min <- 
    col_config$numeric_range_min
  
  range_max <- 
    col_config$numeric_range_max
  
  for(i in 1:length(regex_checks)){
    # Numeric ranges are handled differently.
    # Numeric ranges are marked as "continuous_number" in the config file; handle them first by checking if it is in the correct range.
    # If it isn't a "continuous_number" then use the provided regex to check instead.
    if(col_mode[i] == "continuous_number"){
      
      df_intermediate <-
        df_intermediate %>%
        dplyr::mutate(
          ## Try to see if the number is between two values. 
          ## If a warning occurs it is likely because the raw value cannot be coerced to a number, so it fails the test.
          "check_passed_{col_names[i]}_numeric" := dplyr::case_when(
            is.na(as.numeric(!!rlang::sym(col_names[i]))) ~ F,
            T ~ dplyr::between(!!rlang::sym(col_names[i]), range_min[i], range_max[i])
          )
        )
    } else {
      df_intermediate <-
        df_intermediate %>%
        dplyr::mutate(
          "check_passed_{col_names[i]}_regex" := stringr::str_detect(!!rlang::sym(col_names[i]), pattern = unlist(regex_checks[i]))
        )
    }
  }
  
  df_result = df_intermediate
  
  return(df_result)
}

didAllColumnsContentsPass <- function(
  df,
  col_config,
  col_check_prefix = col_check_prefix
  ){
  
  col_names <- 
    col_config$col_id
  
  passed_checks <- 
    df %>% 
    dplyr::select(
      dplyr::starts_with(col_check_prefix)
    ) %>% 
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ as.logical(prod(.x)))
    ) %>%
    tidyr::pivot_longer(dplyr::everything()) %>% 
    dplyr::summarise(check_passed_ALL = as.logical(prod(value))) %>% 
    dplyr::pull(check_passed_ALL)
}

generateUserMessageColumnContents <- function(columns_contents_passed){
  if(columns_contents_passed){
    message = NULL
  } else {
    message = "Invalid values were detected in your submission. The table below shows the data that you submitted. Entries coloured yellow were detected as invalid."
  }
  return(message)
}

col_check_prefix <- 
  "check_passed_"

df_tested <- 
  test_data %>% 
  checkAllColumnsContents(col_config = col_config)

did_all_tests_pass <- 
  df_tested %>% 
  didAllColumnsContentsPass(col_config = col_config)

user_message_col_contents <- 
  did_all_tests_pass %>% 
  generateUserMessageColumnContents()

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
#### Make regex from string ####

convertAllowedValuesToRegex <- function(vec_allowed_values){
  ## Use the character "*" as a wildcard that will allow all values.
  ## Otherwise, separate out the list of allowed values and turn it into a regex.
  if(vec_allowed_values == "*"){
    regex <- ".*"
  } else {
  
    allowed_split <- 
      vec_allowed_values %>%  
      stringr::str_split(pattern = ";") %>% 
      unlist()
    
    for(i in 1:length(allowed_split)){
      
      additional_clause <- if(allowed_split[i] == "**"){
        "\\*\\*"
      } else {
        paste0("^", allowed_split[i], "$")
      } 
      
      regex <- if(i == 1){
        regex <- additional_clause
      } else {
        paste(regex, additional_clause, sep = "|")
      }
    }
  }
  return(regex)
}

allowed_raw <- 
  col_config$allowed_values %>% 
  purrr::map(
    .f = convertAllowedValuesToRegex
  )

convertAllowedValuesToRegex("10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;99;**
")
