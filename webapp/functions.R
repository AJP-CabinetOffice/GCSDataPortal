checkEmailAddressIsValid <- function(email_to_check, debug) {

  message("Email address submitted = ", email_to_check)

  result <-
    stringr::str_detect(email_to_check, pattern = "^.{1,}@.{1,}$")

  message("Did the email address pass the test? ", result)

  if (debug){}

  return(result)
}

checkAllColumnsArePresent <- function(df, col_config, debug) {

  message("Checking all columns are present...")

  columns_in_data <- colnames(df)

  columns_expected <- col_config$col_id

  names(columns_expected) <- columns_expected

  columns_present <-
    columns_expected  %>%
    purrr::map_df(.f <- function(.x) {
      .x %in% columns_in_data
    }) %>%
    tidyr::pivot_longer(dplyr::everything())

  result <-
    columns_present %>%
    dplyr::summarise(overall_result = prod(value)) %>%
    dplyr::pull(overall_result) %>%
    as.logical()

  message("Result of check = ", result)

  if (debug) {
    func_name <- sys.call()[1]
    message(func_name, "\tName of check = ", func_name)
    message(func_name, "\tResult of check = ", result)
    message(func_name,
            "\tCol names in submission = ",
            paste(columns_in_data, collapse = ", "))
    message(func_name,
            "\tExpected col names = ",
            paste(columns_expected, collapse = ", "))
    str <-  c()
    for (i in 1:nrow(columns_present)) {
      str_new <-
        paste(columns_present[1][[1]][i], columns_present[2][[1]][i], collapse = " = ")
      str <- append(str, str_new)
    }
    message(func_name,
            "\tPresence of each column =\n\t\t",
            paste(str, collapse = "\n\t\t"))
  }

  return(result)

}

checkNoExtraCols <- function(df, col_config) {

  message("Checking that there are no extra columns...")

  columns_in_data <- colnames(df)

  columns_expected <- col_config$col_id

  names(columns_in_data) <- columns_in_data

  result <-
    columns_in_data  %>%
    purrr::map_df(.f <- function(.x) {
      .x %in% columns_expected
    }) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::summarise(overall_result = prod(value)) %>%
    dplyr::pull(overall_result) %>%
    as.logical()

  message("Result of check = ", result)

  return(result)
}

checkAllColumnsContents <- function(df,
                                    col_config,
                                    debug) {

  message("Making a dataframe to check the contents of columns...")

  df_intermediate <-
    df

  col_order <-
    tibble::tibble(data_col_names = colnames(df_intermediate)) %>%
    dplyr::mutate(col_order_in_data = dplyr::row_number())

  col_config_reordered <-
    col_config %>%
    dplyr::left_join(col_order, by = c("col_id" = "data_col_names")) %>%
    dplyr::arrange(col_order_in_data)

  col_names <-
    col_config_reordered$col_id

  col_mode <-
    col_config_reordered$completion_mode

  regex_checks <-
    col_config_reordered$allowed_regex

  ## These next two are only used for numeric columns
  range_min <-
    col_config_reordered$numeric_range_min

  range_max <-
    col_config_reordered$numeric_range_max

  for (i in 1:length(regex_checks)) {
    ## Check that the qa check to be performed is taking place on a column
    ## that was actually submitted.
    if(col_names[i] %in% colnames(df)){
      # Numeric ranges are handled differently.
      # Numeric ranges are marked as "continuous_number" in the config file; handle them first by checking if it is in the correct range.
      # If it isn't a "continuous_number" then use the provided regex to check instead.
      if (col_mode[i] == "continuous_number") {
        df_intermediate <-
          df_intermediate %>%
          dplyr::mutate(
            ## Try to see if the number is between two values.
            ## If a warning occurs it is likely because the raw value cannot be coerced to a number, so it fails the test.
            "check_passed_{col_names[i]}_numeric" := dplyr::case_when(
              stringr::str_detect(!!rlang::sym(col_names[i]), pattern = "^\\*\\*$") ~ T,
              is.na(suppressWarnings(as.numeric(!!rlang::sym(col_names[i])))) ~ F,
              T ~ suppressWarnings(dplyr::between(as.numeric(!!rlang::sym(col_names[i])), as.numeric(range_min[i]), as.numeric(range_max[i])))
            )
          )
      } else {
        df_intermediate <-
          df_intermediate %>%
          dplyr::mutate(
            "check_passed_{col_names[i]}_regex" := dplyr::case_when(
              is.na(!!rlang::sym(col_names[i])) ~ F,
              T ~ stringr::str_detect(!!rlang::sym(col_names[i]), pattern = unlist(regex_checks[i]))
            )
          )
      }
    }
  }

  df_result = df_intermediate

  if (debug) {
    message("Columns in provided data = ", colnames(df))
    assign("df_result", df_result, envir = .GlobalEnv)
  }

  return(df_result)
}

didAllColumnsContentsPass <- function(df,
                                      col_config,
                                      col_check_prefix) {

  message("Checking if all column contents passed...")

  col_names <-
    col_config$col_id

  passed_checks <-
    df %>%
    dplyr::select(dplyr::starts_with(col_check_prefix)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ as.logical(prod(.x)))) %>%
    tidyr::pivot_longer(dplyr::everything())

  passed_checks <-
    passed_checks %>%
    ## If there is an NA value, then assume that the test failed. The na value
    ## is because a test has not been specified for that column.
    dplyr::mutate(value = dplyr::case_when(is.na(value) ~ F,
                                           T ~ value))

  passed_checks <-
    passed_checks %>%
    dplyr::summarise(check_passed_ALL = as.logical(prod(value))) %>%
    dplyr::pull(check_passed_ALL)

  message("Result of check = ", passed_checks)

  return(passed_checks)
}

generateUserMessageColumnContents <-
  function(columns_contents_passed) {
    if (columns_contents_passed) {
      message = NULL
    } else {
      message = "Invalid values were detected in your submission. The table below shows the data that you submitted. Entries coloured yellow were detected as invalid."
    }
    return(message)
  }

makeDiscreteChoicesAllowedValues <- function(col_config) {
  valid_item_codes <-
    col_config$options %>%
    purrr::map2(
      .y = col_config$completion_mode,
      .f = function(.x, .y) {
        if (.y == "discrete_choices") {
          item_labels = unlist(stringr::str_split(string = .x, pattern = ";"))

          extra_list_items = c("Other", "Unknown")

          `Item Label` <- c(item_labels, extra_list_items)

          ## Generate item codes to match the labels
          item_codes <- 10:(10 + length(item_labels) - 1)

          extra_item_codes <- c(99, "**")

          final_item_codes <- c(item_codes, extra_item_codes)
        }

      }
    )

  names(valid_item_codes) <- col_config$col_id
  return(valid_item_codes)
}

makeRegexFromCharacterVector <- function(char_vec) {
  if (is.character(char_vec)) {
    starting_body <- stringr::str_c(char_vec, collapse = "$|^")

    regex_with_start_and_end <- paste0("^", starting_body, "$")

    regex_replace_stars <-
      regex_with_start_and_end %>%
      stringr::str_replace_all(pattern = stringr::fixed("*"),
                               replacement = "\\*")

    regex_final <- regex_replace_stars
  } else {
    regex_final <- NULL
  }

  return(regex_final)
}

makeRegexFromAllowedValues <- function(allowed_codes) {
  allowed_regexes <-
    allowed_codes %>%
    purrr::map(makeRegexFromCharacterVector)
}

detectNilReturn <- function(df, debug) {

  message("Checking if it is a nil return...")

  ## Start by checking that the ORG column exists.
  result <-
    if (!is.null(df$ORG)) {
      ## Don't accept files with more than one row.
      if (nrow(df) == 1) {
        df_is_na <-
          df %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), ~ is.na(.))) %>%
          dplyr::select(-ORG) %>%
          tidyr::pivot_longer(dplyr::everything())

        result <-
          df_is_na %>%
          dplyr::summarise(overall_result = prod(value)) %>%
          dplyr::pull(overall_result) %>%
          as.logical()
      } else {
        F
      }

    } else {
      F
    }

  if (debug) {
    func_name <- sys.call()[1]
    message(func_name, "\t\tName of check = ", func_name)
    message(func_name, "\t\tNumber of rows in dataframe = ", nrow(df))
  }

  message("Result of check = ", result)

  return(result)
}

checkORGColumn <- function(df,
                           col_config,
                           debug = F){

  message("Checking if the ORG column is populated correctly...")

  regex_check <-
    col_config %>%
    dplyr::filter(
      col_id == "ORG"
    ) %>%
    dplyr::pull(allowed_regex)

  df_result <-
    df %>%
    dplyr::mutate(check_passed_ORG_regex = stringr::str_detect(ORG, pattern = regex_check))

  result <-
    df_result %>%
    dplyr::summarise(overall_result = as.logical(prod(check_passed_ORG_regex))) %>%
    dplyr::pull(overall_result)

  if(debug){
    message("regex check = ", regex_check)
    message("result dataframe = ", df_result)
  }

  message("Result of check = ", result)

  return(result)

}
