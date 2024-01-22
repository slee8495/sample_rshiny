library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(lubridate)



clean_data <- function(raw_data_as400) {

  raw_data_as400[3, ] -> data_info
  raw_data_as400[c(-1:-7, -9:-10), ] -> cleaned_data



  colnames(cleaned_data) <- cleaned_data[1, ]

  cleaned_data <- cleaned_data[-1, ]

  cleaned_data <- cleaned_data %>%
    janitor::clean_names() %>%
    type_convert() %>%
    mutate(across(contains("date"), ~janitor::excel_numeric_to_date(.x))) %>%
    mutate(across(contains("date"), ~as.Date(.x, format="%m/%d/%Y")),
                  week_number = lubridate::isoweek(order_date),
                  date_acknowledgement_calc = ifelse(is.na(date_acknowledge), as.character(Sys.Date()), as.character(date_acknowledge)),
                  date_acknowledgement_calc = as.Date(date_acknowledgement_calc, format="%Y-%m-%d"),
                  days_to_acknowledge = ifelse(!is.na(date_acknowledgement_calc) & !is.na(order_date), as.numeric(date_acknowledgement_calc - order_date), "-"),
                  fail = ifelse(days_to_acknowledge > 2, "Yes", "No")) %>%

    plyr::mutate(`On Time` = ifelse(fail == "Yes", "Not on Time", "On Time")) %>%
    dplyr::select(-fail) %>%

    dplyr::relocate(week_number, .after = order_date) %>%
    dplyr::mutate(across(-contains("date"), ~ifelse(is.na(.x), "-", .x))) %>%
    dplyr::rename("Profile owner" = profile_owner,
                  "Profile name" = profile_name,
                  "Enter by" = enter_by,
                  "Enter by name" = enter_by_name,
                  Leader = leader,
                  "Leader name" = leader_name,
                  Loc = loc,
                  Order = order,
                  CustomerName = customer_name,
                  Customer = customer,
                  OrderDate = order_date,
                  Week = week_number,
                  DeliveryDate = delivery_date,
                  ShipDate = ship_date,
                  "Detail hold code" = detail_hold_code,
                  "Order Acknowledgement Flag" = order_ack,
                  "Date acknowledge" = date_acknowledge,
                  "Date Acknowledgement Calc." = date_acknowledgement_calc,
                  "Days to acknowledge" = days_to_acknowledge) %>%
    dplyr::filter(!is.na(OrderDate) & OrderDate != 0)

  return(cleaned_data)
}


data_info <- function() {
  data_info <- raw_data_as400[3, ]
  data_info <- data_info %>%
    dplyr::select(1)
  return(data_info)
}

# Default Data
raw_data_as400 <- read_xlsx("sample_data.xlsx")
actual_data <- clean_data(raw_data_as400)




anonymize_column <- function(column) {
  unique_values <- unique(column)
  
  if(is.numeric(column)) {
    # For numeric values
    random_values <- sample(10000:99999, length(unique_values), replace = TRUE)
  } else if(is.character(column)) {
    # For text values, generating random strings of the same length as the original strings
    random_values <- replicate(length(unique_values), paste0(sample(LETTERS, 5, replace = TRUE), collapse = ""))
  } else if(is.factor(column)) {
    # Convert factors to characters and then anonymize
    column <- as.character(column)
    unique_values <- unique(column)
    random_values <- replicate(length(unique_values), paste0(sample(LETTERS, 5, replace = TRUE), collapse = ""))
  } else if(is.Date(column) || inherits(column, 'POSIXct')) {
    # For date values
    random_values <- sample(seq(as.Date('2023-01-01'), as.Date('2024-1-19'), by="day"), length(unique_values), replace = TRUE)
  } else {
    stop("Unsupported column type")
  }
  
  mapping <- setNames(random_values, unique_values)
  return(mapping[column])
}


actual_data %>% select(-OrderDate, -DeliveryDate, -ShipDate, -"Date Acknowledgement Calc.", -"Date acknowledge", -Week, 
                       -"Order Acknowledgement Flag", -`Days to acknowledge`, -`On Time`) -> actual_data_2


# Assuming your data is in a data frame called df
df_anonymized <- actual_data_2 %>%
  mutate(across(everything(), anonymize_column)) %>% 
  dplyr::mutate(Order = ifelse(is.na(Order), 11111, Order))


actual_data %>% select(OrderDate, DeliveryDate, ShipDate, "Date Acknowledgement Calc.", "Date acknowledge", Week, 
                       "Order Acknowledgement Flag", `Days to acknowledge`, `On Time`) -> actual_data_3


cbind(actual_data_3, df_anonymized) -> cleaned_default_data
