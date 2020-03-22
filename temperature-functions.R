library(tidyverse)

# Functions ----

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
#' temperature_input_table <- read_temperature_file("examples/temperature.txt")
#' temperature_input_table <- read_temperature_file("examples/temperature_30min.txt")
read_temperature_file <- function(file) {
  read_delim(file, "\t")
}

#' Title
#'
#' @param data
#' @param min_increase
#'
#' @return
#' @export
#'
#' @examples
#' temperature_input_table <- read_temperature_file("examples/temperature.txt")
#'
#' temperature_input_table %>% first_valid_timepoint()
first_valid_timepoint <- function(data, min_increase = 0.5) {
  # 'data' must contain columns 'temperature' and 'time'
  data %>%
    mutate(
      diff = c(0, diff(temperature)),
      increase = diff > min_increase) %>%
    subset(increase) %>%
    pull(time) %>% head(1)
}

#' Title
#'
#' @param data
#' @param ...
#' @param reset_time
#'
#' @return
#' @export
#'
#' @examples
#' temperature_input_table <- read_temperature_file("examples/temperature.txt")
#'
#' temperature_input_table %>% drop_pre_measurements()
drop_pre_measurements <- function(data, ..., reset_time = TRUE) {
  time_start <- data %>% first_valid_timepoint(...)

  data <- data %>%
    subset(time >= time_start)

  if (reset_time) {
    data <- data %>% reset_time()
  }

  data
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' temperature_input_table <- read_temperature_file("examples/temperature.txt")
#'
#' temperature_input_table %>% reset_time()
reset_time <- function(data) {
  data %>% mutate(time = time - min(time))
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' temperature_input_table <- read_temperature_file("examples/temperature.txt")
#'
#' temperature_input_table %>% drop_failed_measurements()
drop_failed_measurements <- function(data) {
    data %>%
        mutate(dT = c(diff(temperature), NA)) %>%
        subset(dT != 0) %>%
        select(-dT)
}
