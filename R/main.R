#'  @import tidyverse, rvest, here, openxlsx, ,scholar, stringi, xml2
#'  @title data_getting
#'  @param df A dataframe that has group's informations
#'  @details This function get information from GrupLac
#' @export

getting_data <- function(df) {
  library(here)

  source(here("R","scripts",
              "data_getting.R"))

  source(here("R","scripts",
              "data_cleaning.R"))

  source(here("R","scripts",
              "functions.R"))



  eval(parse(here("R","scripts","functions.R"), encoding = "UTF-8"))

  grupo_df <- data_getting_ucla(df)
  grupo <- data_cleaning_ucla(df, grupo_df)
  return(grupo)
}
