#'  @import tidyverse, rvest, openxlsx, scholar, stringi, xml2, lubridate
#'  @title data_getting
#'  @param df A dataframe that has group's informations
#'  @details This function get information from GrupLac
#' @export

getting_data <- function(df, researchers) {

  if(missing(researchers)){
    researchers = 0
  }

  source("R/data_getting.R")

  source("R/data_cleaning.R")

  source("R/functions.R")

  eval(parse("R/functions.R", encoding = "UTF-8"))

  df <- df |>
    mutate(grupo = str_to_upper(grupo),
           grupo = stri_trans_general(str = grupo,
                                      id = "Latin-ASCII"))

  grupo_df <- data_getting_ucla(df)
  grupo <- data_cleaning_ucla(df, grupo_df, researchers)
  return(grupo)
}
