library(lubridate)
library(stringi)
# source(here("scripts",
#             "functions.R"))

data_cleaning_ucla <- function(grupos, grupo_df, researchers) {

  grupo_main_cleaned <- data_cleaning_main(grupos, grupo_df)
  grupo_product_cleaned <- data_cleaning_product(grupo_df)
  grupo_researcher_cleaned <- data_cleaning_researcher(grupo_df, researchers)

  return(list(grupo_main_cleaned = grupo_main_cleaned,
              grupo_product_cleaned = grupo_product_cleaned,
              grupo_researcher_cleaned = grupo_researcher_cleaned))

}
