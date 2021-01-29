
#' Reads 'mcI' files
#'
#' @param mci_file An mcI file
#'
#' @return
#' @export
#'
#' @examples
#' file <- "ftp://dfoftp.ocean.dal.ca/pub/dfo/BSRTO/2018-2019/mcI/18082923.mcI"
#' read_mci(file)
#'
#' @import readr
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import lubridate
#'
read_mci <- function(mci_file) {
  if (length(mci_file) != 1) {
    return(bind_rows(lapply(mci_file, read_mci)))
  }

  mci_tbl <- read_csv(
    mci_file,
    skip = 3,
    col_names = c(
      "temperature", "conductivity", "pressure",
      "oxygen", "salinity", "sound_speed",
      "date", "time"
    ),
    col_types = cols(
      date = col_date("%d %b %Y"),
      time = col_time(),
      .default = col_double()
    )
  ) %>%
    mutate(
      date_time = as_datetime(date, tz = "UTC") + time
    ) %>%
    select(-date, -time)

  mci_tbl
}
