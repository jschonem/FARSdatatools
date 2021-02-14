utils::globalVariables(c('STATE','MONTH','year','n'))

#' Read Data from File
#'
#' This function checks for the existence of a file having a specified name.
#' If the file exists, its contents are read and returned by the function.
#'
#' @param filename A character string giving the name of the file to be read.
#'    An error is generated if no file with the specified name exists in the
#'    working directory.
#'
#' @return A data.frame object containing the contents of the specified file.
#'
#' @note To ensure a filename passed to this function is properly constructed
#'    use the function in this package called make_filename.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")
#' fars_read(make_filename(2013))
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make Filename
#'
#' Generates the name of a file containing accident data for a specified year.
#'
#' @param year A value indicating the year of the desired data. Values of class
#'    integer or numeric may be safely assigned to this variable. Values of class
#'    character are also safe if coercible to integer, e.g. '2013'. If the
#'    value is not coercible to integer, invalid results may occur due to
#'    introduction of NAs by coercion or an error may be generated.
#'
#' @return  A character string holding the desired filename. As a side effect,
#'    this function also prints out the character string.
#'
#' @note Data only exist for the years 2013, 2014, & 2015. However, if another
#'    year is chosen, the filename is still returned with no error generation.
#'
#' @examples make_filename(2013)
#' @examples make_filename("2013")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Extract Month Data
#'
#' This function extracts month data from a file of accident records for
#'    specified years. These data are the months in which the accidents recorded
#'    in the data file occurred.
#'
#' @param years A value or vector/list of values indicating the year(s) of the
#'    data desired. Values of class integer or numeric may be passed safely to
#'    this function. Values of class character are also safe if coercible to
#'    integer, e.g. '2013'. Data only exist for the years 2013, 2014, & 2015. An
#'    error will be generated if a year other than one of these is passed to the
#'    function.
#'
#' @return This function returns a list of data.frame objects. The members of
#'     the returned list contain the month data corresponding to the year
#'     value(s) passed to the function.
#'
#' @importFrom  dplyr mutate
#' @importFrom  dplyr select
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{fars_read_years(2015)
#' fars_read_years(c('2013','2014'))
#' fars_read_years(list(2014,2015)) }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize a Year's Data as Monthly Totals
#'
#' This function extracts data from a file of accident records for specified
#'    years and summarizes the specified year(s) of accident data in a table
#'    of monthly accident totals.
#'
#' @inheritParams fars_read_years
#'
#'
#' @return This function returns a data.frame object whose first column is the
#'    sequence of integers from 1 to 12 (enumerating the 12 months of the year).
#'    Each subsequent column represents a summary of the desired years in terms
#'    of monthly totals.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{fars_summarize_years(2015)
#' fars_summarize_years(c('2013','2014'))
#' fars_summarize_years(list(2014,2015)) }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}


#' Plot Location Data on State Map
#'
#' This function extracts data from a file of accident records for a specified
#'    year and displays a simple map of the desired state with a dot plotted at
#'    the location of every accident recorded in the data for that state during
#'    the specified year.
#'
#' @inheritParams make_filename
#' @param state.num A unique identifying number for each state (FIPS code). An
#'    error will be generated if a number is assigned to this parameter which
#'    does not exist within the data for the specified year. In the case that no
#'    accident data exists for the specified state and year, the message "no
#'    accidents to plot" will be generated. Values of class integer or numeric
#'    may be safely assigned to this parameter. Values of class character
#'    are also safe if coercible to integer, e.g. "48". If value is not
#'    coercible to integer, invalid results may occur due to introduction of NAs
#'    by coercion or an error may be generated.
#'
#' @return Returns NULL.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples \dontrun{
#' fars_map_state(48, 2013)
#' fars_map_state(6, "2015")
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


