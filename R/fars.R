#' read in a data file as a tibble named "data"
#'
#' this is a simple function that firstly find the datafile's name in the current content, if there is not such the file,
#' the function would return "file dose not exist", if there is the file, the function would read in the data and make it
#' a tibble table.
#'
#' @param filename A character string giving the data file's physical name
#'
#' @return this function returns a tibble data with column number and row number and a dataset named "data", it also
#' returns "file \code{filename} does not exist".
#'
#'@importFrom dplyr tbl_df
#'@importFrom readr read_csv
#'
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
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



#' use year to name the file
#'
#' This is a simple function that you can input the year and it will return the file name depended on the year
#'
#' @param year A number which is a year
#'
#' @return This function returns the filename as "accident_year.csv.bz2"
#'
#' @examples
#' \dontrun{make_filename(2013)}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' remain only variable month and year of the choosen dataset
#'
#' This is a simple function that remain 2 variables of the accident data file of the year you choose, month and year.
#' When it meets an error, it will return "invaild year: \code{year}"
#'
#' @param years a vector that contains the year numbers
#'
#' @return
#' The function will return new tibbles with 2 columns(monthe and year), or a warning message:invaild year: \code{year}
#' and "NULL"
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2014,2015))}
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



#' Summarize the count of accidents of each year, each month
#'
#' This is a fuction that you can enter the year, it will tell you in this year, how many accidents occured in each month
#'
#' @param years A vector that you can put you numeric years here
#'
#' @return
#' This function will return a summarized table that contains the year you inputed and the accident number of each month
#' in this year.
#'
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows group_by summarize %>%
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' map the distribution of a state in a year
#'
#' This is a function that you can choose a state by the state number and a year, it will return a plot that describe
#' the distribution of the accident in the state of the choosen year.
#'
#' @param state.num The state number in 1 to 56, representing each state
#' @param year The numeric representing the year
#'
#' @return
#' This function will return a map describing the distribution of the accident
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(45,2013)}
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
