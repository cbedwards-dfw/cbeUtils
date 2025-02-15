#' Look for breaks in sequences of time blocks
#'
#' Developed to help work with fishing regulation data. `consolidate_time_blocks` takes any number of rows, with a column of start and end dates, and consolidates all contiguous periods of dates into a single one (and so each row of the output represents a period with one or more days break with the last one). Moreover, this function uses the {clipr} package to use the system clipboard. So you can copy a block of dates from excel (like in the screenshot) and then run the function with no arguments to have it pull in the copied cells and run the calculations.
#'
#' @param .data Dataframe. Optional; if not provided, instead use system clipboard (assumes it contains a copy of a 2-column excel block)
#' @param start_col Name of column containing start dates. Defaults to "start". Unused if `.data` is not provided.
#' @param end_col Name of column containing end dates. Defaults to "start". Unused if `.data` is not provided.
#'
#' @return Tibble with start and end date of each non-contiguous period, and length in days
#' @export
#'
#' @examples
#'
#' library(tidyr)
#' df = tribble(
#' ~ start, ~end_date,
#' "2024-8-1", "2024-8-5",
#' "2024-8-6", "2024-8-10",
#' "2024-8-16", "2024-8-30"
#' )
#' df |> consolidate_time_blocks(end_col = "end_date") #column name for start column is the default, "start"
consolidate_time_blocks <- function(.data = NULL,
                                    start_col = "start", ## if provided with a dataframe, looks for a column with label matching start_col to represent the first day of a period.
                                    end_col = "end" ## if provided with a dataframe, looks for a column with label matching end_col to represent the final day of a periodman
){
  if(is.null(.data)){
    cli::cli_alert_info("No data input, instead assuming system clipboard is a `copy` of two columns with start and then end dates.")
    .data = clipr::read_clip_tbl()
    names(.data) = c("start", "end")
  }

  ## add handling of slashes to dashes
  ##

  cli::cli_alert_warning("If dates are in ambiguous format, making best guess. Check years of output!")
  .data[[start_col]] = anytime::anydate(.data[[start_col]])
  .data[[end_col]] = anytime::anydate(.data[[end_col]])

  .data <- .data |>
    dplyr::mutate( "{start_col}" := lubridate::as_date(.data[[start_col]]),
                   "{end_col}" := lubridate::as_date(.data[[end_col]] )) |>
    dplyr::mutate(daily = purrr:::pmap(list(x = .data[[start_col]], y = .data[[end_col]]), function(x, y){seq(lubridate::ymd(x), lubridate::ymd(y), by = "day")})
    ) |>
    tidyr::unnest(.data$daily) |>
    dplyr::arrange(.data$daily)
  if(any(duplicated(.data$daily))){
    cli::cli_alert_danger("One or more days of overlap present when consolidating!")
  }
  .data |>
    dplyr::mutate(block = cumsum(c(TRUE, diff(.data$daily) > 1))) |>
    dplyr::group_by(.data$block) |>
    dplyr::mutate(blockind = dplyr::row_number()) |>
    dplyr::filter(blockind %in% range(.data$blockind)) |>
    dplyr::select("daily", "block", "blockind") |>
    dplyr::summarize(start = .data$daily[1],
                     end = .data$daily[2]) |>
    dplyr::ungroup() |>
    dplyr::mutate(length = .data$end - .data$start + 1) |>
    dplyr::select(-"block")
}
