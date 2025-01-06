#' Transform Data to Wide Format
#'
#' Transforms data provided by COS to "wide" format for analysis in longitudinal structure/.
#'
#' @param data name of file to get data from
#' @param var variable name containing the wave/timepoint indicator (default = "WAVE")
#' @param ids variable name containing the ID variables separating individual cases
#' @param ... other arguments
#' @returns a wide dataset
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gsf_data_to_wide <- function(data, var = "WAVE", ids = "ID", ...){
  suppressMessages({
    suppressWarnings({
      Nwaves <- length(unique(data[,var,drop=TRUE]))
      wavevalues <- unique(data[,var,drop=TRUE])
      # wave 1 data
      df.wide <- data %>%
        dplyr::filter( (!!as.name(var)) == wavevalues[1])
      colnames(df.wide) <- paste0(colnames(data),"_W1")
      colnames(df.wide)[stringr::str_detect(colnames(df.wide), paste0(ids, "_W"))] <- ids
      # append additional waves with the tag "_W"
      for(i in 2:Nwaves){
        tmp.data <- data %>%
          dplyr::filter( (!!as.name(var)) == wavevalues[i])
        colnames(tmp.data) <- paste0(colnames(data),"_W", i)
        colnames(tmp.data)[stringr::str_detect(colnames(tmp.data), paste0(ids, "_W"))] <- ids
        df.wide <- left_join(df.wide, tmp.data, by = ids)
      }
    })
  })
  df.wide
}
