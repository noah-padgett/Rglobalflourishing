#' Transform Data to Wide Format
#'
#' Transforms data provided by COS to "wide" format for analysis in longitudinal structure/.
#'
#' @param data name of file to get data from
#' @param var variable name containing the wave/timepoint indicator (default = "WAVE")
#' @param ids variable name containing the ID variables separating individual cases
#' @param test (false)
#' @param ... other arguments
#' @returns a wide dataset
#' @examples {
#'   # TO-DO
#' }
#' @export
#' @description
#' TO-DO
gfs_data_to_wide <- function(data, var = "WAVE", ids = c("ID", "COUNTRY"), test = FALSE, ...){
  suppressMessages({
    suppressWarnings({

      if(test){
        set.seed(1)
        # construct wave 2 data
        ## OLD VERSION
        # {tmp.dat <- data %>%
        #   dplyr::select(
        #     dplyr::any_of(
        #       unique( c(ids, get_wave_flag(colnames(data),"W2")) )
        #     )
        #   )  %>%
        #   dplyr::rename_with(
        #     .cols = ! (dplyr::any_of(c(ids))),
        #     .fn = ~paste0(.x,"_W2", recycle0 = TRUE)
        #   )
        # tmp.dat <- tmp.dat %>% slice_sample(prop=0.75)
        # tmp.dat <- tmp.dat %>%
        #   group_by(COUNTRY) %>%
        #   nest() %>%
        #   mutate(
        #     data = purrr::map(data, \(x){
        #       x = x %>%
        #         dplyr::mutate(
        #           across(!(any_of(ids) | contains("DOI")), \(x){
        #             ux <- unique(x)
        #             for(i in 1:length(x)){
        #             	pi0 <- rbinom(1,1,0.25)
        #             	if(pi0 == 1){
        #             		x[i] <- sample(ux, 1)
        #             	}
        #             }
        #             x
        #           })
        #         )
        #       x
        #     })
        #
        #   ) %>%
        #   unnest(c(data))
        #
        # # update column names for wave 1 variables
        # data <- data %>%
        #   dplyr::select(
        #     dplyr::any_of(
        #       unique( c(ids, get_wave_flag(colnames(data),"W1")) )
        #     )
        #   ) %>%
        #   dplyr::rename_with(
        #     .cols = ! (dplyr::any_of(ids)),
        #     .fn = ~paste0(.x,"_W1", recycle0 = TRUE)
        #   )
        #
        # df.wide <- left_join(data, tmp.dat)
        # }
        # reformat to "long" for saving and internal testing
        df.w1 <- data %>%
          filter(WAVE == 1) %>%
          select(all_of(get_wave_flag(colnames(data), "W1")))
        df.w2 <- data %>%
          filter(WAVE == 2) %>%
          select(all_of(get_wave_flag(colnames(data), "W2")))
        # The following forces missingness - attrition by country
        {
          df.w2 <- df.w2 %>%
            group_by(COUNTRY) %>%
            slice_sample(prop = 0.75) %>%
            ungroup()
        }
        colnames(df.w1) <- paste0(colnames(df.w1), "_W1")
        colnames(df.w2) <- paste0(colnames(df.w2), "_W2")
        df.wide <- left_join(df.w1, df.w2, by = c("ID_W1" = "ID_W2", "COUNTRY_W1" = "COUNTRY_W2"))

        colnames(df.wide)[str_detect(colnames(df.wide), "ANNUAL_WEIGHT")] <- str_sub(
          colnames(df.wide)[str_detect(colnames(df.wide), "ANNUAL_WEIGHT")],
          start = 1, end = -4
        )
        colnames(df.wide)[colnames(df.wide) %in% c("ID_W1", "COUNTRY_W1", "STRATA_W1", "PSU_W1")] <- str_sub(
          colnames(df.wide)[colnames(df.wide) %in% c("ID_W1", "COUNTRY_W1", "STRATA_W1", "PSU_W1")],
          start = 1, end = -4
        )

      } else{

      Nwaves <- length(unique(data[,var,drop=TRUE]))
      wavevalues <- unique(data[,var,drop=TRUE])

      # wave 1 data
      df.wide <- data %>%
        dplyr::filter( !!sym(var) == wavevalues[1]) %>%
        dplyr::select(
          dplyr::any_of(
            unique( c(ids, get_wave_flag(colnames(data),"W1")) )
          )
        ) %>%
        dplyr::rename_with(
          .cols = ! (dplyr::any_of(c(ids))),
          .fn = ~paste0(.x,"_W1", recycle0 = TRUE)
        )

        # append additional waves with the tag "_W"
        i=2
        for(i in 2:Nwaves){
          tmp.data <- data %>%
            dplyr::filter( !!sym(var)  == wavevalues[i]) %>%
            dplyr::select(
              dplyr::any_of(
                unique( c(ids, get_wave_flag(colnames(data), paste0("W",i))) )
              )
            ) %>%
            dplyr::rename_with(
              .cols = ! (dplyr::any_of(c(ids))),
              .fn = ~paste0(.x,paste0("_W",i), recycle0 = TRUE)
            )
          df.wide <- left_join(df.wide, tmp.data, by = { ids })
        }
      }
    })
  })
  df.wide
}
