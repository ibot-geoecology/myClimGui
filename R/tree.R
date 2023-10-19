.tree_get_list <- function(data) {
    is_agg <- myClim:::.common_is_agg_format(data)

    logger_function <- function (logger) {
        purrr::map(logger$sensors, ~ "")
    }

    locality_function <- function (locality) {
        if(is_agg)
        {
            result <- purrr::map(locality$sensors, ~ "")
            return(result)
        }
        result <- purrr::map(locality$loggers, logger_function)
        names(result) <- purrr::imap(locality$loggers, ~ if(is.na(.x$metadata@serial_number)) as.character(.y) else stringr::str_glue("{.y}_{.x$metadata@serial_number}"))
        return(result)
    }

    result <- purrr::map(data$localities, locality_function)
    return(result)
}