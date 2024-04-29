.data_get_date_range <- function(data) {
    info <- myClim::mc_info(data)
    start_date <- lubridate::floor_date(min(info$start_date), unit="day")
    end_date <- lubridate::ceiling_date(max(info$end_date)+1, unit="day")
    return(c(start_date, end_date))
}

.data_get_sensors <- function(data) {
    info <- myClim::mc_info(data)
    return(unique(info$sensor_name))
}

.data_get_locality_logger_type <- function(data) {
    if(myClim:::.common_is_agg_format(data)) {
        result <- new.env()
        purrr::walk(names(data$localities), ~ result[[.x]] <- .x)
        return(result)
    }
    info <- myClim::mc_info_logger(data)
    info <- dplyr::select(info, .data$locality_id, .data$logger_type)
    info <- dplyr::distinct(info)
    items <- purrr::map2(info$locality_id, info$logger_type, ~ c(.x, .y))
    names(items) <- paste0(info$locality_id, ": ", info$logger_type)
    return(as.environment(items))
}

