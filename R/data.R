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
    info <- myClim::mc_info_logger(data)
    info <- dplyr::select(info, .data$locality_id, .data$logger_type)
    info <- dplyr::distinct(info)
    info <- dplyr::arrange(info, .data$locality_id, .data$logger_type)
    return(paste0(info$locality_id, ": ", info$logger_type))
}
