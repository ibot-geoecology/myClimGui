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
