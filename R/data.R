.data_get_date_range <- function(data, round=NULL) {
    if(myClim:::.common_is_agg_format(data)) {
        start_date_values <- purrr::map_int(data$localities, ~ min(.x$datetime))
        end_date_values <- purrr::map_int(data$localities, ~ max(.x$datetime))
        table <- data.frame(start_date=myClim:::.common_as_utc_posixct(start_date_values),
                            end_date=myClim:::.common_as_utc_posixct(end_date_values))
    } else {
        table <- myClim::mc_info_logger(data)
    }
    start_date <- min(table$start_date)
    end_date <- max(table$end_date)
    if(!is.null(round)) {
        start_date <- lubridate::floor_date(start_date, unit=round)
        end_date <- lubridate::floor_date(end_date, unit=round)
    }
    return(c(start_date, end_date))
}

.data_get_sensors <- function(data) {
    info <- myClim::mc_info(data)
    return(unique(info$sensor_name))
}

.data_get_locality_logger_type <- function(data) {
    if(myClim:::.common_is_agg_format(data)) {
        result <- as.list(names(data$localities))
        names(result) <- names(data$localities)
        return(as.environment(result))
    }
    info <- myClim::mc_info_logger(data)
    info <- dplyr::select(info, .data$locality_id, .data$logger_type)
    info <- dplyr::distinct(info)
    items <- purrr::map2(info$locality_id, info$logger_type, ~ c(.x, .y))
    names(items) <- paste0(info$locality_id, ": ", info$logger_type)
    return(as.environment(items))
}

.data_get_filtered_data_table <- function(data) {
    if(myClim:::.common_is_agg_format(data)) {
        return(data.frame(locality_id=names(data$localities)))
    }
    result <- myClim::mc_info_logger(data)
    return(dplyr::select(result, "locality_id", "index", "serial_number", "logger_type"))
}

.data_edit_states <- function(data, edit_table, selected_states_table) {
    browser()
    row_index <- edit_table$row[[1]]
    column_index <- edit_table$col[[1]]
    changed_row <- selected_states_table[row_index, ]
    states_table <- dplyr::setdiff(myClim::mc_info_states(data), changed_row)
    changed_row[column_index] <- changed_row$value[1, 1]
    states_table <- dplyr::union_all(states_table, changed_row)
    return(myClim::mc_states_update(data, states_table))
}