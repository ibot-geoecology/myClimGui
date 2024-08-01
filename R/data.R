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
    row_index <- edit_table$row[[1]]
    column_index <- edit_table$col[[1]]
    changed_row <- selected_states_table[row_index, ]
    states_table <- myClim::mc_info_states(data)
    condition <- .data_compareNA(states_table$locality_id, changed_row$locality_id) &
                 .data_compareNA(states_table$logger_index, changed_row$logger_index) &
                 .data_compareNA(states_table$sensor_name, changed_row$sensor_name) &
                 .data_compareNA(states_table$tag, changed_row$tag) &
                 .data_compareNA(states_table$start, changed_row$start) &
                 .data_compareNA(states_table$end, changed_row$end) &
                 .data_compareNA(states_table$value, changed_row$value)
    states_table[condition, column_index] <- edit_table$value
    return(myClim::mc_states_update(data, states_table))
}

.data_filter_by_selection_table <- function(data, selection_table) {
    is_agg <- myClim:::.common_is_agg_format(data)
    
    result <- data

    logger_function <- function(param_locality_id, param_logger_index) {
        logger_table <- dplyr::filter(selection_table, (.data$locality_id == param_locality_id) & (.data$logger_index == param_logger_index))
        logger <- data$localities[[param_locality_id]]$loggers[[param_logger_index]]
        logger$sensors <- logger$sensors[logger_table$sensor_name]
        return(logger)
    }

    locality_function <- function(param_locality_id) {
        locality_table <- dplyr::filter(selection_table, .data$locality_id == param_locality_id)
        locality <- data$localities[[param_locality_id]]
        if(is_agg) {
            locality$sensors <- locality$sensors[locality_table$sensor_name]
            return(locality)
        }
        locality$loggers <- purrr::map2(param_locality_id, unique(locality_table$logger_index), logger_function)
        return(locality)
    }

    locality_ids <- unique(selection_table$locality_id)
    result$localities <- purrr::map(locality_ids, locality_function)
    names(result$localities) <- locality_ids
    
    return(result)
}

.data_get_selection_table <- function(data) {
    is_agg <- myClim:::.common_is_agg_format(data)
    
    logger_function <- function(locality_id, logger_index, logger) {
        step <- as.integer(logger$clean_info@step)

        return(
             list(locality_id=locality_id,
                  logger_index=logger_index,
                  sensor_name=names(logger$sensors)))
    }

    locality_function <- function(locality) {
        if(is_agg) {
            return(
                 list(locality_id=locality$metadata@locality_id,
                      sensor_name=names(locality$sensors)))
        }
        purrr::pmap_dfr(list(
                            locality_id = locality$metadata@locality_id,
                            logger_index = seq_along(locality$loggers),
                            logger = locality$loggers),
                            logger_function)
    }

    result <- purrr::map_dfr(data$localities, locality_function)
    return(result)
}

.data_compareNA <- function(v1, v2) 
{
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}