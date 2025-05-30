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
    info <- dplyr::select(info, "locality_id", "logger_type")
    info <- dplyr::distinct(info)
    items <- purrr::map2(info$locality_id, info$logger_type, ~ c(.x, .y))
    names(items) <- paste0(info$locality_id, ": ", info$logger_type)
    return(as.environment(items))
}

.data_get_filtered_data_table <- function(selection_table) {
    result <- dplyr::select(selection_table, "locality_id", "logger_name")
    return(dplyr::distinct(result))
}

.data_edit_states <- function(data, changed_table, new_values) {
    result_env <- new.env()
    result_env$data <- data

    group_function <- function(.x, key_table) {
        sensor <- result_env$data$localities[[key_table$locality_id]]$loggers[[key_table$logger_name]]$sensors[[key_table$sensor_name]]
        .x$edit <- TRUE
        states_table <- sensor$states
        columns <- colnames(states_table)
        states_table <- dplyr::left_join(states_table, .x, by=columns)
        states_table$edit[is.na(states_table$edit)] <- FALSE
        for(name in names(new_values)) {
            states_table[states_table$edit, name] <- new_values[[name]]
        }
        states_table <- dplyr::select(states_table, -dplyr::any_of("edit"))
        result_env$data$localities[[key_table$locality_id]]$loggers[[key_table$logger_name]]$sensors[[key_table$sensor_name]]$states <- states_table
    }

    changed_table <- dplyr::group_by(changed_table, .data$locality_id, .data$logger_name, .data$sensor_name)
    dplyr::group_walk(changed_table, group_function)

    return(result_env$data)
}

.data_delete_states <- function(data, delete_table) {
    is_agg <- myClim:::.common_is_agg_format(data)

    result_env <- new.env()
    result_env$data <- data

    delete_function <- function(locality_id, logger_name, sensor_name, tag,
                                start, end, value) {
        delete_tag <- tag
        delete_start <- start
        delete_end <- end
        delete_value <- value
        if(is_agg) {
            states_table <- result_env$data$localities[[locality_id]]$sensors[[sensor_name]]$states
        } else {
            states_table <- result_env$data$localities[[locality_id]]$loggers[[logger_name]]$sensors[[sensor_name]]$states
        }
        states_table <- dplyr::filter(states_table, (.data$tag != delete_tag) |
                                      (.data$start != delete_start) |
                                      (.data$end != delete_end) |
                                      (.data$value != delete_value))
        if(is_agg) {
            result_env$data$localities[[locality_id]]$sensors[[sensor_name]]$states <- states_table
        } else {
            result_env$data$localities[[locality_id]]$loggers[[logger_name]]$sensors[[sensor_name]]$states <- states_table
        }
    }

    purrr::pwalk(delete_table, delete_function)
    return(result_env$data)
}

.data_filter_by_selection_table <- function(data, selection_table) {
    is_agg <- myClim:::.common_is_agg_format(data)
    
    result <- data

    logger_function <- function(param_locality_id, param_logger_name) {
        logger_table <- dplyr::filter(selection_table, (.data$locality_id == param_locality_id) &
                                                       (.data$logger_name == param_logger_name))
        logger <- data$localities[[param_locality_id]]$loggers[[param_logger_name]]
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
        locality$loggers <- purrr::map2(param_locality_id, unique(locality_table$logger_name), logger_function)
        names(locality$loggers) <- purrr::map_chr(locality$loggers, ~ .x$metadata@name)
        return(locality)
    }

    locality_ids <- unique(selection_table$locality_id)
    result$localities <- purrr::map(locality_ids, locality_function)
    names(result$localities) <- locality_ids
    
    return(result)
}

.data_get_selection_table <- function(data, locality, logger_type_value) {
    is_agg <- myClim:::.common_is_agg_format(data)

    if(is_agg) {
        sensors <- names(data$localities[[locality]]$sensors)
        return(data.frame(locality_id=locality,
                          logger_name=NA_character_,
                          sensor_name=sensors))
    }
    
    logger_function <- function(locality_id, logger_name) {
        logger <- data$localities[[locality_id]]$loggers[[logger_name]]
        return(
             list(locality_id=locality_id,
                  logger_name=logger_name,
                  sensor_name=names(logger$sensors)))
    }

    filtered_data <- myClim::mc_filter(data, localities=locality)
    loggers_table <- myClim::mc_info_logger(filtered_data)
    loggers_table <- dplyr::filter(loggers_table, .data$logger_type == logger_type_value)
    loggers_table <- dplyr::select(loggers_table, "locality_id", "logger_name")

    result <- purrr::pmap_dfr(loggers_table, logger_function)
    return(result)
}

.data_compareNA <- function(v1, v2) 
{
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

.data_selection_table_add_range <- function(data, selection_table) {
    is_agg <- myClim:::.common_is_agg_format(data)
    
    agg_locality_function <- function(locality_id) {
        int <- lubridate::interval(start=min(data$localities[[locality_id]]$datetime),
                                   end=max(data$localities[[locality_id]]$datetime))
        result <- list(locality_id=locality_id,
                       int=int)
    }

    logger_function <- function(locality_id, logger_name) {
        int <- lubridate::interval(start=min(data$localities[[locality_id]]$loggers[[logger_name]]$datetime),
                                   end=max(data$localities[[locality_id]]$loggers[[logger_name]]$datetime))
        result <- list(locality_id=locality_id,
                       logger_name=logger_name,
                       int=int)
    }
    
    if(is_agg) {
        localities <- unique(selection_table$locality_id)
        ranges_table <- purrr::map_dfr(localities, agg_locality_function)
        by <- "locality_id"
    } else {
        logger_table <- dplyr::distinct(dplyr::select(selection_table, "locality_id", "logger_name"))
        ranges_table <- purrr::map2_dfr(logger_table$locality_id, logger_table$logger_name,logger_function)
        by <- c("locality_id", "logger_name")
    }

    result <- dplyr::left_join(selection_table, ranges_table, by=by)
    return(result)
}

.data_get_dataview_table <- function(data, selection_table, intervals) {
    start_datetime <- NULL
    end_datetime <- NULL
    if(!is.null(intervals)) {
        start_datetime <- min(lubridate::int_start(intervals))
        end_datetime <- max(lubridate::int_end(intervals))
    }
    result <- .data_get_wide_table(data, selection_table, start_datetime, end_datetime)
    is_agg <- myClim:::.common_is_agg_format(data)
    if(!is.null(intervals) && length(intervals) > 1) {
        conditions <- purrr::map(intervals, ~ lubridate::`%within%`(result$datetime, .x))
        datetime_condition <- purrr::reduce(conditions, `|`)
        result <- dplyr::filter(result, datetime_condition)
    }
    result$datetime <- format(result$datetime, "%Y-%m-%d %H:%M:%S")
    return(result)
}

.data_get_wide_table <- function(data, selection_table, start_datetime=NULL, end_datetime=NULL,
                                 start_index=NULL, end_index=NULL) {
    if(is.null(selection_table)) {
        filter_data <- data
    } else {
        filter_data <- .data_filter_by_selection_table(data, selection_table)
    }
    if(!is.null(start_datetime) && !is.null(end_datetime)) {
        crop_data <- myClim::mc_prep_crop(filter_data, start=start_datetime, end=end_datetime)
    } else if(!is.null(start_index) && !is.null(end_index)) {
        crop_data <- .data_crop_by_index_range(filter_data, c(start_index, end_index))
    } else {
        crop_data <- filter_data
    }
    result <- myClim::mc_reshape_wide(crop_data, show_logger_name=TRUE)
    if(length(crop_data$localities) == 1 && length(crop_data$localities[[1]]$loggers) == 1) {
        logger <- crop_data$localities[[1]]$loggers[[1]]
        if(!any(is.na(logger$metadata@raw_index))) {
            result <- tibble::add_column(result, index = logger$metadata@raw_index, .before = 1)
        }
    }
    return(result)
}

.data_get_dataview_table_index <- function(data, start_index, end_index) {
    result <- .data_get_wide_table(data, NULL, start_index=start_index, end_index=end_index)
    result$datetime <- format(result$datetime, "%Y-%m-%d %H:%M:%S")
    return(result)
}

.data_get_all_tags <- function(data) {
    result <- unique(myClim::mc_info_states(data)$tag)
    result <- sort(result)
    return(result)
}

.data_crop_by_index_range <- function(data, index_range) {
    if(is.null(index_range)) {
        return(data)
    }

    logger_function <- function(logger) {
        table <- myClim:::.prep_get_logger_table_with_index(logger)
        table <- dplyr::filter(table, (.data$raw_index__ >= index_range[1]) &
                               (.data$raw_index__ <= index_range[2]))
        logger <- myClim:::.prep_load_logger_values_from_table(logger, table)
        return(logger)
    }

    locality_function <- function(locality) {
        locality$loggers <- purrr::map(locality$loggers, logger_function)
        return(locality)
    }

    data$localities <- purrr::map(data$localities, locality_function)
    return(data)
}

.data_get_max_index <- function(data) {
    is_agg <- myClim:::.common_is_agg_format(data)

    locality_function <- function(locality) {
        if(is_agg) {
            return(length(locality$datetime))
        }
        result_values <- purrr::map_int(locality$loggers, ~ length(.x$datetime))
        return(max(result_values))
    }

    result_values <- purrr::map_int(data$localities, locality_function)
    return(max(result_values))
}