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

.data_get_filtered_data_table <- function(selection_table) {
    if("logger_index" %in% colnames(selection_table)) {
        result <- dplyr::select(selection_table, "locality_id", "logger_index")
    } else {
        result <- dplyr::select(selection_table, "locality_id")
    }
    return(dplyr::distinct(result))
}

.data_edit_states <- function(data, changed_table, new_values) {
    changed_table$edit <- TRUE
    states_table <- myClim::mc_info_states(data)
    columns <- colnames(states_table)
    states_table <- dplyr::left_join(states_table, changed_table, by=columns)
    states_table$edit[is.na(states_table$edit)] <- FALSE
    for(name in names(new_values)) {
        states_table[states_table$edit, name] <- new_values[[name]]
    }
    return(myClim::mc_states_update(data, dplyr::select(states_table, columns)))
}

.data_delete_states <- function(data, delete_table) {
    delete_table$delete <- TRUE
    states_table <- myClim::mc_info_states(data)
    columns <- colnames(states_table)
    states_table <- dplyr::left_join(states_table, delete_table, by=columns)
    states_table$delete[is.na(states_table$delete)] <- FALSE
    states_table <- dplyr::filter(states_table, !.data$delete)
    return(myClim::mc_states_update(data, dplyr::select(states_table, columns)))
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

.data_get_selection_table <- function(data, locality, logger_type_value) {
    is_agg <- myClim:::.common_is_agg_format(data)

    if(is_agg) {
        sensors <- names(date$localities[[locality]]$sensors)
        return(data.frame(locality_id=locality, sensor_name=sensors))
    }
    
    logger_function <- function(locality_id, index) {
        logger <- data$localities[[locality_id]]$loggers[[index]]
        return(
             list(locality_id=locality_id,
                  logger_index=index,
                  sensor_name=names(logger$sensors)))
    }

    loggers_table <- myClim::mc_info_logger(data)
    loggers_table <- dplyr::filter(loggers_table, .data$locality_id == locality & .data$logger_type == logger_type_value)
    loggers_table <- dplyr::select(loggers_table, "locality_id", "index")

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

    logger_function <- function(locality_id, logger_index) {
        int <- lubridate::interval(start=min(data$localities[[locality_id]]$loggers[[logger_index]]$datetime),
                                   end=max(data$localities[[locality_id]]$loggers[[logger_index]]$datetime))
        result <- list(locality_id=locality_id,
                       logger_index=logger_index,
                       int=int)
    }
    
    if(is_agg) {
        localities <- unique(selection_table$locality_id)
        ranges_table <- purrr::map_dfr(localities, agg_locality_function)
        by <- "locality_id"
    } else {
        logger_table <- dplyr::distinct(dplyr::select(selection_table, "locality_id", "logger_index"))
        ranges_table <- purrr::map2_dfr(logger_table$locality_id, logger_table$logger_index,logger_function)
        by <- c("locality_id", "logger_index")
    }

    result <- dplyr::left_join(selection_table, ranges_table, by=by)
    return(result)
}

.data_get_dataview_table <- function(data, selection_table, crop_range) {
    filter_data <- .data_filter_by_selection_table(data, selection_table)
    crop_data <- myClim::mc_prep_crop(filter_data, crop_range[[1]], crop_range[[2]])
    result <- myClim::mc_reshape_wide(crop_data)
    result <- .data_rename_dataview_columns(data, selection_table, result)
    result$datetime <- format(result$datetime, "%Y-%m-%d %H:%M:%S")
    return(result)
}

.data_rename_dataview_columns <- function(data, selection_table, wide_table){
    if(myClim:::.common_is_agg_format(data)) {
        return(wide_table)
    }
    name_env <- new.env()
    name_env$columns <- colnames(wide_table)
    selection_table <- dplyr::group_by(selection_table, .data$locality_id)

    rename_function <- function(old_column_preffix, new_column_preffix) {
        old_column_preffix <- stringr::fixed(old_column_preffix)
        condition <- stringr::str_starts(name_env$columns, old_column_preffix)
        name_env$columns[condition] <- stringr::str_replace(name_env$columns[condition], old_column_preffix, new_column_preffix)
    }

    group_function <- function(group_table, .y) {
        indexes <- sort(unique(group_table$logger_index))
        wrong_indexes <- seq_along(indexes)
        old_column_preffix <- as.character(stringr::str_glue("{.y$locality_id[[1]]}_{wrong_indexes}_"))
        new_column_preffix <- stringr::str_glue("{.y$locality_id[[1]]}_{indexes}_")
        purrr::walk2(old_column_preffix, new_column_preffix, rename_function)
    }
    dplyr::group_walk(selection_table, group_function)
    colnames(wide_table) <- name_env$columns
    return(wide_table)
}