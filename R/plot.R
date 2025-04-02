.plot_const_NEIGHBORHOOD_LENGTH <- 13

.plot_states <- function(shared, states_table) {
    groupped_table <- dplyr::group_by(states_table, .data$locality_id, .data$logger_name, .data$sensor_name)
    filter_data <- myClim::mc_filter(shared$data, localities = unique(states_table$locality_id))
    range <- .plot_states_get_range(filter_data, groupped_table)
    data_table <- .plot_states_get_data(filter_data, groupped_table, range)
    p <- ggplot2::ggplot(data=data_table, ggplot2::aes(x=.data$datetime, y=.data$value, group=.data$name)) +
         ggplot2::geom_line(ggplot2::aes(color=.data$name)) +
         ggplot2::theme(legend.position="bottom") +
         ggplot2::ylab("Values") +
         ggplot2::xlab("Date")
    for(rectangle in .plot_get_states_rectangles(data_table, states_table)) {
        p <- p + rectangle + ggplot2::theme(legend.position="bottom")
    }
    return(p)
}

.plot_states_get_range <- function(data, groupped_states) {
    is_agg <- myClim:::.common_is_agg_format(data)
    is_cleaned <- TRUE
    if(!is_agg) {
        is_cleaned <- myClim:::.prep_is_datetime_step_processed_in_object(data)
    }
    step <- NULL
    if(is_agg) {
        step <- data$metadata@step
    }
    group_function <- function(group_table, .y) {
        if(!is_agg && is_cleaned) {
            locality_id <- .y$locality_id[[1]]
            logger_name <- .y$logger_name[[1]]
            step <- data$localities[[locality_id]]$loggers[[logger_name]]$clean_info@step
        }
        if(is_cleaned) {
            time_diff <- lubridate::seconds(.plot_const_NEIGHBORHOOD_LENGTH * step)
        } else {
            time_diff <- lubridate::hours(3)
        }
        start <- min(group_table$start) - time_diff
        end <- max(group_table$end) + time_diff
        return(lubridate::interval(start, end))
    }
    intervals <- purrr::map_vec(dplyr::group_map(groupped_states, group_function), ~ .x)
    start <- min(lubridate::int_start(intervals))
    end <- max(lubridate::int_end(intervals))
    return(lubridate::interval(start, end))
}

.plot_states_get_data <- function(data, groupped_states, date_range) {
    cropped_data <- myClim::mc_prep_crop(data, start=lubridate::int_start(date_range),
                                         end=lubridate::int_end(date_range))
    is_agg <- myClim:::.common_is_agg_format(data)
    group_function <- function(group_table, .y) {
        locality_id <- .y$locality_id[[1]]
        sensor_name <- .y$sensor_name[[1]]
        locality <- cropped_data$localities[[locality_id]]
        if(is_agg) {
            series_name <- stringr::str_glue("{locality_id}: {sensor_name}")
            datetime <- locality$datetime
            values <- locality$sensors[[sensor_name]]$values
        } else {
            logger_name <- .y$logger_name[[1]]
            series_name <- stringr::str_glue("{locality_id} {logger_name}: {sensor_name}")
            datetime <- locality$loggers[[logger_name]]$datetime
            values <- locality$loggers[[logger_name]]$sensors[[sensor_name]]$values
        }
        result <- tibble::tibble(datetime=datetime,
                                 name=series_name,
                                 value=values)
    }
    tables <- dplyr::group_map(groupped_states, group_function)
    return(dplyr::bind_rows(tables))
}

.plot_get_states_rectangles <- function(data_table, states) {
    min_value <- min(data_table$value, na.rm=TRUE)
    max_value <- max(data_table$value, na.rm=TRUE)
    tags <- unique(states$tag)
    tags_table <- tibble::tibble(i = seq_along(tags),
                                 tag = tags)
    tag_height <- (max_value - min_value) / length(tags)
    tags_table$ymin <- (tags_table$i - 1) * tag_height + min_value
    tags_table$ymax <- tags_table$ymin + tag_height
    tags_table$color <- RColorBrewer::brewer.pal(8, "Set2")[1:length(tags)]
    rectangle_data_table <- dplyr::left_join(states, tags_table, by="tag")
    rectangle_data_table <- dplyr::select(rectangle_data_table, "tag", "start", "end", "ymin", "ymax")
    group_tags_table <- dplyr::group_by(rectangle_data_table, .data$tag)
    group_function <- function(group_table, .y) {
        tag <- .y$tag[[1]]
        color <- tags_table$color[tags_table$tag == .y$tag[[1]]]
        return(ggplot2::geom_rect(data=group_table, inherit.aes = FALSE,
                                  ggplot2::aes(xmin=.data$start, xmax=.data$end,
                                               ymin=.data$ymin, ymax=.data$ymax,
                                               group=tag),
                                  color="transparent", fill=color, alpha=0.3, show.legend=TRUE))
    }
    rectangles <- dplyr::group_map(group_tags_table, group_function)
    return(rectangles)
}

.plot_loggers_x_index <- function(data, color_by_logger=FALSE, is_datetime) {
    loggers_table <- myClim::mc_info_logger(data)
    localities <- unique(loggers_table$locality_id)
    if(length(localities) > 1) {
        stop("This type of plot is not supported for multiple localities.")
    }

    if(is_datetime) {
        sensors_table <- tibble::tibble(sensor="datetime",
                                        physical="datetime",
                                        color="gray",
                                        main_axis=TRUE)
        data_table <- .plot_loggers_x_index_datetime_table(data)
    } else {
        sensors_table <- myClim:::.plot_get_sensors_table(data, "physical")
        data_table <- .plot_loggers_x_index_data_table(data)
    }
    if(color_by_logger && !is_datetime) {
        series <- unique(data_table$series)
        color_table <- tibble::tibble(series=series,
                                      color=myClim:::.plot_const_PALETTE[1:length(series)])
    } else {
        series_table <- dplyr::distinct(data_table, .data$sensor_name, .data$series)
        color_table <- dplyr::select(sensors_table, "sensor", "color")
        color_table <- dplyr::left_join(series_table, color_table, by=c("sensor_name"="sensor"))
    }
    color_values <- color_table$color
    names(color_values) <- color_table$series

    ggplot_vars <- ggplot2::vars(.data$physical)
    scales <- "free_y"
    plot <- ggplot2::ggplot(data=data_table,
                            mapping=ggplot2::aes(x=.data$index, y=.data$value,
                                                    group=.data$series, colour=.data$series,
                                                    fill=.data$series)) +
            ggplot2::geom_line(mapping = ggplot2::aes(color=.data$series)) +
            ggplot2::scale_color_manual(values=color_values) +
            ggplot2::scale_fill_manual(values=color_values) +
            ggplot2::facet_grid(rows = ggplot_vars, scales=scales)
    return(plot)
}

.plot_loggers_x_index_data_table <- function(data) {
    logger_function <- function(logger) {
        sensor_function <- function(sensor) {
            sensor_info <- myClim::mc_data_sensors[[sensor$metadata@sensor_id]]
            result <- tibble::tibble(index=seq_along(logger$datetime),
                                     sensor_name=sensor$metadata@name,
                                     series=stringr::str_glue("{logger$metadata@name} {sensor$metadata@name}"),
                                     physical=sensor_info@physical,
                                     value=sensor$values)
            return(result)
        }
        sensors_data_table <- purrr::map_dfr(logger$sensors, sensor_function)
        return(sensors_data_table)
    }
    locality <- dplyr::first(data$localities)
    result <- purrr::map_dfr(locality$loggers, logger_function)
    return(result)
}

.plot_loggers_x_index_datetime_table <- function(data) {
    logger_function <- function(logger) {
        datetime_table <- tibble::tibble(index=seq_along(logger$datetime),
                                         sensor_name="datetime",
                                         series=stringr::str_glue("{logger$metadata@name} datetime"),
                                         physical="datetime",
                                         value=logger$datetime)
        return(datetime_table)
    }
    locality <- dplyr::first(data$localities)
    result <- purrr::map_dfr(locality$loggers, logger_function)
    return(result)
}
