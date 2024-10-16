.plot_const_NEIGHBORHOOD_LENGTH <- 13

.plot_states <- function(shared, states_table) {
    groupped_table <- dplyr::group_by(states_table, .data$locality_id, .data$logger_index, .data$sensor_name)
    range <- .plot_states_get_range(shared$data, groupped_table)
    data_table <- .plot_states_get_data(shared$data, groupped_table, range)
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
            logger_index <- .y$logger_index[[1]]
            step <- data$localities[[locality_id]]$loggers[[logger_index]]$clean_info@step
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
            logger_index <- .y$logger_index[[1]]
            series_name <- stringr::str_glue("{locality_id}[{logger_index}]: {sensor_name}")
            datetime <- locality$loggers[[logger_index]]$datetime
            values <- locality$loggers[[logger_index]]$sensors[[sensor_name]]$values
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