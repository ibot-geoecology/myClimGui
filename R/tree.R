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
        names(result) <- purrr::imap(locality$loggers, ~ if(is.na(.x$metadata@serial_number)) stringr::str_glue("[{.y}]") else stringr::str_glue("[{.y}] {.x$metadata@serial_number}"))
        return(result)
    }

    result <- purrr::map(data$localities, locality_function)
    return(result)
}

.tree_filter_data <- function(data, selected) {
    is_agg <- myClim:::.common_is_agg_format(data)
    
    keep_function <- function(value) {
        subvalue <- value[[1]]
        if(is.double(subvalue)) return(FALSE)
        subsubvalue <- subvalue[[1]]
        if(is_agg) return(is.double(subsubvalue))
        if(is.double(subsubvalue)) return(FALSE)
        return(is.double(subsubvalue[[1]]))
    }

    selected <- purrr::keep(selected, keep_function)
    
    row_function <- function (item) {
        locality <- names(item)[[1]]
        second_name <- names(item[[1]])[[1]]
        if(is_agg) {
            return(list(locality=locality, sensor=second_name))
        }
        logger <- as.double(stringr::str_extract(second_name, "\\[(\\d+)\\].*", group=1))
        sensor <- names(item[[1]][[1]])[[1]]
        return(list(locality=locality, logger=logger, sensor=sensor))
    }
    
    table <- purrr::map_dfr(selected, row_function)
    result <- data

    logger_function <- function(locality_name, logger_index) {
        logger_table <- dplyr::filter(table, (.data$locality == locality_name) & (.data$logger == logger_index))
        logger <- data$localities[[locality_name]]$loggers[[logger_index]]
        logger$sensors <- logger$sensors[logger_table$sensor]
        return(logger)
    }

    locality_function <- function(name) {
        locality_table <- dplyr::filter(table, .data$locality == name)
        locality <- data$localities[[name]]
        if(is_agg) {
            locality$sensors <- locality$sensors[locality_table$sensor]
            return(locality)
        }
        locality$loggers <- purrr::map2(name, unique(locality_table$logger), logger_function)
        return(locality)
    }

    locality_names <- unique(table$locality)
    result$localities <- purrr::map(locality_names, locality_function)
    names(result$localities) <- locality_names
    
    return(result)
}

.tree_selected_to_vector <- function(selected) {
    item_function <- function(value) {
        result <- NULL
        repeat {
            if(length(value) == 0 || is.null(names(value))) {
                break
            } else if(length(value) == 1) {
                result <- c(result, names(value)[[1]])
                value <- value[[1]]
            } else {
                stop("Problem")
            }
        }
        return(paste(result, collapse="$"))
    }

    result <- sort(purrr::map_chr(selected, item_function))

    return(result)
}
