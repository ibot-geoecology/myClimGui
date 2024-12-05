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
        names(result) <- purrr::imap(locality$loggers, ~ if(is.na(.x$metadata@serial_number)) stringr::str_glue("{.y}") else stringr::str_glue("{.y}({.x$metadata@serial_number})"))
        return(result)
    }

    result <- purrr::map(data$localities, locality_function)
    return(result)
}

.tree_get_selection_table <- function(data, selected) {
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
        locality_id <- names(item)[[1]]
        second_name <- names(item[[1]])[[1]]
        if(is_agg) {
            return(list(locality_id=locality_id, logger_name=NA, sensor_name=second_name))
        }
        logger_name <- stringr::str_extract(second_name, "(.+_\\d+)\\(.*\\)$", group=1)
        if(is.na(logger_name)) {
            logger_name <- second_name
        }
        sensor_name <- names(item[[1]][[1]])[[1]]
        return(list(locality_id=locality_id, logger_name=logger_name, sensor_name=sensor_name))
    }
    
    result <- purrr::map_dfr(selected, row_function)
    return(result)
}

.tree_change_selection <- function(tree, sensor, add) {
    node_function <- function(name, node) {
        if(name == sensor) {
            attr(node, "stselected") <- add
        }
        if(is.list(node))
        {
            result <- purrr::map2(names(node), node, node_function)
            attributes(result) <- attributes(node)
            node <- result
        }
        return(node)
    }

    result <- purrr::map2(names(tree), tree, node_function)
    attributes(result) <- attributes(tree)
    return(result)
}
