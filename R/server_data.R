.server_data_get_main <- function(input, output, session, shared, data_table_value) {
    delete_table <- shiny::reactiveVal(shared$delete_table)

    shiny::observeEvent(input$delete_index_button, {
        selected_indexes <- .server_data_get_selected_indexes(input, data_table_value)
        if(is.null(selected_indexes)) {
            return()
        }
        insert_table <- tibble::tibble(locality_id = unique(shared$selection_table$locality_id),
                                       logger_name = unique(shared$selection_table$logger_name),
                                       raw_index = selected_indexes)
        shared$delete_table <- dplyr::bind_rows(shared$delete_table, insert_table) |>
            dplyr::distinct()
        delete_table(shared$delete_table)
    })

    shiny::observeEvent(input$restore_index_button, {
        selected_indexes <- .server_data_get_selected_indexes(input, data_table_value)
        locality_id = unique(shared$selection_table$locality_id)
        logger_name = unique(shared$selection_table$logger_name)
        shared$delete_table <- shared$delete_table |>
            dplyr::filter(!(.data$locality_id == locality_id &
                           .data$logger_name == logger_name &
                           .data$raw_index %in% selected_indexes))
        delete_table(shared$delete_table)
    })

    shiny::observeEvent(input$save_delete_table_button, {
        saveRDS(shared$delete_table, input$file_delete_textinput)
        shiny::showNotification(stringr::str_glue(.texts_data_file_saved))
    })

    output$data_table <- DT::renderDataTable({
        if(is.null(data_table_value())) {
            return(NULL)
        }
        data_table <- DT::datatable(data_table_value(),
                                   options = list(pageLength = 1000))
        selected_locality_id = unique(shared$selection_table$locality_id)
        selected_logger_name = unique(shared$selection_table$logger_name)
        selected_delete_table <- dplyr::filter(delete_table(), 
                                               .data$locality_id == selected_locality_id &
                                               .data$logger_name == selected_logger_name)
        if(nrow(selected_delete_table) > 0) { 
            data_table <- DT::formatStyle(data_table, "index", target = "row",
                                          backgroundColor = DT::styleEqual(selected_delete_table$raw_index,
                                            rep("gray", length(selected_delete_table$raw_index))))
        }
        return(data_table)
    })
}

.server_data_get_table <- function(input, shared){
    filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
    if(shared$is_uncleaned_raw && (length(filter_data$localities) > 1 ||
                                   length(filter_data$localities[[1]]$loggers) > 1)) {  
        shiny::showNotification(.texts_data_multi_loggers_error)
        return(NULL)
    }
    if(.server_plot_is_visible_datetime_plot(input)) {
        result <- .data_get_dataview_table_index(filter_data, shared$crop_range[[1]], shared$crop_range[[2]])
    } else {
        crop_interval <- lubridate::interval(shared$crop_range[[1]], shared$crop_range[[2]])
        result <- .data_get_dataview_table(filter_data, NULL, crop_interval)
    }
    return(result)
}

.server_data_get_selected_indexes <- function(input, data_table_value) {
    selected_rows <- input$data_table_rows_selected
    if(length(selected_rows) == 0) {
        return(NULL)
    }
    selection_table <- data_table_value()[selected_rows, ]
    return(selection_table$index)
}
