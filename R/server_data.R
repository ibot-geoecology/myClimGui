.server_data_get_main <- function(input, output, session, shared, data_table_value) {
    shiny::observeEvent(input$delete_states_button, {
        selected_rows <- input$data_table_rows_selected
    })

    shiny::observeEvent(input$save_delete_table_button, {
        saveRDS(shared$delete_table, input$file_delete_textinput)
    })

    output$data_table <- DT::renderDataTable({
        if(is.null(data_table_value())) {
            return(NULL)
        }
        data_table <- DT::datatable(data_table_value(),
                                   options = list(pageLength = 1000))
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