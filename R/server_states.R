.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    shiny::observeEvent(input$states_table_cell_edit, {
        shared$data <- .data_edit_states(shared$data, input$states_table_cell_edit, states_table_value())
    })
    
    output$states_table <- DT::renderDataTable({
        if(is.null(states_table_value())) {
            return(NULL)
        }
        data_table <-DT::datatable(states_table_value(),
                                   options = list(pageLength = 100),
                                   editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4, 6, 7))))
        return(data_table)
    })
    output$states_plot <- plotly::renderPlotly({
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        states_table <- .server_states_get_table_for_plot(shared, states_table_value(), selected_rows)
        if(nrow(states_table) == 0) {
            return(NULL)
        }
        plot <- .plot_states(shared, states_table)
        return(plotly::ggplotly(plot))
    })
}

.server_states_get_table <- function(shared){
    result <- myClim::mc_info_states(shared$filter_data)
    result$start <- format(result$start, "%Y-%m-%d %H:%M:%S")
    result$end <- format(result$end, "%Y-%m-%d %H:%M:%S")
    return(result)
}

.server_states_get_table_for_plot <- function(shared, states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    states_table$start <- lubridate::ymd_hms(states_table$start)
    states_table$start[states_table$start < shared$crop_range[[1]]] <- shared$crop_range[[1]]
    states_table$end <- lubridate::ymd_hms(states_table$end)
    states_table$end[states_table$end > shared$crop_range[[2]]] <- shared$crop_range[[2]]
    states_table <- dplyr::filter(states_table, .data$start <= .data$end)
}