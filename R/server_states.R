.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    shiny::observeEvent(input$states_table_cell_edit, {
        shared$data <- .data_edit_states(shared$data, input$states_table_cell_edit, states_table_value())
        shared$filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
        .server_states_reload_table(shared, states_table_value)
    })
    
    output$states_table <- DT::renderDataTable({
        if(is.null(states_table_value())) {
            return(NULL)
        }
        return(.server_states_get_table_for_dt(states_table_value()))
    })
    output$states_plot <- plotly::renderPlotly({
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        states_table <- .server_states_get_table_for_plot(states_table_value(), selected_rows)
        if(nrow(states_table) == 0) {
            return(NULL)
        }
        plot <- .plot_states(shared, states_table)
        return(plotly::ggplotly(plot))
    })
}

.server_states_get_table_for_dt <- function(states_table){
    states_table$start <- format(states_table$start, "%Y-%m-%d %H:%M:%S")
    states_table$end <- format(states_table$end, "%Y-%m-%d %H:%M:%S")
    result <-DT::datatable(states_table,
                           options = list(pageLength = 100),
                           editable = list(target = "cell", disable = list(columns = seq(7)[-5])))
    return(result)
}

.server_states_get_table_for_plot <- function(states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    states_table <- dplyr::filter(states_table, .data$start <= .data$end)
    return(states_table)
}

.server_states_reload_table <- function(shared, states_table_value) {
    states_table_value(myClim::mc_info_states(shared$filter_data))
}