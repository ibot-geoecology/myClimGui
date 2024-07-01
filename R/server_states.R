.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    output$states_table <- DT::renderDataTable({
        if(is.null(states_table_value())) {
            return(NULL)
        }
        data_table <-DT::datatable(states_table_value(),
                                   options = list(pageLength = 100))
        return(data_table)
    })
    output$states_plot <- plotly::renderPlotly({
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        plot <- .plot_states(shared$selected_data, states_table_value()[selected_rows, ])
        return(plotly::ggplotly(plot))
    })
}

.server_states_get_table <- function(shared){
    result <- myClim::mc_info_states(shared$selected_data)
    result$start <- format(result$start, "%Y-%m-%d %H:%M:%S")
    result$end <- format(result$end, "%Y-%m-%d %H:%M:%S")
    return(result)
}