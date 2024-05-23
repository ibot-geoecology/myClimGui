.server_states_get_main <- function(data, input, output, session) {
    states_table <- myClim::mc_info_states(data)
    output$states_table <- DT::renderDataTable(DT::datatable(states_table))
    output$states_plot <- plotly::renderPlotly({
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        plot <- .plot_states(data, states_table[selected_rows, ])
        return(plotly::ggplotly(plot))
    })
}