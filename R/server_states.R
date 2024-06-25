.server_states_get_main <- function(input, output, session, shared) {
    states_table <- shiny::reactiveVal()
    
    shiny::observeEvent(input$navbar_page, {
        tab_value <- shiny::req(input$navbar_page)
        if(tab_value == .ui_const_STATES_TITLE) {
            states_table(myClim::mc_info_states(shared$selected_data))
        }
    })
    
    output$states_table <- DT::renderDataTable(DT::datatable(states_table()))
    output$states_plot <- plotly::renderPlotly({
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        plot <- .plot_states(shared$selected_data, states_table()[selected_rows, ])
        return(plotly::ggplotly(plot))
    })
}