.server_states_get_main <- function(data, input, output, session) {
    states_table <- myClim::mc_info_states(data)
    output$states_table <- DT::renderDataTable(DT::datatable(states_table))
}