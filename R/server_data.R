.server_data_get_main <- function(input, output, session, shared, data_table_value) {
    output$data_table <- DT::renderDataTable({
        if(is.null(data_table_value())) {
            return(NULL)
        }
        data_table <- DT::datatable(data_table_value(),
                                   options = list(pageLength = 1000))
        return(data_table)
    })
}

.server_data_get_table <- function(shared){
    crop_interval <- lubridate::interval(shared$crop_range[[1]], shared$crop_range[[2]])
    result <- .data_get_dataview_table(shared$data, shared$selection_table, crop_interval)
    return(result)
}