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

.server_data_get_table <- function(input, shared){
    if(.server_plot_is_visible_datetime_plot(input)) {
        result <- .data_get_dataview_table_index(shared$data, shared$selection_table, shared$crop_range[[1]], shared$crop_range[[2]])
    } else {
        crop_interval <- lubridate::interval(shared$crop_range[[1]], shared$crop_range[[2]])
        result <- .data_get_dataview_table(shared$data, shared$selection_table, crop_interval)
    }
    return(result)
}