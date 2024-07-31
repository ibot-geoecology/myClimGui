.server_data_get_main <- function(input, output, session, shared, data_table_value) {
    output$data_table <- DT::renderDataTable({
        if(is.null(data_table_value())) {
            return(NULL)
        }
        data_table <-DT::datatable(data_table_value(),
                                   options = list(pageLength = 1000))
        return(data_table)
    })
}

.server_data_get_table <- function(shared){
    crop_data <- myClim::mc_prep_crop(shared$filter_data, shared$crop_range[[1]], shared$crop_range[[2]])
    result <- myClim::mc_reshape_wide(crop_data)
    result$datetime <- format(result$datetime, "%Y-%m-%d %H:%M:%S")
    return(result)
}