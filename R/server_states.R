.server_states_const_COUNT_EDIT_RANGE_ROWS <- 500

.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    edit_range_table_value <- shiny::reactiveVal()
    selected_range <- shiny::reactiveVal()
        
    shiny::observeEvent(input$states_table_cell_edit, {
        shared$data <- .data_edit_states(shared$data, input$states_table_cell_edit, states_table_value())
        shared$filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
        .server_states_reload_table(shared, states_table_value)
    })
    
    shiny::observeEvent(input$range_button, {
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification)
            return(NULL)
        }
        edit_range_table_value(.server_states_get_table_for_edit_range(shared, states_table_value(), selected_rows))
        shiny::showModal(shiny::modalDialog(title=.texts_edit_range,
                                            size="l",
                                            footer= shiny::tagList(
                                                shiny::modalButton(.texts_cancel),
                                                shiny::actionButton("select_range_button", .texts_select)
                                            ),
                                            shiny::textInput("new_tag", .texts_tag),
                                            shiny::textInput("new_value", .texts_value),
                                            shiny::tagAppendAttributes(
                                                shiny::textOutput("selected_range_text"),
                                                style="font-weight: bold; "),
                                            shiny::br(),
                                            DT::dataTableOutput("edit_range_table")))
    })
   
    shiny::observeEvent(input$select_range_button, {
        selected_datetimes <- .server_states_get_selected_datetimes(input, edit_range_table_value)
        if(length(selected_datetimes) == 0 || length(selected_datetimes) > 2) {
            shiny::showNotification(.texts_states_not_correct_range_notification)
            return()
        }
        shiny::removeModal()
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
    
    output$edit_range_table <- DT::renderDataTable({
        if(is.null(edit_range_table_value())) {
            return(NULL)
        }
        result <-DT::datatable(edit_range_table_value(),
                               options = list(pageLength = .server_states_const_COUNT_EDIT_RANGE_ROWS))
        return(result)
    })
    
    output$selected_range_text <- shiny::renderText({
        selected_datetimes <- .server_states_get_selected_datetimes(input, edit_range_table_value)
        if(length(selected_datetimes) == 0) {
            return(.texts_states_not_selected_range)
        }
        if(length(selected_datetimes) == 1) {
            return(paste0(.texts_selected_semicolon, selected_datetimes))
        }
        if(length(selected_datetimes) == 2) {
            return(stringr::str_glue("{.texts_selected_semicolon}{selected_datetimes[[1]]} - {selected_datetimes[[2]]}"))
        }
        return(.texts_states_selected_too_many_rows)
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

.server_states_get_table_for_edit_range <- function(shared, states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    selection_table <- dplyr::select(states_table, "locality_id", "logger_index", "sensor_name")
    selection_table <- dplyr::distinct(selection_table)
    data <- .data_filter_by_selection_table(shared$filter_data, selection_table)
    crop_data <- myClim::mc_prep_crop(data, shared$crop_range[[1]], shared$crop_range[[2]])
    result <- myClim::mc_reshape_wide(crop_data)
    result$datetime <- format(result$datetime, "%Y-%m-%d %H:%M:%S")
    return(result)
}

.server_states_get_selected_datetimes <- function(input, edit_range_table_value) {
    selected_rows <- input$edit_range_table_rows_selected
    result <- edit_range_table_value()[selected_rows, ]
    result <- dplyr::arrange(result, "datetime")
    return(result$datetime)
}