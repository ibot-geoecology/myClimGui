.server_states_const_RANGE_PAGE_LENGTH <- 25
.server_states_const_RANGE_LENGTH_MENU <- c(10, 25, 50, 100, 1000)

.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    edit_range_table_value <- shiny::reactiveVal()
    selected_range <- shiny::reactiveVal()
    form_mode <- shiny::reactiveVal()
        
    shiny::observeEvent(input$states_table_cell_edit, {
        shared$data <- .server_states_edit_text_cells(shared$data, input$states_table_cell_edit, states_table_value())
        .server_states_reload_data_after_edit(shared, states_table_value)
    })
    
    shiny::observeEvent(input$range_button, {
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification)
            return(NULL)
        }
        form_mode("edit")
        edit_range_table_value(.server_states_get_table_for_edit_range(shared, states_table_value(), selected_rows))
        .server_states_open_form_dialog(FALSE)
    })
   
    shiny::observeEvent(input$new_state_button, {
        form_mode("new")
        edit_range_table_value(.server_states_get_table_for_states_form(shared$filter_data, shared$crop_range))
        .server_states_open_form_dialog(TRUE)
    })
   
    shiny::observeEvent(input$confirm_state_form_button, {
        selected_datetimes <- .server_states_get_selected_datetimes(input, edit_range_table_value)
        if(length(selected_datetimes) == 0 || length(selected_datetimes) > 2) {
            shiny::showNotification(.texts_states_not_correct_range_notification)
            return()
        }
        if(form_mode() == "new")
        {
            if(input$new_tag == "") {
                shiny::showNotification(.texts_states_empty_tag_notification)
                return()
            }
            shared$data <- .server_states_add_states(shared, selected_datetimes, input$new_tag, input$new_value)
        } else if(form_mode() == "edit") {
            changed_table <- states_table_value()[input$states_table_rows_selected, ]
            shared$data <- .server_states_edit_range(shared$data, changed_table, selected_datetimes)
        }
        
        .server_states_reload_data_after_edit(shared, states_table_value)
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
                               options = list(pageLength = .server_states_const_RANGE_PAGE_LENGTH,
                                              lengthMenu = .server_states_const_RANGE_LENGTH_MENU,
                                              scrollX = TRUE))
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

.server_states_reload_data_after_edit <- function(shared, states_table_value) {
    shared$filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
    .server_states_reload_table(shared, states_table_value)
}

.server_states_reload_table <- function(shared, states_table_value) {
    states_table_value(myClim::mc_info_states(shared$filter_data))
}

.server_states_get_table_for_edit_range <- function(shared, states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    selection_table <- dplyr::select(states_table, "locality_id", "logger_index", "sensor_name")
    selection_table <- dplyr::distinct(selection_table)
    data <- .data_filter_by_selection_table(shared$filter_data, selection_table)
    result <- .server_states_get_table_for_states_form(data, shared$crop_range)
    return(result)
}

.server_states_get_table_for_states_form <- function(data, crop_range) {
    crop_data <- myClim::mc_prep_crop(data, crop_range[[1]], crop_range[[2]])
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

.server_states_open_form_dialog <- function(is_new_state) {
    new_tag_input <- NULL
    new_value_input <- NULL
    button_text <- .texts_edit
    if(is_new_state) {
        new_tag_input <- shiny::textInput("new_tag", .texts_tag)
        new_value_input <- shiny::textInput("new_value", .texts_value)
        button_text <- .texts_new
    }
    shiny::showModal(shiny::modalDialog(title=.texts_edit_range,
                                        size="l",
                                        footer= shiny::tagList(
                                            shiny::modalButton(.texts_cancel),
                                            shiny::actionButton("confirm_state_form_button", button_text)
                                        ),
                                        new_tag_input,
                                        new_value_input,
                                        shiny::tagAppendAttributes(
                                            shiny::textOutput("selected_range_text"),
                                            style="font-weight: bold; "),
                                        shiny::br(),
                                        DT::dataTableOutput("edit_range_table")))
}

.server_states_edit_text_cells <- function(data, edit_table, selected_states_table) {
    row_index <- edit_table$row[[1]]
    changed_row <- selected_states_table[row_index, ]
    column_index <- edit_table$col[[1]]
    column_name <- colnames(changed_row)[[column_index]]
    new_values <- list()
    new_values[[column_name]] <- edit_table$value
    result <- .data_edit_states(data, changed_row, new_values)
    return(result)
}

.server_states_edit_range <- function(data, changed_table, selected_datetimes) {
    new_values <- list()
    new_values[["start"]] <- lubridate::ymd_hms(selected_datetimes[[1]])
    if(length(selected_datetimes) == 1) {
        new_values[["end"]] <- new_values[["start"]]
    } else {
        new_values[["end"]] <- lubridate::ymd_hms(selected_datetimes[[2]])
    }
    result <- .data_edit_states(data, changed_table, new_values)
    return(result)
}


.server_states_add_states <- function(shared, selected_datetimes, tag, value) {
    browser()
    ranges_table <- .data_selection_table_add_range(shared$filter_data, shared$selection_table)
    start <- lubridate::ymd_hms(selected_datetimes[[1]])
    end <- start
    if(length(selected_datetimes) > 1) {
        end <- lubridate::ymd_hms(selected_datetimes[[2]])
    }
    selected_interval <- lubridate::interval(start=start, end=end)
    
    selection_table <- shared$selection_table[lubridate::int_overlaps(ranges_table$int, selected_interval), ]
    selection_table$tag <- tag
    selection_table$start <- start
    selection_table$end <- end
    
    if(value != "") {
        selection_table$value <- value
    }
    
    result <- myClim::mc_states_insert(shared$data, selection_table)
    return(result)
}
