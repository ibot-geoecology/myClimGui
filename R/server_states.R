.server_states_const_RANGE_PAGE_LENGTH <- 25
.server_states_const_RANGE_LENGTH_MENU <- c(10, 25, 50, 100, 1000)

.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    edit_range_table_value <- shiny::reactiveVal()
    selected_range <- shiny::reactiveVal()
    form_mode <- shiny::reactiveVal()
        
    shiny::observeEvent(input$states_table_cell_edit, {
        shared$data <- .server_states_edit_text_cells(shared$data, input$states_table_cell_edit, states_table_value())
        .server_states_reload_data_after_edit(input, session, shared, states_table_value)
    })
    
    shiny::observeEvent(input$range_button, {
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification)
            return(NULL)
        }
        form_mode("edit")
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        edit_range_table_value(.server_states_get_table_for_edit_range(shared, df_states, selected_rows))
        .server_states_open_form_dialog(FALSE)
    })
   
    shiny::observeEvent(input$new_state_button, {
        form_mode("new")
        table_data <- .data_get_dataview_table(shared$data, shared$selection_table, shared$crop_range)
        edit_range_table_value(table_data)
        .server_states_open_form_dialog(TRUE)
    })
   
    shiny::observeEvent(input$delete_states_button, {
        selected_rows <- input$states_table_rows_selected
        count_states <- length(selected_rows)
        if(count_states == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification)
            return(NULL)
        }
        question <- stringr::str_glue(.texts_states_delete_states_question)
        shiny::showModal(shiny::modalDialog(title=question,
                                        footer= shiny::tagList(
                                            shiny::modalButton(.texts_cancel),
                                            shiny::actionButton("confirm_delete_states_button", .texts_delete)
                                        )))
    })
   
    shiny::observeEvent(input$confirm_delete_states_button, {
        selected_rows <- input$states_table_rows_selected
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        delete_table <- df_states[selected_rows, ]
        shared$data <- .data_delete_states(shared$data, delete_table)
        .server_states_reload_data_after_edit(input, session, shared, states_table_value)
        shiny::removeModal()
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
        
        .server_states_reload_data_after_edit(input, session, shared, states_table_value)
        shiny::removeModal()
    })
    
    shiny::observeEvent(input$states_use_plotly_checkbox, {
        if(input$states_use_plotly_checkbox) {
            shinyjs::show(id="states_plotly")
            shinyjs::hide(id="states_ggplot")
        } else {
            shinyjs::show(id="states_ggplot")
            shinyjs::hide(id="states_plotly")
        }
    })

    shiny::observeEvent(input$filter_by_plot_checkbox, {
        tab_value <- shiny::req(input$navbar_page)
        if(tab_value != .ui_const_STATES_TITLE) {
            return()
        }
        .server_states_reload_table(input, session, shared, states_table_value)
    })

    output$states_table <- DT::renderDataTable({
        if(is.null(states_table_value())) {
            return(NULL)
        }
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        return(.server_states_get_table_for_dt(df_states))
    })
    
    output$states_plotly <- plotly::renderPlotly({
        if(!input$states_use_plotly_checkbox) {
            return(NULL)
        }
        plot <- .server_states_get_plot(shared, input, states_table_value)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plotly::ggplotly(plot))
    })
    
    output$states_ggplot <- shiny::renderPlot({
        if(input$states_use_plotly_checkbox) {
            return(NULL)
        }
        plot <- .server_states_get_plot(shared, input, states_table_value)
        return(plot)
    }, res = 96)
    
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

.server_states_reload_data_after_edit <- function(input, session, shared, states_table_value) {
    .server_states_reload_table(input, session, shared, states_table_value)
}

.server_states_reload_table <- function(input, session, shared, states_table_value) {
    if(input$filter_by_plot_checkbox) {
        selection_table <- shared$selection_table
        filter_data <- myClim::mc_filter(shared$data, localities = unique(selection_table$locality_id))
        result <- myClim::mc_info_states(filter_data)
        selection_table$selected <- TRUE
        result <- dplyr::left_join(result, selection_table, by=c("locality_id", "logger_index", "sensor_name"))
        result <- dplyr::filter(result, !is.na(.data$selected))
        result <- dplyr::select(result, -"selected")
    } else {
        result <- myClim::mc_info_states(shared$data)
    }
    tags <- c("all", sort(unique(result$tag)))
    selected_tag <- input$tag_select
    if(!(selected_tag %in% tags)) {
        selected_tag <- "all"
    }
    shiny::updateSelectInput(session, "tag_select", choices = tags, selected = selected_tag)
    states_table_value(result)
}

.server_states_get_table_for_edit_range <- function(shared, states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    selection_table <- dplyr::select(states_table, "locality_id", "logger_index", "sensor_name")
    selection_table <- dplyr::distinct(selection_table)
    result <- .data_get_dataview_table(shared$data, selection_table, shared$crop_range)
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
    message_text <- NULL
    button_text <- .texts_edit
    title <- .texts_edit_range
    if(is_new_state) {
        new_tag_input <- shiny::textInput("new_tag", .texts_tag)
        new_value_input <- shiny::textInput("new_value", .texts_value)
        message_text <- shiny::span(.texts_states_new_states_warning)
        button_text <- .texts_new
        title <- .texts_states_new_states
    }
    shiny::showModal(shiny::modalDialog(title=title,
                                        size="l",
                                        footer= shiny::tagList(
                                            shiny::modalButton(.texts_cancel),
                                            shiny::actionButton("confirm_state_form_button", button_text)
                                        ),
                                        message_text,
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
    ranges_table <- .data_selection_table_add_range(shared$data, shared$selection_table)
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

.server_states_get_plot <- function(shared, input, states_table_value) {
    selected_rows <- input$states_table_rows_selected
    if(length(selected_rows) == 0) {
        return(NULL)
    }
    df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
    states_table <- .server_states_get_table_for_plot(df_states, selected_rows)
    if(nrow(states_table) == 0) {
        return(NULL)
    }
    plot <- .plot_states(shared, states_table)
    return(plot)
}

.server_states_get_filtered_dataframe <- function(states_table_value, selected_tag) {
    result <- states_table_value()
    if(selected_tag != "all") {
        result <- dplyr::filter(result, .data$tag == selected_tag)
    }
    return(result)
}