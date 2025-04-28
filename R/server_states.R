.server_states_const_RANGE_PAGE_LENGTH <- 25
.server_states_const_RANGE_LENGTH_MENU <- c(10, 25, 50, 100, 1000)

.server_states_get_main <- function(input, output, session, shared, states_table_value) {
    edit_range_table_value <- shiny::reactiveVal()
    selected_range <- shiny::reactiveVal()
    form_mode <- shiny::reactiveVal()
    action_selected_rows <- shiny::reactiveVal()
    selected_range_value <- shiny::reactiveVal(list(is_init=TRUE, value=NULL))
        
    shiny::observeEvent(input$edit_state_button, {
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification, type = "error")
            return(NULL)
        }
        action_selected_rows(selected_rows)
        form_mode("edit")
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        edit_range_table_value(.server_states_get_table_for_edit_range(shared, df_states, selected_rows))
        edit_states_table <- df_states[action_selected_rows(), ]
        edit_date_range <- .server_states_get_edit_date_range(edit_states_table)
        selection_range <- .server_states_get_range_table_selection(edit_range_table_value(), edit_date_range,
                                                                    selected_range_value)
        selected_range_value(list(is_init=TRUE, value=selection_range))
        .server_states_open_form_dialog(FALSE, edit_states_table)
    })
   
    shiny::observeEvent(input$new_state_button, {
        form_mode("new")
        crop_interval <- lubridate::interval(shared$crop_range[[1]], shared$crop_range[[2]])
        table_data <- .data_get_dataview_table(shared$data, shared$selection_table, crop_interval)
        edit_range_table_value(table_data)
        action_selected_rows(NULL)
        selected_range_value(list(is_init=TRUE, value=NULL))
        .server_states_open_form_dialog(TRUE)
    })
   
    shiny::observeEvent(input$delete_states_button, {
        selected_rows <- input$states_table_rows_selected
        count_states <- length(selected_rows)
        if(count_states == 0) {
            shiny::showNotification(.texts_states_not_selected_states_notification, type = "error")
            return(NULL)
        }
        action_selected_rows(selected_rows)
        question <- stringr::str_glue(.texts_states_delete_states_question)
        shiny::showModal(shiny::modalDialog(title=question,
                                        footer= shiny::tagList(
                                            shiny::modalButton(.texts_cancel),
                                            shiny::actionButton("confirm_delete_states_button", .texts_delete)
                                        )))
    })
   
    shiny::observeEvent(input$confirm_delete_states_button, {
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        delete_table <- df_states[action_selected_rows(), ]
        action_selected_rows(NULL)
        shared$data <- .data_delete_states(shared$data, delete_table)
        .server_states_reload_data_after_edit(input, session, shared, states_table_value)
        shiny::removeModal()
    })
   
    shiny::observeEvent(input$confirm_state_form_button, {
        editors_range <- .server_states_get_range_from_editors(input)
        start <- editors_range[[1]]
        end <- editors_range[[2]]
        if(is.na(start) || is.na(end)) {
            shiny::showNotification(.texts_states_not_correct_range_notification, type = "error")
            return()
        }
        start_text <- format(start, "%Y-%m-%d %H:%M:%S")
        end_text <- format(end, "%Y-%m-%d %H:%M:%S")
        table_range <- range(edit_range_table_value()$datetime)
        if(start_text > end_text || start_text > table_range[[2]] || end_text < table_range[[1]]) {
            shiny::showNotification(.texts_states_not_correct_range_notification, type = "error")
            return()
        }
        if(form_mode() == "new")
        {
            if(input$edit_tag == "") {
                shiny::showNotification(.texts_states_empty_tag_notification, type = "error")
                return()
            }
            shared$data <- .server_states_add_states(shared, start, end, input$edit_tag, input$edit_value)
        } else if(form_mode() == "edit") {
            df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
            changed_table <- df_states[action_selected_rows(), ]
            action_selected_rows(NULL)
            shared$data <- .server_states_edit_form(shared$data, changed_table, start, end, input)
        }
        
        .server_states_reload_data_after_edit(input, session, shared, states_table_value)
        shiny::removeModal()
    })
    
    shiny::observeEvent(input$cancel_form_button, {
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

    shiny::observeEvent(input$edit_range_table_rows_selected, {
        if(selected_range_value()$is_init)
        {
            previous_range <- c(0, 0)
            output_range <- selected_range_value()$value
        } else {
            selected_rows <- input$edit_range_table_rows_selected
            previous_range <- selected_range_value()$value
            output_range <- .server_states_get_output_selected_range(selected_rows, previous_range)
        }
        .server_states_change_range_selection(previous_range, output_range, selected_range_value)
    }, ignoreNULL=FALSE)

    shiny::observeEvent(input$clear_range_state_form_button, {
        .server_states_clean_range_selection(selected_range_value)
    })

    shiny::observeEvent(input$save_states_button, {
        states_table <- myClim::mc_info_states(shared$data)
        saveRDS(states_table, input$file_states_textinput)
        shiny::showNotification(stringr::str_glue(.texts_states_file_saved))
    })

    # Update start and end date and time inputs by changed selected_range_value
    shiny::observe({
        start_date <- as.Date(NA)
        start_time <- lubridate::ymd_h("1970-01-01 00")
        end_date <- as.Date(NA)
        end_time <- start_time
        range_value <- selected_range_value()
        if(!is.null(range_value$value)) {
            shiny::isolate(selected_datetimes <- edit_range_table_value()$datetime[range_value$value])
            start_date <- lubridate::ymd_hms(selected_datetimes[[1]])
            start_time <- start_date
            end_date <- lubridate::ymd_hms(selected_datetimes[[2]])
            end_time <- end_date
        }
        if(!("edit_start_date" %in% names(input))) {
            return()
        }
        shiny::isolate({
            suppressWarnings({
                shiny::updateDateInput(session, "edit_start_date", value=start_date)
                shinyTime::updateTimeInput(session, "edit_start_time", value=start_time)
                shiny::updateDateInput(session, "edit_end_date", value=end_date)
                shinyTime::updateTimeInput(session, "edit_end_time", value=end_time)})
        })
    })

    range_editors_changed <- shiny::reactive({
        list(input$edit_start_date,
            input$edit_start_time,
            input$edit_end_date,
            input$edit_end_time)
    })

    shiny::observeEvent(range_editors_changed(), {
        if(is.null(input$edit_start_date)) {
            return()
        }
        editors_range <- .server_states_get_range_from_editors(input)
        start <- editors_range[[1]]
        end <- editors_range[[2]]
        if(is.na(start)) {
            return()
        }
        if(is.na(end)) {
            shiny::updateDateInput(session, "edit_end_date", value=start)
            return()
        }
        if(start > end) {
            return()
        }
        shiny::isolate({
            previous_range <- selected_range_value()$value
            output_range <- .server_states_get_range_table_selection(edit_range_table_value(), c(start, end),
                                                                     selected_range_value)
        })
        .server_states_change_range_selection(previous_range, output_range, selected_range_value)
    })

    output$states_table <- DT::renderDataTable({
        if(is.null(states_table_value()) || is.null(states_table_value()$table)) {
            return(NULL)
        }
        df_states <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        return(.server_states_get_table_for_dt(df_states))
    }, server = FALSE)
    
    output$states_plotly <- plotly::renderPlotly({
        if(!input$states_show_plot_checkbox || !input$states_use_plotly_checkbox) {
            return(NULL)
        }
        plot <- .server_states_get_plot(shared, input, states_table_value)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plotly::ggplotly(plot))
    })
    
    output$states_ggplot <- shiny::renderPlot({
        if(!input$states_show_plot_checkbox || input$states_use_plotly_checkbox) {
            return(NULL)
        }
        plot <- .server_states_get_plot(shared, input, states_table_value)
        return(plot)
    }, res = 96)
    
    output$states_data_table <- DT::renderDataTable({
        if(!input$states_show_data_checkbox) {
            return(NULL)
        }
        selected_rows <- input$states_table_rows_selected
        if(length(selected_rows) == 0) {
            return(NULL)
        }
        states_table <- .server_states_get_filtered_dataframe(states_table_value, input$tag_select)
        states_table <- states_table[selected_rows, ]
        return(.server_states_get_data_table_for_dt(shared, states_table))
    })
    
    output$edit_range_table <- DT::renderDataTable({
        if(is.null(edit_range_table_value())) {
            return(NULL)
        }
        shiny::isolate({
            range <- selected_range_value()$value
        })
        if(!is.null(range)) {
            selected <- range[[1]]:range[[2]]
        } else {
            selected <- NULL
        }
        result <-DT::datatable(edit_range_table_value(),
                               selection = list(target="row", selected=selected),
                               options = list(pageLength = .server_states_const_RANGE_PAGE_LENGTH,
                                              lengthMenu = .server_states_const_RANGE_LENGTH_MENU,
                                              scrollX = TRUE))
        return(result)
    })
}

.server_states_get_table_for_dt <- function(states_table) {
    states_table$start <- format(states_table$start, "%Y-%m-%d %H:%M:%S")
    states_table$end <- format(states_table$end, "%Y-%m-%d %H:%M:%S")
    result <-DT::datatable(states_table,
                           selection = "none", #list(target="row", selected=selected_rows),
                           options = list(pageLength = 10,
                                          select = list(style = "os", items = "row")),
                           extensions = c('Select'))
    return(result)
}

.server_states_get_table_for_plot <- function(states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    states_table <- dplyr::filter(states_table, .data$start <= .data$end)
    return(states_table)
}

.server_states_reload_data_after_edit <- function(input, session, shared, states_table_value) {
    .server_states_reload_table(input, session, shared, states_table_value)
    table_tags <- unique(states_table_value()$table$tag)
    if(.app_shared_load_tags_if_need(shared, table_tags)) {
        .server_plot_update_tags(session, shared$tags)
    }
}

.server_states_reload_table <- function(input, session, shared, states_table_value) {
    if(input$filter_by_plot_checkbox) {
        selection_table <- shared$selection_table
        filter_data <- myClim::mc_filter(shared$data, localities = unique(selection_table$locality_id))
        result <- myClim::mc_info_states(filter_data)
        selection_table$selected <- TRUE
        result <- dplyr::left_join(result, selection_table, by=c("locality_id", "logger_name", "sensor_name"))
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
    states_table_value(list(table=result, selected_rows=NULL))
}

.server_states_get_table_for_edit_range <- function(shared, states_table, selected_rows) {
    states_table <- states_table[selected_rows, ]
    selection_table <- dplyr::select(states_table, "locality_id", "logger_name", "sensor_name")
    selection_table <- dplyr::distinct(selection_table)
    crop_interval <- NULL
    if(!is.null(shared$crop_range)) {
        crop_interval <- lubridate::interval(shared$crop_range[[1]], shared$crop_range[[2]])
    }
    result <- .data_get_dataview_table(shared$data, selection_table, crop_interval)
    return(result)
}

.server_states_open_form_dialog <- function(is_new_state, edit_states_table=NULL) {
    message_text <- NULL
    button_text <- .texts_edit
    title <- .texts_edit
    tag_title <- .texts_tag
    value_title <- .texts_value
    placeholder <- ""
    if(is_new_state) {
        message_text <- shiny::span(.texts_states_new_states_warning)
        button_text <- .texts_create
        title <- .texts_states_new_states
    }
    if(!is.null(edit_states_table)) {
        edited_tags <- stringr::str_c(unique(edit_states_table$tag), collapse=", ")
        tag_title <- stringr::str_glue("{.texts_tag} ({edited_tags})")
        edited_values <- stringr::str_c(unique(edit_states_table$value), collapse=", ")
        value_title <- stringr::str_glue("{.texts_value} ({edited_values})")
        placeholder <- .texts_unchanged
    }
    shiny::showModal(shiny::modalDialog(title=title,
                                        size="l",
                                        footer=NULL,
                                        message_text,
                                        shiny::textInput("edit_tag", tag_title, placeholder = placeholder),
                                        shiny::textInput("edit_value", value_title, placeholder = placeholder),
                                        shiny::fluidRow(
                                            shiny::column(
                                                suppressWarnings(
                                                    shiny::dateInput("edit_start_date", .texts_start_date,
                                                                     value=as.Date(NA), width="100%")),
                                                width = 2,
                                                style="padding-right: 0px;"
                                            ),
                                            shiny::column(
                                                shinyTime::timeInput("edit_start_time", .texts_start_time, seconds=FALSE),
                                                width = 3
                                            ),
                                            shiny::column(
                                                suppressWarnings(
                                                    shiny::dateInput("edit_end_date", .texts_end_date,
                                                                     value=as.Date(NA), width="100%")),
                                                width = 2,
                                                style="padding-right: 0px;"
                                            ),
                                            shiny::column(
                                                shinyTime::timeInput("edit_end_time", .texts_end_time, seconds=FALSE),
                                                width = 3
                                            ),
                                            shiny::column(
                                                shiny::actionButton("clear_range_state_form_button", .texts_clear, icon=shiny::icon("trash-can")),
                                                width = 2
                                            )),
                                        shiny::fluidRow(
                                            shiny::column(
                                                width = 8
                                            ),
                                            shiny::column(
                                                shiny::actionButton("cancel_form_button", .texts_cancel, width="100%"),
                                                width = 2,
                                                style="padding-right: 2px;"
                                            ),
                                            shiny::column(
                                                shiny::actionButton("confirm_state_form_button", button_text, width="100%"),
                                                width = 2,
                                                style="padding-left: 2px;"
                                            )),
                                        shiny::br(),
                                        shiny::br(),
                                        DT::dataTableOutput("edit_range_table")))
}

.server_states_edit_form <- function(data, changed_table, start, end, input) {
    new_values <- list()
    new_values[["start"]] <- start
    new_values[["end"]] <- end
    if(input$edit_tag != "") {
        new_values[["tag"]] <- input$edit_tag
    }
    if(input$edit_value != "") {
        new_values[["value"]] <- input$edit_value
    }
    result <- .data_edit_states(data, changed_table, new_values)
    return(result)
}


.server_states_add_states <- function(shared, start, end, tag, value) {
    ranges_table <- .data_selection_table_add_range(shared$data, shared$selection_table)
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
    result <- states_table_value()$table
    if(selected_tag != "all") {
        result <- dplyr::filter(result, .data$tag == selected_tag)
    }
    return(result)
}

.server_states_get_data_table_for_dt <- function(shared, states_table){
    selection_table <- dplyr::select(states_table, "locality_id", "logger_name", "sensor_name")
    selection_table <- dplyr::distinct(selection_table)
    intervals <- lubridate::interval(states_table$start, states_table$end)
    data_df <- .data_get_dataview_table(shared$data, selection_table, intervals)

    result <-DT::datatable(data_df,
                           options = list(pageLength = 1000))
    return(result)
}

.server_states_get_edit_date_range <- function(edit_states_table) {
    start <- min(edit_states_table$start)
    end <- max(edit_states_table$end)
    return(c(start, end))
}

.server_states_get_range_table_selection <- function(range_table, edit_date_range, selected_range_value) {
    start <- format(edit_date_range[[1]], "%Y-%m-%d %H:%M:%S") 
    end <- format(edit_date_range[[2]], "%Y-%m-%d %H:%M:%S")
    condition <- (range_table$datetime >= start) & (range_table$datetime <= end)
    indexes <- which(condition)
    if(length(indexes) == 0) {
        return(NULL)
    }
    return(c(min(indexes), max(indexes)))
}

.server_states_get_output_selected_range <- function(selected_rows, previous_range) {
    if(length(selected_rows) == 0) {
        return(NULL)
    }
    if(is.null(previous_range) ||
        min(selected_rows) < previous_range[[1]] ||
        max(selected_rows) > previous_range[[2]] ||
        length(selected_rows) == 1 ||
        all(diff(selected_rows) == 1)) {
        return(c(min(selected_rows), max(selected_rows)))
    }
    selected_table <- tibble::tibble(row_id=selected_rows, selected=TRUE)
    table <- tibble::tibble(row_id=previous_range[[1]]:previous_range[[2]])
    table <- dplyr::left_join(table, selected_table, by=c("row_id"="row_id"))
    table$selected[is.na(table$selected)] <- FALSE
    new_selected_rows <- table$row_id[!table$selected]
    if(length(new_selected_rows) == 0) {
        return(previous_range)
    }
    new_selected_row <- min(new_selected_rows)
    start_diff <- new_selected_row - previous_range[[1]]
    end_diff <- previous_range[[2]] - new_selected_row
    if(start_diff < end_diff) {
        return(c(new_selected_row, previous_range[[2]]))
    }
    return(c(previous_range[[1]], new_selected_row))
}

.server_states_change_range_selection <- function(previous_range, output_range, selected_range_value) {
    if(is.null(previous_range) && is.null(output_range)) {
        return(previous_range)
    }
    if(!is.null(previous_range) && !is.null(output_range) && all(previous_range == output_range)) {
        return(previous_range)
    }
    new_selected_rows <- NULL
    if(!is.null(output_range)) {
        new_selected_rows <- output_range[[1]]:output_range[[2]]
    }
    shiny::isolate({
        DT::selectRows(DT::dataTableProxy("edit_range_table"), new_selected_rows)
    })
    selected_range_value(list(is_init=FALSE, value=output_range))
}

.server_states_clean_range_selection <- function(selected_range_value) {
    DT::selectRows(DT::dataTableProxy("edit_range_table"), NULL)
    selected_range_value(list(is_init=FALSE, value=NULL))
}

.server_states_get_range_from_editors <- function(input) {
    start_date <- input$edit_start_date
    start_time <- input$edit_start_time
    end_date <- input$edit_end_date
    end_time <- input$edit_end_time
    if(length(start_date) == 0) {
        start <- lubridate::NA_Date_
    } else {
        start <- as.POSIXct(start_date) +
            lubridate::hour(start_time) * 60 * 60 +
            lubridate::minute(start_time) * 60 +
            lubridate::second(start_time)
    }
    if(length(end_date) == 0) {
        end <- lubridate::NA_Date_
    } else {
        end <- as.POSIXct(end_date) +
            lubridate::hour(end_time) * 60 * 60 +
            lubridate::minute(end_time) * 60 +
            lubridate::second(end_time)
    }
    return(c(start, end))
}