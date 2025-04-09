.ui_states_tab <- function(shared) {
    shiny::tabPanel(.ui_const_STATES_TITLE,
                    icon = shiny::icon("tags"),
        shiny::fluidRow(
            shiny::column(
                shiny::checkboxInput("filter_by_plot_checkbox", .texts_plot_filter_by_plot, value=TRUE),
                width = 2,
            ),
            shiny::column(
                width = 5,
            ),
            shiny::column(
                shiny::actionButton("save_states_button", .texts_states_save_button),
                width = 2,
                style="padding-right: 0px; text-align: right; ",
            ),
            shiny::column(
                shiny::textInput("file_states_textinput", NULL, value="states_backup.rds", width="100%"),
                width = 3,
            ),
        ),
        shiny::fluidRow(
            shiny::column(
                shiny::selectInput("tag_select", "Tag", "all"),
                width = 6,
                style="padding: 5px; "
            ),
            shiny::column(
                shiny::actionButton("new_state_button", .texts_new, disabled = shared$is_uncleaned_raw),
                shiny::actionButton("edit_state_button", .texts_edit, disabled = shared$is_uncleaned_raw),
                shiny::actionButton("delete_states_button", .texts_delete, disabled = shared$is_uncleaned_raw),
                width = 6,
                style="padding: 5px; text-align: right;",
            ),
        ),
        shiny::fluidRow(
            shiny::column(
                DT::dataTableOutput("states_table"),
                width = 12
            )
        ),
        shiny::fluidRow(
            shiny::column(
                shiny::checkboxInput("states_show_plot_checkbox", "Show plot", value=TRUE),
                width = 2,
            ),
            shiny::column(
                shiny::checkboxInput("states_show_data_checkbox", "Show data", value=FALSE),
                width = 2,
            ),
            shiny::column(
                shiny::checkboxInput("states_use_plotly_checkbox", "Use plotly", value=FALSE),
                width = 2,
            ),
        ),
        shiny::fluidRow(
            shiny::column(
                plotly::plotlyOutput("states_plotly", width="100%"),
                shiny::plotOutput("states_ggplot", width="100%"),
                width = 12
            ),
        ),
        shiny::column(
            DT::dataTableOutput("states_data_table"),
            width = 12
        )
    )
}