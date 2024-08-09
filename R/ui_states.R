.ui_states_tab <- function(data) {
    shiny::tabPanel(.ui_const_STATES_TITLE,
                    icon = shiny::icon("tags"),
        shiny::fluidRow(
            shiny::column(
                shiny::actionButton("new_state_button", .texts_new),
                shiny::actionButton("range_button", .texts_edit_range),
                shiny::actionButton("delete_states_button", .texts_delete),
                width = 12,
                style="padding: 5px; "
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
                plotly::plotlyOutput("states_plot", width="100%"),
                width = 12
            )
        )
    )
}