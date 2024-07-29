.ui_states_tab <- function(data) {
    shiny::tabPanel(.ui_const_STATES_TITLE,
                    icon = shiny::icon("tags"),
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