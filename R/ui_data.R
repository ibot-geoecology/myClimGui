.ui_data_tab <- function(data) {
    shiny::tabPanel(.ui_const_DATA_TITLE,
                    icon = shiny::icon("table"),
        shiny::fluidRow(
            shiny::column(
                DT::dataTableOutput("data_table"),
                width = 12
            )
        ),
    )
}