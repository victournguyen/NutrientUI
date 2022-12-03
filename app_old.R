library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(colourpicker)

ingredients <- read_csv('ingredients.csv')

shorten <- function(col) {
    return(str_replace_all(col, '^.*\\.', ''))
}

var_choices <- vector()
cols <- colnames(ingredients)
for (i in 4:length(cols)) {
    var_choices[shorten(cols[i])] <- cols[i]
}
units <- c('Alpha Carotene'='mcg', 'Beta Carotene'='mcg', 'Beta Cryptoxanthin'='mcg', 'Carbohydrate'='g', 'Cholesterol'='mg', 'Choline'='mg', 'Fiber'='g', 'Lutein and Zeaxanthin'='mcg', 'Lycopene'='mcg', 'Niacin'='mg', 'Protein'='g', 'Retinol'='mcg', 'Riboflavin'='mg', 'Selenium'='mcg', 'Sugar Total'='g', 'Thiamin'='mg', 'Water'='g', 'Monosaturated Fat'='g', 'Polysaturated Fat'='g', 'Saturated Fat'='g', 'Total Lipid'='g', 'Calcium'='mg', 'Copper'='mg', 'Iron'='mg', 'Magnesium'='mg', 'Phosphorus'='mg', 'Potassium'='mg', 'Sodium'='mg', 'Zinc'='mg', 'Vitamin A - RAE'='mcg', 'Vitamin B12'='mcg', 'Vitamin B6'='mg', 'Vitamin C'='mg', 'Vitamin E'='mg', 'Vitamin K'='mcg')

ui <- fluidPage(
    titlePanel('title'),
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId='plot_radio',
                label='test',
                choices=list('Histogram'='histogram', 'Scatter Plot'='scatter'),
                selected='histogram'
            ),
            conditionalPanel(
                condition='input.plot_radio == "histogram"',
                selectInput(
                    inputId='var_select_hist',
                    label='select',
                    choices=var_choices,
                    selected='Data.Water'
                ),
                sliderInput(
                    inputId='bins',
                    label='Bins',
                    min=5,
                    max=100,
                    value=30
                )
            ),
            conditionalPanel(
                condition='input.plot_radio == "scatter"',
                selectInput(
                    inputId='var_select_scatter_x',
                    label='select x',
                    choices=var_choices,
                    selected='Data.Water'
                ),
                selectInput(
                    inputId='var_select_scatter_y',
                    label='select y',
                    choices=var_choices,
                    selected='Data.Fiber'
                )
            ),
            colourInput(
                inputId='color',
                label='Select color',
                value='blue',
                allowTransparent=TRUE
            )
        ),
        mainPanel(
            plotOutput('graph'),
        )
    )
)

server <- function(input, output) {
    output$graph <- renderPlot({
        if (input$plot_radio == 'histogram') {
            var_sel <- input$var_select_hist
            var_sel_short <- shorten(var_sel)
            ggplot(ingredients) +
                geom_histogram(
                    aes(ingredients[[var_sel]]),
                    bins=input$bins,
                    color='black',
                    fill=input$color
                ) +
                labs(
                    title=str_c('Distribution of ', var_sel_short),
                    x=sprintf('%s (%s)', var_sel_short, units[var_sel_short]),
                    y='Frequency'
                ) +
                theme_bw()
        }
        else {
            var_x_sel <- input$var_select_scatter_x
            var_y_sel <- input$var_select_scatter_y
            var_x_sel_short <- shorten(var_x_sel)
            var_y_sel_short <- shorten(var_y_sel)
            ggplot(ingredients) +
                geom_point(
                    aes(x=ingredients[[var_x_sel]], y=ingredients[[var_y_sel]]),
                    color=input$color
                ) +
                labs(
                    title=str_c(var_y_sel_short, ' vs. ', var_x_sel_short),
                    x=sprintf('%s (%s)', var_x_sel_short, units[var_x_sel_short]),
                    y=sprintf('%s (%s)', var_y_sel_short, units[var_y_sel_short])
                ) +
                theme_bw()
        }
    })
}

shinyApp(ui=ui, server=server)