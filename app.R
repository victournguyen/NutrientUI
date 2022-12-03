library(shiny)
library(shiny.fluent)
library(tidyverse)
library(stringr)
library(ggplot2)
library(plotly)
library(colourpicker)
library(RColorBrewer)

ingredients <- read_csv('ingredients.csv')

shorten <- function(col) {
    return(str_replace_all(col, '^.*\\.', ''))
}

var_choices <- list()
cols <- colnames(ingredients)
for (i in 4:length(cols)) {
    var_choices[[i - 3]] <- list(key=cols[i], text=shorten(cols[i]))
}
units <- c('Alpha Carotene'='mcg', 'Beta Carotene'='mcg', 'Beta Cryptoxanthin'='mcg', 'Carbohydrate'='g', 'Cholesterol'='mg', 'Choline'='mg', 'Fiber'='g', 'Lutein and Zeaxanthin'='mcg', 'Lycopene'='mcg', 'Niacin'='mg', 'Protein'='g', 'Retinol'='mcg', 'Riboflavin'='mg', 'Selenium'='mcg', 'Sugar Total'='g', 'Thiamin'='mg', 'Water'='g', 'Monosaturated Fat'='g', 'Polysaturated Fat'='g', 'Saturated Fat'='g', 'Total Lipid'='g', 'Calcium'='mg', 'Copper'='mg', 'Iron'='mg', 'Magnesium'='mg', 'Phosphorus'='mg', 'Potassium'='mg', 'Sodium'='mg', 'Zinc'='mg', 'Vitamin A - RAE'='mcg', 'Vitamin B12'='mcg', 'Vitamin B6'='mg', 'Vitamin C'='mg', 'Vitamin E'='mg', 'Vitamin K'='mcg')

# Read broad category data
groups <- read_csv('category_broad.csv')
broad_cat <- character(0)
# Loop through each category
for (c in ingredients$Category) {
    u <- FALSE
    # Check which broad category the category belongs to and add to the vector
    for (g in 1:ncol(groups)) {
        if (c %in% groups[[g]]) {
            broad_cat <- append(broad_cat, colnames(groups)[g])
            u <- TRUE
            break
        }
    }
}
ingredients$Category.Broad <- as.factor(broad_cat)

ui <- fluentPage(
    tags$style(HTML('
        body {
            background-color: #faf9f8;
        }
        
        .checkbox-not-first {
            margin-top: 5px;
        }
        
        .stat-output {
            flex: 1 1 auto;
        }
    ')),
    h1('Nutrients in Food Ingredients'),
    div(
        style='display: flex;',
        div(
            class='card ms-depth-8 ms-sm4 ms-xl4',
            style='padding: 10px; margin-right: 10px; background-color: white;',
            ChoiceGroup.shinyInput(
                inputId='plot_radio',
                label='Select Plot Type',
                options=list(
                    list(key='histogram', text='Histogram'),
                    list(key='scatter', text='Scatter Plot')
                ),
                value='histogram'
            ),
            conditionalPanel(
                condition='input.plot_radio == "histogram"',
                br(),
                Dropdown.shinyInput(
                    inputId='var_select_hist',
                    label='Select Variable',
                    options=var_choices,
                    value='Data.Water'
                ),
                br(),
                Slider.shinyInput(
                    inputId='bins',
                    label='Bins',
                    min=1,
                    max=100,
                    value=30
                )
            ),
            conditionalPanel(
                condition='input.plot_radio == "scatter"',
                br(),
                div(
                    style='display: flex;',
                    div(
                        style='flex: 1 1 auto;',
                        Dropdown.shinyInput(
                            inputId='var_select_scatter_x',
                            label='Select X Variable',
                            options=var_choices,
                            value='Data.Water'
                        )
                    ),
                    div(
                        style='flex: 1 1 auto; margin-left: 10px;',
                        Dropdown.shinyInput(
                            inputId='var_select_scatter_y',
                            label='Select Y Variable',
                            options=var_choices,
                            value='Data.Fiber'
                        )
                    )
                )
            ),
            br(),
            Slider.shinyInput(
                inputId='num_obs',
                label='Observations',
                min=1,
                max=nrow(ingredients),
                value=nrow(ingredients)
            ),
            br(),
            div(
                style='display: flex;',
                div(
                    style='flex: 1 1 auto;',
                    Label('Select Categories'),
                    Checkbox.shinyInput(inputId='check_dairy', label='Dairy/Fatty', value=TRUE),
                    Checkbox.shinyInput(inputId='check_meat', className='checkbox-not-first', label='Meat', value=TRUE),
                    Checkbox.shinyInput(inputId='check_fruits', className='checkbox-not-first', label='Fruits/Vegetables', value=TRUE),
                    Checkbox.shinyInput(inputId='check_cereals', className='checkbox-not-first', label='Cereals/Grains', value=TRUE),
                    Checkbox.shinyInput(inputId='check_other', className='checkbox-not-first', label='Other', value=TRUE)
                ),
                div(
                    style='flex: 1 1 auto;',
                    Label('Color by Category'),
                    Toggle.shinyInput(inputId='toggle_colbycat')
                )
            ),
            conditionalPanel(
                condition='!input.toggle_colbycat',
                br(),
                Label('Select Color'),
                div(
                    style='display: flex; justify-content: center;',
                    ColorPicker.shinyInput(
                        inputId='color',
                        value='#3bbf9580',
                        showPreview=TRUE
                    )
                )
            )
        ),
        div(
            class='card ms-depth-8 ms-sm8 ms-xl8',
            style='display: flex; flex-direction: column; padding: 10px; background-color: white;',
            plotlyOutput(outputId='plot', height='100%'),
            uiOutput(outputId='stats')
            # textOutput(outputId='stats')
            # uiOutput(outputId='table')
        )
    )
)

server <- function(input, output) {
    
    slice_ingredients <- reactive({
        cat_sel <- c(input$check_dairy, input$check_meat, input$check_fruits, input$check_cereals, input$check_other)
        cat_sel <- c('Dairy/Fatty', 'Meat', 'Fruits/Vegetables', 'Cereals/Grains', 'Other')[cat_sel]
        slice_head(ingredients, n=input$num_obs) %>%
            filter(Category.Broad %in% cat_sel)
    })
    
    output$plot <- renderPlotly({
        ingredients_sliced <- slice_ingredients()
        
        if (input$plot_radio == 'histogram') {
            var_sel <- input$var_select_hist
            var_sel_short <- shorten(var_sel)
            gh <- if (input$toggle_colbycat) {
                geom_histogram(
                    aes(ingredients_sliced[[var_sel]], fill=Category.Broad),
                    bins=input$bins,
                    color='black',
                    alpha=0.35
                )
            }
            else {
                geom_histogram(
                    aes(ingredients_sliced[[var_sel]]),
                    bins=input$bins,
                    color='black',
                    fill=input$color
                )
            }
            p <- ggplot(ingredients_sliced) +
                gh +
                labs(
                    title=sprintf('Distribution of %s', var_sel_short),
                    x=sprintf('%s (%s)', var_sel_short, units[var_sel_short]),
                    y='Frequency',
                    fill='Category'
                ) +
                theme_bw()
            ggply <- ggplotly(p)
            tt <- sprintf('Bin Min: %.2f\nFrequency: %d', ggply$x$data[[1]]$x, ggply$x$data[[1]]$y)
        }
        else {
            var_x_sel <- input$var_select_scatter_x
            var_y_sel <- input$var_select_scatter_y
            var_x_sel_short <- shorten(var_x_sel)
            var_y_sel_short <- shorten(var_y_sel)
            gp <- if (input$toggle_colbycat) {
                geom_point(
                    aes(
                        x=ingredients_sliced[[var_x_sel]],
                        y=ingredients_sliced[[var_y_sel]],
                        color=ingredients_sliced$Category.Broad
                    ),
                    alpha=0.35
                )
            }
            else {
                geom_point(
                    aes(x=slice_ingredients()[[var_x_sel]], y=slice_ingredients()[[var_y_sel]]),
                    color=input$color
                )
            }
            p <- ggplot(slice_ingredients()) +
                gp +
                labs(
                    title=sprintf('%s vs. %s', var_y_sel_short, var_x_sel_short),
                    x=sprintf('%s (%s)', var_x_sel_short, units[var_x_sel_short]),
                    y=sprintf('%s (%s)', var_y_sel_short, units[var_y_sel_short]),
                    color='Category'
                ) +
                theme_bw()
            ggply <- ggplotly(p)
            tt <- sprintf('%s: %.2f\n%s: %.2f',
                var_x_sel_short, ggply$x$data[[1]]$x,
                var_y_sel_short, ggply$x$data[[1]]$y
            )
        }
        ggply %>% config(displayModeBar = F) %>% style(text=tt)
    })
    
    output$stats <- renderUI({
        if (input$plot_radio == 'histogram') {
            five_num <- fivenum(slice_ingredients()[[input$var_select_hist]])
            tagList(
                span(strong('Min: '), sprintf('%.2f', five_num[1]), ' | '),
                span(strong('Q1: '), sprintf('%.2f', five_num[2]), ' | '),
                span(strong('Median: '), sprintf('%.2f', five_num[3]), ' | '),
                span(strong('Mean: '), sprintf('%.2f', mean(slice_ingredients()[[input$var_select_hist]])), ' | '),
                span(strong('Q3: '), sprintf('%.2f', five_num[4]), ' | '),
                span(strong('Max: '), sprintf('%.2f', five_num[5]))
            )
        }
        else {
            span(strong('Correlation: '), sprintf('%.2f', cor(x=slice_ingredients()[[input$var_select_scatter_x]], y=slice_ingredients()[[input$var_select_scatter_y]])))
        }
    })
    
    # output$stats <- renderText({
    #     if (input$plot_radio == 'histogram') {
    #         five_num <- fivenum(slice_ingredients()[[input$var_select_hist]])
    #         sprintf('Min: %.2f\nQ1: %.2f\nMedian: %.2f\nMean: %.2f\nQ3: %.2f\nMax: %.2f',
    #             five_num[1],
    #             five_num[2],
    #             five_num[3],
    #             mean(slice_ingredients()[[input$var_select_hist]]),
    #             five_num[4],
    #             five_num[5]
    #         )
    #     }
    #     else {
    #         sprintf('Correlation: %.2f', cor(x=slice_ingredients()[[input$var_select_scatter_x]], y=slice_ingredients()[[input$var_select_scatter_y]]))
    #     }
    # })
    
    # output$table <- renderUI({
    #     stats <- if (input$plot_radio == 'histogram') {
    #         rnd <- function(num) {
    #             return(sprintf('%.2f', num))
    #         }
    #         five_num <- fivenum(slice_ingredients()[[input$var_select_hist]])
    #         tribble(
    #           ~min, ~q1, ~median, ~mean, ~q3, ~max,
    #           rnd(five_num[1]), rnd(five_num[2]), rnd(five_num[3]), rnd(mean(slice_ingredients()[[input$var_select_hist]])), rnd(five_num[4]), rnd(five_num[5])
    #         )
    #     }
    #     else {
    #         tibble(correlation=c(cor(x=slice_ingredients()[[input$var_select_scatter_x]], y=slice_ingredients()[[input$var_select_scatter_y]])))
    #     }
    #     stats_cols <- if (input$plot_radio == 'histogram') {
    #         tibble(
    #             fieldName=c('min', 'q1', 'median', 'mean', 'q3', 'max'),
    #             name=c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max'),
    #             key=fieldName
    #         )
    #     }
    #     else {
    #         tibble(
    #             fieldName=c('correlation'),
    #             name=c('Correlation'),
    #             key=fieldName
    #         )
    #     }
    #     DetailsList(items=stats, columns=stats_cols)
    # })
}

shinyApp(ui=ui, server=server)