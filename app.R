library(shiny)
library(shiny.fluent)
library(tidyverse)
library(stringr)
library(ggplot2)
library(plotly)
library(colourpicker)

# Read in data set
ingredients <- read_csv('ingredients.csv')

# Function to simplify column name for display
shorten <- function(col) {
    return(str_replace_all(col, '^.*\\.', ''))
}
# Create options for selecting variables
cols <- colnames(ingredients)
var_choices <- tibble(
    key=cols[4:length(cols)],
    text=sapply(key, shorten)
)
# List of units for the variables
units <- c('Alpha Carotene'='mcg', 'Beta Carotene'='mcg', 'Beta Cryptoxanthin'='mcg', 'Carbohydrate'='g', 'Cholesterol'='mg', 'Choline'='mg', 'Fiber'='g', 'Lutein and Zeaxanthin'='mcg', 'Lycopene'='mcg', 'Niacin'='mg', 'Protein'='g', 'Retinol'='mcg', 'Riboflavin'='mg', 'Selenium'='mcg', 'Sugar Total'='g', 'Thiamin'='mg', 'Water'='g', 'Monosaturated Fat'='g', 'Polysaturated Fat'='g', 'Saturated Fat'='g', 'Total Lipid'='g', 'Calcium'='mg', 'Copper'='mg', 'Iron'='mg', 'Magnesium'='mg', 'Phosphorus'='mg', 'Potassium'='mg', 'Sodium'='mg', 'Zinc'='mg', 'Vitamin A - RAE'='mcg', 'Vitamin B12'='mcg', 'Vitamin B6'='mg', 'Vitamin C'='mg', 'Vitamin E'='mg', 'Vitamin K'='mcg')

# Read broad category data
groups <- read_csv('category_broad.csv')
# Returns which broad category the category belongs to
add_broad_cat <- function(cat) {
    for (g in colnames(groups)) {
        if (cat %in% groups[[g]]) {
            return(g)
        }
    }
}
# Create broad category column
ingredients$Category.Broad <- ingredients$Category %>%
    sapply(add_broad_cat)%>%
    as.factor()

# Arrange UI
ui <- fluentPage(
    # Inline styling
    tags$style(HTML('
        body {
            background-color: #faf9f8;
        }
        
        .checkbox-not-first {
            margin-top: 5px;
        }
    ')),
    # Title
    h1('Nutrients in Food Ingredients'),
    div(
        style='display: flex;',
        # Side panel
        div(
            class='card ms-depth-8 ms-sm4 ms-xl4',
            style='padding: 10px; margin-right: 10px; background-color: white;',
            ChoiceGroup.shinyInput(
                inputId='plot_radio',
                label='Select Plot Type',
                options=tibble(
                    key=c('histogram', 'scatter'),
                    text=c('Histogram', 'Scatter Plot')
                ),
                value='histogram'
            ),
            # Histogram options
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
            # Scatter plot options
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
            # Category selection
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
            # Color options
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
        # Main panel
        div(
            class='card ms-depth-8 ms-sm8 ms-xl8',
            style='display: flex; flex-direction: column; padding: 10px; background-color: white;',
            plotlyOutput(outputId='plot', height='100%'),
            uiOutput(outputId='stats')
        )
    )
)

# Handle input and output
server <- function(input, output) {
    # Filter and take first n rows of ingredients
    slice_ingredients <- reactive({
        cat_sel <- c(input$check_dairy, input$check_meat, input$check_fruits, input$check_cereals, input$check_other)
        cat_sel <- c('Dairy/Fatty', 'Meat', 'Fruits/Vegetables', 'Cereals/Grains', 'Other')[cat_sel]
        slice_head(ingredients, n=input$num_obs) %>%
            filter(Category.Broad %in% cat_sel)
    })
    
    # Set the plot
    output$plot <- renderPlotly({
        # Get filtered & sliced data
        ingredients_sliced <- slice_ingredients()
        
        # Histogram or scatter plot based on user input
        if (input$plot_radio == 'histogram') {
            # Get selected variable
            var_sel <- input$var_select_hist
            var_sel_short <- shorten(var_sel)
            gh <- if (input$toggle_colbycat) {
                # Histogram colored by category
                geom_histogram(
                    aes(ingredients_sliced[[var_sel]], fill=Category.Broad),
                    bins=input$bins,
                    color='black',
                    alpha=0.35
                )
            }
            else {
                # Histogram colored by input
                geom_histogram(
                    aes(ingredients_sliced[[var_sel]]),
                    bins=input$bins,
                    color='black',
                    fill=input$color
                )
            }
            # Add labels and theme
            p <- ggplot(ingredients_sliced) +
                gh +
                labs(
                    title=sprintf('Distribution of %s', var_sel_short),
                    x=sprintf('%s (%s)', var_sel_short, units[var_sel_short]),
                    y='Frequency',
                    fill='Category'
                ) +
                theme_bw()
            # Make ggplot into Plotly figure
            ggply <- ggplotly(p)
            # Format tooltip (hover text)
            tt <- sprintf('Bin Min: %.2f\nFrequency: %d', ggply$x$data[[1]]$x, ggply$x$data[[1]]$y)
        }
        else {
            # Get selected variables
            var_x_sel <- input$var_select_scatter_x
            var_y_sel <- input$var_select_scatter_y
            var_x_sel_short <- shorten(var_x_sel)
            var_y_sel_short <- shorten(var_y_sel)
            gp <- if (input$toggle_colbycat) {
                # Scatter plot colored by category
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
                # Scatter plot colored by input
                geom_point(
                    aes(x=ingredients_sliced[[var_x_sel]], y=ingredients_sliced[[var_y_sel]]),
                    color=input$color
                )
            }
            # Add labels and theme
            p <- ggplot(ingredients_sliced) +
                gp +
                labs(
                    title=sprintf('%s vs. %s', var_y_sel_short, var_x_sel_short),
                    x=sprintf('%s (%s)', var_x_sel_short, units[var_x_sel_short]),
                    y=sprintf('%s (%s)', var_y_sel_short, units[var_y_sel_short]),
                    color='Category'
                ) +
                theme_bw()
            # Make ggplot into Plotly figure
            ggply <- ggplotly(p)
            # Format tooltip (hover text)
            tt <- sprintf('%s: %.2f\n%s: %.2f',
                var_x_sel_short, ggply$x$data[[1]]$x,
                var_y_sel_short, ggply$x$data[[1]]$y
            )
        }
        # Set plot options
        ggply %>% config(displayModeBar = F) %>% style(text=tt)
    })
    
    # Set descriptive statistics for plot
    output$stats <- renderUI({
        # Get filtered & sliced data
        ingredients_sliced <- slice_ingredients()
        
        # Five-number summary + mean for histogram, correlation coefficient for scatter plot
        if (input$plot_radio == 'histogram') {
            five_num <- fivenum(ingredients_sliced[[input$var_select_hist]])
            tagList(
                span(strong('Min: '), sprintf('%.2f', five_num[1]), ' | '),
                span(strong('Q1: '), sprintf('%.2f', five_num[2]), ' | '),
                span(strong('Median: '), sprintf('%.2f', five_num[3]), ' | '),
                span(strong('Mean: '), sprintf('%.2f', mean(ingredients_sliced[[input$var_select_hist]])), ' | '),
                span(strong('Q3: '), sprintf('%.2f', five_num[4]), ' | '),
                span(strong('Max: '), sprintf('%.2f', five_num[5]))
            )
        }
        else {
            span(strong('Correlation: '), sprintf('%.2f', cor(x=ingredients_sliced[[input$var_select_scatter_x]], y=ingredients_sliced[[input$var_select_scatter_y]])))
        }
    })
}

# Create application
shinyApp(ui=ui, server=server)