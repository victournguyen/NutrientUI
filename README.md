# NutrientUI
A UI for visualizing the CORGIS [ingredients](https://think.cs.vt.edu/corgis/csv/ingredients/) data set. Access at [https://victournguyen.shinyapps.io/NutrientUI/](https://victournguyen.shinyapps.io/NutrientUI/).

This R application uses Shiny and shiny.fluent, which integrates Microsoft's Fluent UI into Shiny, to display a reactive plot with multiple options, ranging from variable selection to color input. Other libraries used include tidyverse and Plotly.

**NOTE:** The word "category" in the code comments and UI refers to the `Category.Broad` column derived from the original `Category` column.