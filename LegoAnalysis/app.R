#
# flexdashboard example https://colorado.rstudio.com/rsc/connect/#/apps/2279/access
# shiny example https://colorado.rstudio.com/rsc/connect/#/apps/2293/access
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analysis of Lego Set Part Count"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 1953,
                        max = 2017,
                        value = 1978,
                        sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$distPlot <- renderPlot({
        setsData <-read_csv(("sets.csv"))
        parts_in_year <- setsData %>% 
            filter(year == input$year) %>% 
            select(year, num_parts, name, set_num) %>% 
            arrange(desc(num_parts))
         # generate violin plot based on input$year from ui.R
        ggplot(parts_in_year, aes(year, num_parts)) + 
            geom_violin(fill=c("#75AADB")) + 
            scale_x_discrete(limits = input$year) +
            # scale_y_continuous(breaks=seq(0,max(parts_in_year$num_parts), 100)) +
            geom_boxplot(width=0.1) +
            theme_minimal() + 
            labs(x="", y="Number of Parts in Set", title="Lego Set Part Count Distribution") +
            coord_flip()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
