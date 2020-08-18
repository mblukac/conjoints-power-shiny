#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conjoint Experiments: Statistical Power Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("num_respondents",
                        "Number of subjects:",
                        min = 1,
                        max = 5000,
                        value = 2000),
            sliderInput("num_tasks",
                        "Number of trials:",
                        min = 1,
                        max = 20,
                        value = 5),
            sliderInput("true_coef",
                        "Expected effect size (%):",
                        min = 0.01,
                        max = 0.2,
                        value = 0.02),
            sliderInput("num_lvls",
                        "Number of levels:",
                        min = 2,
                        max = 30,
                        value = 4)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
            textOutput("summary"),
            
            textOutput("predpwr"),
            
            #plotly::plotlyOutput("heatplot")
            plotOutput("heatplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #output$distPlot <- renderPlot({
    #    # generate bins based on input$bins from ui.R
    #    x    <- faithful[, 2]
    #    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    #    # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
    
    output$summary <- renderText({
        paste("You selected a conjoint design with", input$num_respondents, 
              "respondents,", input$num_tasks, "trials,", 
              input$num_lvls, "levels.", 
              "Your expected effect size is", input$true_coef, ".")
    })
    
    pred_pwr <- reactive({
        c <- read.csv("glm_coefs.csv")
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1] * input$num_respondents + 
            c[3, 1] * input$num_tasks + 
            c[4, 1] * input$true_coef + 
            c[5, 1] * input$num_lvls +
            c[6, 1] * input$num_respondents * input$num_tasks +
            c[7, 1] * input$num_respondents * input$true_coef +
            c[8, 1] * input$num_respondents * input$num_lvls +
            c[9, 1] * input$num_tasks *  input$true_coef +
            c[10, 1] * input$num_tasks * input$num_lvls +
            c[11, 1] * input$true_coef * input$num_lvls +
            c[12, 1] * input$num_respondents * input$num_tasks * input$true_coef +
            c[13, 1] * input$num_respondents * input$num_tasks * input$num_lvls +
            c[14, 1] * input$num_respondents * input$true_coef * input$num_lvls +
            c[15, 1] * input$num_tasks * input$true_coef * input$num_lvls +
            c[16, 1] * input$num_respondents * input$num_tasks * input$true_coef * input$num_lvls
        
        o_predicted_pwr <- exp(lo_predicted_pwr)
        
        paste0(round(o_predicted_pwr / (1 + o_predicted_pwr), 2) * 100, "%")
    })
    
    output$predpwr <- renderText({
        
        paste("Predicted power is", pred_pwr())
    })
    
    output$heatplot <- renderPlot({ #plotly::renderPlotly({
        c <- read.csv("glm_coefs.csv")
        new <- expand.grid(num_respondents = seq(500, 5000, 10),
                           num_tasks = seq(1, 20, 0.1),
                           true_coef = input$true_coef, 
                           num_lvls = input$num_lvls)
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1] * new$num_respondents + 
            c[3, 1] * new$num_tasks + 
            c[4, 1] * new$true_coef + 
            c[5, 1] * new$num_lvls +
            c[6, 1] * new$num_respondents * new$num_tasks +
            c[7, 1] * new$num_respondents * new$true_coef +
            c[8, 1] * new$num_respondents * new$num_lvls +
            c[9, 1] * new$num_tasks *  new$true_coef +
            c[10, 1] * new$num_tasks * new$num_lvls +
            c[11, 1] * new$true_coef * new$num_lvls +
            c[12, 1] * new$num_respondents * new$num_tasks * new$true_coef +
            c[13, 1] * new$num_respondents * new$num_tasks * new$num_lvls +
            c[14, 1] * new$num_respondents * new$true_coef * new$num_lvls +
            c[15, 1] * new$num_tasks * new$true_coef * new$num_lvls +
            c[16, 1] * new$num_respondents * new$num_tasks * new$true_coef * new$num_lvls
        o_predicted_pwr <- exp(lo_predicted_pwr)
        new$pred_sig <- o_predicted_pwr / (1 + o_predicted_pwr)
        
        plot_input <- data.frame(num_respondents = input$num_respondents,
                                 num_tasks = input$num_tasks,
                                 pred_sig = 1)
        #print(
        #    plotly::ggplotly(
        ggplot(new, aes(num_respondents, num_tasks, fill = pred_sig)) +
            geom_raster(interpolate = F) +
            coord_cartesian(expand = FALSE) +
            scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000),
                               labels = c("1k", "2k", "3k", "4k", "5k")) +
            scale_fill_gradient2(
                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
                limits = c(0, 1),
                midpoint = 0.8,
                high = "#0D0887FF",   # or  scales::muted("darkblue")
                low = "#E16462FF"     # and scales::muted("red")
            ) +
            theme_bw() +
            theme(
                axis.text = element_text(size = 12),
                #axis.title = element_text(size = 8),
                legend.position = "bottom",
                legend.title = element_blank(),
                #legend.text = element_text(size = 6)
            ) +
            xlab("Respondents") + ylab("Tasks") +
            guides(fill = guide_colourbar(barwidth = 20,
                                          barheight = 0.5,
                                          frame.colour = "black",
                                          frame.linewidth = 1,
                                          ticks.colour = "black",
                                          ticks.linewidth = 1)) + 
            geom_segment(aes(x = 500, y = input$num_tasks, 
                             xend = input$num_respondents, 
                             yend = input$num_tasks),
                         linetype = 2) + 
            geom_segment(aes(x = input$num_respondents, y = 1, 
                             xend = input$num_respondents, 
                             yend = input$num_tasks),
                         linetype = 2) +
            geom_point(data = plot_input, aes(num_respondents, num_tasks),
                       size = 3.5) +
            annotate("text", x = (input$num_respondents + 100),
                     y = (input$num_tasks + .5),
                     label = paste(pred_pwr()))
        #    )
        #)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
