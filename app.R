#
# This is a Shiny app for the project:
# Stefanelli, A. and Lukac, M.: Subjects, trials, attributes: 
#   statistical power in conjoint experiments
#

# 0. Libraries ----------------------------------------------------------------
library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(ggrepel)

# 1. Shiny App ----------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("yeti"),

    # Application title
    titlePanel("Conjoint experiments: Power Analysis"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                box(width = 12, title = "", 
                    splitLayout(
                        cellWidths = c("25%", "75%"),
                        textInput("num_respondents_text", "Respondents",
                                  value = "2000"),
                        sliderInput("num_respondents",
                                    "",
                                    min = 500,
                                    max = 5000,
                                    value = 2000,
                                    step = 100,
                                    width = '90%',
                                    ticks = F)
                    )
                )
            ),
            
            fluidRow(
                box(width = 12, title = "",
                    splitLayout(
                        cellWidths = c("25%", "75%"),
                        textInput("num_tasks_text", "Tasks",
                                  value = "5"),
                        sliderInput("num_tasks",
                                    "",
                                    min = 1,
                                    max = 20,
                                    value = 5,
                                    width = "90%",
                                    ticks = F)
                    )
                )
            ),
            
            fluidRow(
                box(width = 12, title = "",
                    splitLayout(
                        cellWidths = c("25%", "75%"),
                        textInput("true_coef_text", "Effect size (%)",
                                  value = "0.02"),
                        sliderInput("true_coef",
                                    "",
                                    min = 0.01,
                                    max = 0.2,
                                    value = 0.02,
                                    step = 0.01,
                                    width = "90%",
                                    ticks = F)
                    )
                )
            ),
            
            fluidRow(
                box(width = 12, title = "",
                    splitLayout(
                        cellWidths = c("25%", "75%"),
                        textInput("num_lvls_text", "Variable levels",
                                  value = "4"),
                        sliderInput("num_lvls",
                                    "",
                                    min = 2,
                                    max = 30,
                                    value = 4,
                                    width = "90%",
                                    ticks = F)
                    )
                )
            ),
            
            fluidRow(
                box(width = 12, title = "",
                    splitLayout(
                        cellWidths = c("50%", "50%"),
                        actionButton("show_about", "About",
                                     width = "150px"),
                        actionButton("show_info", "Help",
                                     width = "150px")
                    )
                )
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
            #textOutput("summary"),
            
            textOutput("predpwr"),
            
            br(),
            
            #plotly::plotlyOutput("heatplot")
            plotOutput("heatplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ### Action Buttons
    # About
    observeEvent(input$show_about, {
        text_about <- "This is an online power calculator for 
        conjoint experiments based on work of 
        <a href=\"https://albertostefanelli.com\">Alberto Stefanelli</a> 
        and 
        <a href=\"https://mblukac.github.io\">Martin Lukac</a>."
        showModal(modalDialog(HTML(text_about), title = 'About'))
    })
    
    # Info
    observeEvent(input$show_info, {
        text_about <- "<b>Respondents:</b> Number of people you are going 
        to recruit.<br><br>
        <b>Tasks:</b> Number of tasks you are going to give them -- 
        i.e. conjoint trials<br><br>
        <b>Effect size:</b> The expected effect size in %<br><br>
        <b>Variable levels:</b> Number of levels of your categorical 
        variable -- i.e. gender (male vs. female) has two categories"
        showModal(modalDialog(HTML(text_about), title = 'Help'))
    })

    ### Connect text fields to sliders
    # Respondents
    observe({
        updateTextInput(
            session = session,
            inputId = "num_respondents_text",
            value = input$num_respondents
        )
    })
    observe({
        updateSliderInput(
            session = session,
            inputId = "num_respondents",
            value = input$num_respondents_text
        )
    })
    
    # Tasks
    observe({
        updateTextInput(
            session = session,
            inputId = "num_tasks_text",
            value = input$num_tasks
        )
    })
    observe({
        updateSliderInput(
            session = session,
            inputId = "num_tasks",
            value = input$num_tasks_text
        )
    })
    
    # Coefficient
    observe({
        updateTextInput(
            session = session,
            inputId = "true_coef_text",
            value = input$true_coef
        )
    })
    observe({
        updateSliderInput(
            session = session,
            inputId = "true_coef",
            value = input$true_coef_text
        )
    })
    
    # Levels
    observe({
        updateTextInput(
            session = session,
            inputId = "num_lvls_text",
            value = input$num_lvls
        )
    })
    observe({
        updateSliderInput(
            session = session,
            inputId = "num_lvls",
            value = input$num_lvls_text
        )
    })
    
    ### Set up outputs
    #output$summary <- renderText({
    #    paste("You selected a conjoint design with", input$num_respondents, 
    #          "respondents,", input$num_tasks, "trials,", 
    #          input$num_lvls, "levels.", 
    #          "Your expected effect size is", input$true_coef, ".")
    #})
    
    pred_pwr <- reactive({
        c <- read.csv("glm_coefs.csv")
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1] * input$num_respondents + 
            c[3, 1] * input$num_tasks + 
            c[4, 1] * input$true_coef + 
            c[5, 1] * input$num_lvls +
            c[6, 1]  * input$num_respondents ^ 2 + 
            c[7, 1]  * input$num_tasks ^ 2 +
            c[8, 1]  * input$true_coef ^ 2 +
            c[9, 1]  * input$num_lvls ^ 2 + 
            c[10, 1] * input$num_respondents ^ 3 + 
            c[11, 1] * input$num_tasks ^ 3 +
            c[12, 1] * input$true_coef ^ 3 +
            c[13, 1] * input$num_lvls ^ 3 +
            c[14, 1] * input$num_respondents * input$num_tasks +
            c[15, 1] * input$num_respondents * input$true_coef +
            c[16, 1] * input$num_respondents * input$num_lvls +
            c[17, 1] * input$num_tasks *  input$true_coef +
            c[18, 1] * input$num_tasks * input$num_lvls +
            c[19, 1] * input$true_coef * input$num_lvls +
            c[20, 1] * input$num_respondents * input$num_tasks * input$true_coef +
            c[21, 1] * input$num_respondents * input$num_tasks * input$num_lvls +
            c[22, 1] * input$num_respondents * input$true_coef * input$num_lvls +
            c[23, 1] * input$num_tasks * input$true_coef * input$num_lvls +
            c[24, 1] * input$num_respondents * input$num_tasks * input$true_coef * input$num_lvls
        
        o_predicted_pwr <- exp(lo_predicted_pwr)
        
        paste0(round(o_predicted_pwr / (1 + o_predicted_pwr), 2) * 100, "%")
    })
    
    output$predpwr <- renderText({
        
        paste0("Predicted statistical power for the specificed design is ", 
              pred_pwr(), ".")
    })
    
    output$heatplot <- renderPlot({
        c <- read.csv("glm_coefs.csv")
        new <- expand.grid(num_respondents = seq(500, 5000, 50),
                           num_tasks = seq(1, 20, 0.5),
                           true_coef = input$true_coef, 
                           num_lvls = input$num_lvls)
        
        # Rewrite this part to predict values only within the bounds
        # of the simulation experiment
        #    - num_resp: 500-3000
        #    - num_tasks: 1-9
        # and change the plot accordingly
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1] * new$num_respondents + 
            c[3, 1] * new$num_tasks + 
            c[4, 1] * new$true_coef + 
            c[5, 1] * new$num_lvls +
            c[6, 1]  * new$num_respondents ^ 2 + 
            c[7, 1]  * new$num_tasks ^ 2 +
            c[8, 1]  * new$true_coef ^ 2 +
            c[9, 1]  * new$num_lvls ^ 2 + 
            c[10, 1] * new$num_respondents ^ 3 + 
            c[11, 1] * new$num_tasks ^ 3 +
            c[12, 1] * new$true_coef ^ 3 +
            c[13, 1] * new$num_lvls ^ 3 +
            c[14, 1] * new$num_respondents * new$num_tasks +
            c[15, 1] * new$num_respondents * new$true_coef +
            c[16, 1] * new$num_respondents * new$num_lvls +
            c[17, 1] * new$num_tasks *  new$true_coef +
            c[18, 1] * new$num_tasks * new$num_lvls +
            c[19, 1] * new$true_coef * new$num_lvls +
            c[20, 1] * new$num_respondents * new$num_tasks * new$true_coef +
            c[21, 1] * new$num_respondents * new$num_tasks * new$num_lvls +
            c[22, 1] * new$num_respondents * new$true_coef * new$num_lvls +
            c[23, 1] * new$num_tasks * new$true_coef * new$num_lvls +
            c[24, 1] * new$num_respondents * new$num_tasks * new$true_coef * new$num_lvls
        
        o_predicted_pwr <- exp(lo_predicted_pwr)
        new$pred_sig <- o_predicted_pwr / (1 + o_predicted_pwr)
        
        plot_input <- data.frame(num_respondents = input$num_respondents,
                                 num_tasks = input$num_tasks,
                                 pred_sig = 1,
                                 power = pred_pwr())
        
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
                axis.title = element_text(size = 14),
                legend.position = "bottom",
                legend.title = element_text(size = 14, hjust = 1,
                                            margin = margin(0, 15, 0, 0)),
                legend.text = element_text(size = 12)
            ) +
            xlab("Respondents") + ylab("Tasks") +
            guides(fill = guide_colourbar(barwidth = 20,
                                          barheight = 0.5,
                                          frame.colour = "black",
                                          frame.linewidth = 1,
                                          ticks.colour = "black",
                                          ticks.linewidth = 1)) + 
            labs(fill = "Power") +
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
            geom_text_repel(data = plot_input, aes(label = power),
                            size = 5, 
                            box.padding = 0.5,
                            min.segment.length = 1)
    
        
    },
    width = 550,
    height = 450)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
