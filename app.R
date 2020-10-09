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
ui <- fluidPage(
    #theme = shinytheme("yeti"),
    theme = shinytheme("cerulean"),
    # Application title
    titlePanel("Conjoint experiments: Power Analysis Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                box(width = 12, title = "", 
                    splitLayout(
                        cellWidths = c("25%", "75%"),
                        textInput("num_respondents_text", "Respondents",
                                  value = "1000"),
                        sliderInput("num_respondents",
                                    "",
                                    min = 500,
                                    max = 3000,
                                    value = 1000,
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
                                  value = "3"),
                        sliderInput("num_tasks",
                                    "",
                                    min = 1,
                                    max = 9,
                                    value = 3,
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
                                  value = "0.05"),
                        sliderInput("true_coef",
                                    "",
                                    min = 0.01,
                                    max = 0.2,
                                    value = 0.05,
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
                                  value = "5"),
                        sliderInput("num_lvls",
                                    "",
                                    min = 2,
                                    max = 30,
                                    value = 5,
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
        <a href=\"https://mblukac.github.io\">Martin Lukac</a>. A paper 
        with full methodological detail will be published soon. Until then,
        please feel free to get in touch for methodological inquiries.<br><br>
        Cite as Lukac, M. & Stefanelli, A. (2020). Conjoint experiments:
        Power Analysis Tool. Retrieved from 
        https://mblukac.shinyapps.io/conjoints-power-shiny/
        <br><br><hr><br>
        Copyright (c) 2020 Martin Lukac and Alberto Stefanelli
        <br>
        This work is distributed under MIT Licence. See the file 
        <a href=\"https://github.com/mblukac/conjoints-power-shiny/LICENSE.txt\">
        LICENSE.txt</a> for details."
        showModal(modalDialog(HTML(text_about), title = 'About'))
    })
    
    # Info
    observeEvent(input$show_info, {
        text_about <- "<b>Respondents:</b> Number of respondents that are going
        to answer the survey.<br><br>
        <b>Tasks:</b> Number of tasks each respondent will receive (sometimes called
        trials or selection tasks).<br><br>
        <b>Effect size:</b> The expected effect size in %. This is the expected 
        Average Marginal Component Effect (see <a href=\"https://doi.org/10.1093/pan/mpt024\">Hainmueller et al. 2014</a>)<br><br>
        <b>Variable levels:</b> Number of levels of your categorical 
        variable — i.e. gender (male vs. female) has two categories. 
        Use the number of levels of the attribute with the highest number of levels
        to obtain a power of the experimental design as a whole (lowest threashold)."
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
    
    pred_pwr <- reactive({
        c <- read.csv("glm_coefs.csv")
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1]  * log(input$num_respondents) + 
            c[3, 1]  * log(input$num_tasks) + 
            c[4, 1]  * log(input$true_coef) + 
            c[5, 1]  * log(input$num_lvls) +
            c[6, 1]  * I(log(input$num_respondents) ^ 2) + 
            c[7, 1]  * I(log(input$num_tasks) ^ 2) + 
            c[8, 1]  * I(log(input$true_coef) ^ 2) + 
            c[9, 1]  * I(log(input$num_lvls) ^ 2) +
            c[10, 1] * I(log(input$num_respondents) ^ 3) + 
            c[11, 1] * I(log(input$num_tasks) ^ 3) + 
            c[12, 1] * I(log(input$true_coef) ^ 3) + 
            c[13, 1] * I(log(input$num_lvls) ^ 3) +
            c[14, 1] * I(log(input$num_respondents) ^ 4) + 
            c[15, 1] * I(log(input$num_tasks) ^ 4) + 
            c[16, 1] * I(log(input$true_coef) ^ 4) + 
            c[17, 1] * I(log(input$num_lvls) ^ 4) +
            c[18, 1] * log(input$num_respondents) * log(input$num_tasks) +
            c[19, 1] * log(input$num_respondents) * log(input$true_coef) +
            c[20, 1] * log(input$num_respondents) * log(input$num_lvls) +
            c[21, 1] * log(input$num_tasks) * log(input$true_coef) +
            c[22, 1] * log(input$num_tasks) * log(input$num_lvls) +
            c[23, 1] * log(input$true_coef) * log(input$num_lvls) +
            c[24, 1] * log(input$num_respondents) * log(input$num_tasks) * log(input$true_coef) +
            c[25, 1] * log(input$num_respondents) * log(input$num_tasks) * log(input$num_lvls) +
            c[26, 1] * log(input$num_respondents) * log(input$true_coef) * log(input$num_lvls) +
            c[27, 1] * log(input$num_tasks) * log(input$true_coef) * log(input$num_lvls) +
            c[28, 1] * log(input$num_respondents) * log(input$num_tasks) * log(input$true_coef) * log(input$num_lvls)
        
        o_predicted_pwr <- exp(lo_predicted_pwr)
        
        paste0(round(o_predicted_pwr / (1 + o_predicted_pwr), 2) * 100, "%")
    })
    
    output$predpwr <- renderText({
        
        paste0("Predicted statistical power for the specificed design is ", 
              pred_pwr(), ".")
    })
    
    output$heatplot <- renderPlot({
        c <- read.csv("glm_coefs.csv")
        new <- expand.grid(num_respondents = seq(500, 3000, 25),
                           num_tasks = seq(1, 9, 0.25),
                           true_coef = input$true_coef, 
                           num_lvls = input$num_lvls)
        
        lo_predicted_pwr <- c[1, 1] + 
            c[2, 1]  * log(new$num_respondents) + 
            c[3, 1]  * log(new$num_tasks) + 
            c[4, 1]  * log(new$true_coef) + 
            c[5, 1]  * log(new$num_lvls) +
            c[6, 1]  * I(log(new$num_respondents) ^ 2) + 
            c[7, 1]  * I(log(new$num_tasks) ^ 2) + 
            c[8, 1]  * I(log(new$true_coef) ^ 2) + 
            c[9, 1]  * I(log(new$num_lvls) ^ 2) +
            c[10, 1] * I(log(new$num_respondents) ^ 3) + 
            c[11, 1] * I(log(new$num_tasks) ^ 3) + 
            c[12, 1] * I(log(new$true_coef) ^ 3) + 
            c[13, 1] * I(log(new$num_lvls) ^ 3) +
            c[14, 1] * I(log(new$num_respondents) ^ 4) + 
            c[15, 1] * I(log(new$num_tasks) ^ 4) + 
            c[16, 1] * I(log(new$true_coef) ^ 4) + 
            c[17, 1] * I(log(new$num_lvls) ^ 4) +
            c[18, 1] * log(new$num_respondents) * log(new$num_tasks) +
            c[19, 1] * log(new$num_respondents) * log(new$true_coef) +
            c[20, 1] * log(new$num_respondents) * log(new$num_lvls) +
            c[21, 1] * log(new$num_tasks) * log(new$true_coef) +
            c[22, 1] * log(new$num_tasks) * log(new$num_lvls) +
            c[23, 1] * log(new$true_coef) * log(new$num_lvls) +
            c[24, 1] * log(new$num_respondents) * log(new$num_tasks) * log(new$true_coef) +
            c[25, 1] * log(new$num_respondents) * log(new$num_tasks) * log(new$num_lvls) +
            c[26, 1] * log(new$num_respondents) * log(new$true_coef) * log(new$num_lvls) +
            c[27, 1] * log(new$num_tasks) * log(new$true_coef) * log(new$num_lvls) +
            c[28, 1] * log(new$num_respondents) * log(new$num_tasks) * log(new$true_coef) * log(new$num_lvls)
        
        o_predicted_pwr <- exp(lo_predicted_pwr)
        new$pred_sig <- as.double(o_predicted_pwr / (1 + o_predicted_pwr))
        
        plot_input <- data.frame(num_respondents = input$num_respondents,
                                 num_tasks = input$num_tasks,
                                 pred_sig = 1,
                                 power = pred_pwr())
        
        ggplot(new, aes(num_respondents, num_tasks, fill = pred_sig)) +
            geom_raster(interpolate = T) +
            coord_cartesian(expand = FALSE) +
            scale_x_continuous(breaks = c(1000, 2000, 3000),
                               labels = c("1k", "2k", "3k")) +
            scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
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
            labs(fill = "Power",
                 caption = paste0("Conjoint design: ", 
                                  input$num_respondents, " respondents, ",
                                  input$num_tasks, " tasks, ",
                                  "assumed effect size ", input$true_coef,
                                  "%, on a variable with ", input$num_lvls,
                                  " levels.")) +
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
