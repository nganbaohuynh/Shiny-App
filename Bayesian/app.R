#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
parameter_tabs <- tabsetPanel(
    id = "params",
    type = "hidden",
    tabPanel("2",
             numericInput("A1", "P(A1)",min = 0, value = 0.5, max = 1),
             numericInput("A2", "P(A2)",min = 0, value = 0.5, max = 1),
             numericInput("B_A1", "P(B|A1)",min = 0, value = 0.5, max = 1),
             numericInput("B_A2", "P(B|A2)",min = 0, value = 0.5, max = 1)
    ),
    tabPanel("3", 
             numericInput("A1", "P(A1)",min = 0, value = 0.1, max = 1),
             numericInput("A2", "P(A2)",min = 0, value = 0.4, max = 1),
             numericInput("A3", "P(A3)",min = 0, value = 0.5, max = 1),
             numericInput("B_A1", "P(B|A1)",min = 0, value = 0.5, max = 1),
             numericInput("B_A2", "P(B|A2)",min = 0, value = 0.5, max = 1),
             numericInput("B_A3", "P(B|A3)",min = 0, value = 0.5, max = 1)
    ),
    tabPanel("4",
             numericInput("A1", "P(A1)",min = 0, value = 0.2, max = 1),
             numericInput("A2", "P(A2)",min = 0, value = 0.3, max = 1),
             numericInput("A3", "P(A3)",min = 0, value = 0.1, max = 1),
             numericInput("A4", "P(A4)",min = 0, value = 0.4, max = 1),
             numericInput("B_A1", "P(B|A1)",min = 0, value = 0.5, max = 1),
             numericInput("B_A2", "P(B|A2)",min = 0, value = 0.5, max = 1),
             numericInput("B_A3", "P(B|A3)",min = 0, value = 0.5, max = 1),
             numericInput("B_A4", "P(B|A4)",min = 0, value = 0.5, max = 1)
    ),
    tabPanel("5",
             numericInput("A1", "P(A1)",min = 0, value = 0.2, max = 1),
             numericInput("A2", "P(A2)",min = 0, value = 0.3, max = 1),
             numericInput("A3", "P(A3)",min = 0, value = 0.1, max = 1),
             numericInput("A4", "P(A4)",min = 0, value = 0.4, max = 1),
             numericInput("A5", "P(A5)",min = 0, value = 0.4, max = 1),
             numericInput("B_A1", "P(B|A1)",min = 0, value = 0.5, max = 1),
             numericInput("B_A2", "P(B|A2)",min = 0, value = 0.5, max = 1),
             numericInput("B_A3", "P(B|A3)",min = 0, value = 0.5, max = 1),
             numericInput("B_A4", "P(B|A4)",min = 0, value = 0.5, max = 1),
             numericInput("B_A5", "P(B|A5)",min = 0, value = 0.5, max = 1)
             
    )
)
# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Bayesian"),
        sidebarLayout(
            sidebarPanel(
                selectInput("k", "How many events (k) are in the sample space?", 
                            choices = c("2", "3", "4", "5")
                ),
                parameter_tabs
            ),
            mainPanel(
               tableOutput("Table")
            )
        )
    )
# Define server logic 
server <- function(input, output, session) {
    observeEvent(input$k, {
        updateTabsetPanel(inputId = "params", selected = input$k)
    }) 
    calculation <- reactive({
        A1 <- input$A1
        A2 <- input$A2
        A3 <- input$A3
        A4 <- input$A4
        A5 <- input$A5
        B_A1 <- input$B_A1
        B_A2 <- input$B_A2
        B_A3 <- input$B_A3
        B_A4 <- input$B_A4
        B_A5 <- input$B_A5
        
        switch(input$k,
               "2" = data.frame(c("P(A1|B)","P(A2|B)"),
                              c((A1*B_A1)/(A1*B_A1+A2*B_A2),
                                (A2*B_A2)/(A1*B_A1+A2*B_A2))),
               "3" = data.frame(c("P(A1|B)","P(A2|B)","P(A3|B)"),
                              c((A1*B_A1)/(A1*B_A1+A2*B_A2+A3*B_A3),
                                (A2*B_A2)/(A1*B_A1+A2*B_A2+A3*B_A3),
                                (A3*B_A3)/(A1*B_A1+A2*B_A2+A3*B_A3))),
               "4" = data.frame(c("P(A1|B)","P(A2|B)","P(A3|B)","P(A4|B)"),
                              c((A1*B_A1)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4),
                                (A2*B_A2)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4),
                                (A3*B_A3)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4),
                                (A4*B_A4)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4))),
               "5" = data.frame(c("P(A1|B)","P(A2|B)","P(A3|B)","P(A4|B)","P(A5|B)"),
                              c((A1*B_A1)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4+A5*B_A5),
                                (A2*B_A2)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4+A5*B_A5),
                                (A3*B_A3)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4+A5*B_A5),
                                (A4*B_A4)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4+A5*B_A5),
                                (A5*B_A5)/(A1*B_A1+A2*B_A2+A3*B_A3+A4*B_A4+A5*B_A5)))
        )
    })
    output$Table <- renderTable({
        data <- calculation()
        colnames(data) = c("Probability of Ak given B", "Result")
        data
        })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
