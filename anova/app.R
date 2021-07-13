#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mixlm)

ui <- fluidPage(
    
    # Application title
    titlePanel("Interaction Plot in two-factorial design"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("y11.",
                        "Mean of group 1 level a and group 2 level c: y11.",
                        value = 50),
            numericInput("y12.",
                        "Mean of group 1 level a and group 2 level d: y12.",
                        value = 15),
            numericInput("y21.",
                        "Mean of group 1 level b and group 2 level c: y21.",
                        value = 10),
            numericInput("y22.",
                        "Mean of group 1 level b and group 2 level d: y22.",
                        value = 40),
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Simulated dataframe",tableOutput("data"), htmlOutput("calculation")),
                tabPanel("Interaction Plot",plotOutput("interactionplot")),
                tabPanel("ANOVA",verbatimTextOutput("ANOVA"))
            ))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
        output$data <- renderTable({
            set.seed(16)
            y11. <- input$y11.
            y12. <- input$y12.
            y21. <- input$y21.
            y22. <- input$y22.
            y<-rnorm(n = 16, mean = c(y11.,y12.,y21.,y22.), sd = 1)
            group1 <- rep(letters[1:2], each = 2, time=1)
            group2 <- rep(letters[3:4], length.out = 4)
            data<-data.frame(y,group1,group2)
            colnames(data)<-c("y","group 1","group 2")
            data
    })
        output$calculation <- renderUI({
            y11. <- input$y11.
            y12. <- input$y12.
            y21. <- input$y21.
            y22. <- input$y22.
            set.seed(16)
            y<-rnorm(n = 16, mean = c(y11.,y12.,y21.,y22.), sd = 1)
            group1 <- rep(letters[1:2], each = 2, time=1)
            group2 <- rep(letters[3:4], length.out = 4)
            data<-data.frame(y,group1,group2)
            str0 <- paste('   *Estimation of the parameter based on the input:')
            str1 <- paste('The mean of the population is: μ = (y11.+y12.+y21.+y22.)/4 = ', (y11.+y12.+y21.+y22.)/4)
            str2 <- paste('τ1  = - τ2 = (y11.+y12.)/2 - μ = ', (y11.+y12.)/2 - (y11.+y12.+y21.+y22.)/4)
            str3 <- paste('β1  = - β2 = (y11.+y21.)/2 - μ = ', (y11.+y21.)/2 - (y11.+y12.+y21.+y22.)/4)
            str4 <- paste('(τβ)11  = - (τβ)12 = - (τβ)21 = (τβ)22 = y11. - (y11.+y12.)/2 - (y11.+y21.)/2 + μ = ', y11. - (y11.+y12.)/2 - (y11.+y21.)/2 + (y11.+y12.+y21.+y22.)/4)
            str5 <- paste('   *Estimation of the parameter based on the simulated data:')
            str6 <- paste('The mean of the population is: μ = ', mean(data$y))
            str7 <- paste('τ1  = -τ2 =', mean(data[data$group1=="a",1])-mean(data$y))
            str8 <- paste('β1  = - β2 = ', mean(data[data$group2=="c",1])-mean(data$y))
            str9 <- paste('(τβ)11  = - (τβ)12 = - (τβ)21 = (τβ)22 = ', mean(data[data$group1=="a"&data$group2=="c",1]) - mean(data[data$group1=="a",1]) - mean(data[data$group2=="c",1]) + mean(data$y))
            HTML(paste(str0, str1, str2, str3, str4, str5, str6, str7, str8, str9, sep = '<br/>'))
        })
       output$ANOVA <- renderPrint({
           y11. <- input$y11.
           y12. <- input$y12.
           y21. <- input$y21.
           y22. <- input$y22.
           set.seed(16)
           y<-rnorm(n = 16, mean = c(y11.,y12.,y21.,y22.), sd = 1)
           group1 <- rep(letters[1:2], each = 2, time=1)
           group2 <- rep(letters[3:4], length.out = 4)
           data<-data.frame(y,group1,group2)
           data$group1 <- as.factor(data$group1)
           data$group2 <- as.factor(data$group2)
           options(contrasts =c("contr.sum","contr.poly"))
           lm1 <- lm(y ~ group1*group2, data=data)
           
           print(summary(lm1))
           br()
           br()
           print(Anova(lm1,type="II"))
           
       })
       output$interactionplot<- renderPlot({
           y11. <- input$y11.
           y12. <- input$y12.
           y21. <- input$y21.
           y22. <- input$y22.
           set.seed(16)
           y<-rnorm(n = 16, mean = c(y11.,y12.,y21.,y22.), sd = 1)
           group1 <- rep(letters[1:2], each = 2, time=1)
           group2 <- rep(letters[3:4], length.out = 4)
           data<-data.frame(y,group1,group2)
           interaction.plot(x.factor = data$group1, #x-axis variable
                            trace.factor = data$group2, #variable for lines
                            response = data$y, #y-axis variable
                            fun = mean, #metric to plot
                            ylab = "y",
                            xlab = "group 1",
                            col = c("red", "blue"),
                            lty = 1, #line type
                            lwd = 2, #line width
                            trace.label = "group 2", ylim=c(min(y)-10,max(y)+10))
       })
}

# Run the application 
shinyApp(ui = ui, server = server)
