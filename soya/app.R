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
library(tidyverse)
library(mixlm)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Soya example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("y11",
                        "Weight of piglet 1 with diet NONS",
                        min = 1,max = 50,value = 10),
            sliderInput("y12",
                    "Weight of piglet 2 with diet NONS",
                    min = 1,
                    max = 50,
                    value = 15),
            sliderInput("y21",
                "Weight of piglet 3 with diet S1",
                min = 1,
                max = 50,
                value = 30),
        sliderInput("y22",
            "Weight of piglet 4 with diet S1",
            min = 1,
            max = 50,
            value = 35),
        sliderInput("y31",
            "Weight of piglet 5 with diet S2",
            min = 1,
            max = 50,
            value = 45),
        sliderInput("y32",
            "Weight of piglet 6 with diet S2",
            min = 1,
            max = 50,
            value = 50),
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
            tabPanel("Scatterplot", plotOutput("scatterplot")),
            tabPanel("Total and Treatment mean",tableOutput("mean"),plotOutput("plotwithmean")),
            tabPanel("ANOVA tabel",verbatimTextOutput("anovatable")),
            tabPanel("SSE and SS treatment", plotOutput("ssplot"),textOutput("explain"))
        ))
    ))

# Define server logic required to draw a histogram
server <- function(input, session,output) {
    output$mean <- renderTable({
    y11 <- input$y11
    y12 <- input$y12
    y21 <- input$y21
    y22 <- input$y22
    y31 <- input$y31
    y32 <- input$y32
    y1. <- mean(c(y11,y12))
    y2. <- mean(c(y21,y22))
    y3. <- mean(c(y31,y32))
    y.. <- mean(c(y11,y12,y21,y22,y31,y32))
    value <- c(y1.,y2.,y3.,y..)
    name <- c("y1.","y2.","y3.","y..")
    paste(name,value)
    })
    output$scatterplot <- renderPlot({
        y11 <- input$y11
        y12 <- input$y12
        y21 <- input$y21
        y22 <- input$y22
        y31 <- input$y31
        y32 <- input$y32
        weight <- c(y11,y12,y21,y22,y31,y32)
        diet <- c("NONS","NONS","S1","S1","S2","S2")
        soya <- data.frame(weight,diet)
        soya$diet <- as.factor(soya$diet)
        ggplot()+geom_point(data=soya, aes(x = diet, y = weight, fill = diet), shape=21,size=6)+
            scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
    })
    output$plotwithmean <- renderPlot({
        y11 <- input$y11
        y12 <- input$y12
        y21 <- input$y21
        y22 <- input$y22
        y31 <- input$y31
        y32 <- input$y32
        weight <- c(y11,y12,y21,y22,y31,y32)
        diet <- c("NONS","NONS","S1","S1","S2","S2")
        soya <- data.frame(weight,diet)
        soya$diet <- as.factor(soya$diet)
        y1. <- mean(c(y11,y12))
        y2. <- mean(c(y21,y22))
        y3. <- mean(c(y31,y32))
        y.. <- mean(c(y11,y12,y21,y22,y31,y32))
        ggplot()+geom_point(data=soya, aes(x = diet, y = weight, fill = diet), shape=21,size=6)+
            scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
            geom_hline(yintercept=y1., linetype="dashed", color = "#00AFBB", size=2)+
                geom_hline(yintercept=y2., linetype="dashed", color = "#E7B800", size=2)+
                geom_hline(yintercept=y3., linetype="dashed", color = "#FC4E07", size=2)+
                geom_hline(yintercept=y.., color = "black", size=2)
    })
    output$ssplot <- renderPlot({
        y11 <- input$y11
        y12 <- input$y12
        y21 <- input$y21
        y22 <- input$y22
        y31 <- input$y31
        y32 <- input$y32
        y1. <- mean(c(y11,y12))
        y2. <- mean(c(y21,y22))
        y3. <- mean(c(y31,y32))
        y.. <- mean(c(y11,y12,y21,y22,y31,y32))
        length1<- abs(y11 - y1.)
        length2<- abs(y21 - y2.)
        length3<- abs(y31 - y3.)
        length4<- abs(y1. - y..)
        length5<- abs(y2. - y..)
        length6<- abs(y3. - y..)
        y_error <- c(length1,length1,length2,length2,length3,length3)
        tr_error <- c(length4,length4,length5,length5,length6,length6)
        value <- c(y1.,y1.,y2.,y2.,y3.,y3.)
        y <- c(y11,y12,y21,y22,y31,y32)
        x <- c("NONS","NONS","S1","S1","S2","S2")
        mean <- c(y..,y..,y..,y..,y..,y..)
        ss <- data.frame(x,y,y_error,value,mean,tr_error)
        ss$x<-as.factor(ss$x)
        ggplot(data=ss)+geom_point(aes(x = x, y = y, fill = x), shape=21,size=6)+
            facet_grid(cols=vars(x), scale="free_x")+
            scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
            geom_hline(yintercept=y1., linetype="dashed", color = "#00AFBB", size=2)+
            geom_hline(yintercept=y2., linetype="dashed", color = "#E7B800", size=2)+
            geom_hline(yintercept=y3., linetype="dashed", color = "#FC4E07", size=2)+
            geom_hline(yintercept=y.., color = "black", size=2)+
            geom_segment(data=ss, aes(x=x,y=y,xend = x, yend=(ifelse(value<y,y-y_error,y+y_error))))+
            geom_point(aes(x=x, y=value, fill=x), shape=25,size=6)+
            geom_tile(data=ss,aes(x=x,y=ifelse(value>y,y+(y_error/2),y-(y_error/2)),height=y_error,width=y_error,alpha=0.1),position = position_dodge2(preserve = "total"))+
            theme(axis.text.y=element_text(size=22),strip.text.x = element_text(size = 22),plot.title = element_text(size=22,hjust=0.5),axis.title.y=element_blank(),axis.text.x=element_blank(),axis.title.x = element_blank())+theme(legend.position="none")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            geom_tile(data=ss[c(1,3,5),],aes(x=x,y=ifelse(mean>value,value+(tr_error/2),value-(tr_error/2)),height=tr_error,width=tr_error,alpha=0.1),fill="turquoise")+
            coord_cartesian(xlim=c(-25,25),
                            ylim=c(0,50))
    })
    output$explain <- renderText({
        paste("Black line is the total mean. 
              Dashed lines are the mean of each group.
              Black squares are the SSE. 
              Turquoise squares are the SS Treatment")
    })
    output$anovatable <- renderPrint({
        y11 <- input$y11
        y12 <- input$y12
        y21 <- input$y21
        y22 <- input$y22
        y31 <- input$y31
        y32 <- input$y32
        weight <- c(y11,y12,y21,y22,y31,y32)
        diet <- c("NONS","NONS","S1","S1","S2","S2")
        soya <- data.frame(weight,diet)
        soya$diet <- as.factor(soya$diet)
        options(contrasts =c("contr.sum","contr.poly"))
        lm1<- lm(weight~diet, data=soya)
        print(summary(lm1))
        br()
        br()
        print(Anova(lm1,type="II"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
