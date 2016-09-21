hinded=read.csv2("./data/UT_13_14_hinded.csv")
source("helpers.R")
library("ggplot2")
library("dplyr")
library("reshape2")
library("shiny")

shinyServer(function(input, output) {

    output$text1 <- renderText({ 
        paste("Kasutaja valitud väärtus. Teaduskond:", input$teaduskond)
    })
    output$text2 <- renderText({ 
        paste("Kasutaja valitud väärtus. Semester:", input$semester)
    })
    output$text3 <- renderText({ 
        paste("Kasutaja valitud väärtus. Semester:", input$slider)
    })
    output$plot1 <- renderPlot({ 
        plot_oppeained(data=hinded, tk=input$teaduskond, sem=input$semester,
                       nr=input$slider, jrk=input$jarjestus)
    })

})
