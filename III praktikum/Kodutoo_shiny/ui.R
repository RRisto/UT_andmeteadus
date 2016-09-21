
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Rakendus õppeainete valimiseks"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        # Copy the line below to make a select box 
        selectInput("teaduskond", label = h3("Vali teaduskond"), 
                    choices = list("Arstiteaduskond" = "AR", "Euroopa kolledž"="EC",
                               "Filosoofiateaduskond"="FL", 
                               "Kehakultuuriteaduskond"="KK",    
                                "Loodus- ja tehnoloogiateaduskond"="LO",
                               "Majandusteaduskond"="MJ",
                               "Matemaatika-informaatikateaduskond"="MT",
                               "Narva kolledž"="NC",
                                "Õigusteaduskond"="OI",
                               "Pärnu kolledž"="PC",
                               "Sotsiaal- ja haridusteaduskond"="SH",
                               "Usuteaduskond"="US",
                               "Viljandi kultuuriakadeemia"="VK"), 
                    selected = 1),
        radioButtons("semester", label = h3("Vali semester"),
                     choices = list("Kevadsemester" = "K", "Sügissemester" = "S"), 
                     selected = "K"),
        sliderInput("slider", label = h3("Kuvatavate õppeainete arv"), min = 0, 
                    max = 50, value = 25),
        radioButtons("jarjestus", label = h3("Järjestus"),
                     choices = list("Parimad" = 0, "Halvimad" = 1), 
                     selected = 0)
    ),

    # Show a plot of the generated distribution
    mainPanel(
        h1("Sissejuhatus"),
        p("Aine andmeteadus kodutöö raames tehtav rakendus, mis aitab leida 
            aineid, kus saadi parimaid hindeid."),
        textOutput("text1"),
        textOutput("text2"),
        textOutput("text3"),
        plotOutput("plot1")
    )
  )
))
