### shiny
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)

dane <- read.csv("/Users/adekkiezun/Downloads/dane_gotowe_v3.csv")

dane$release_date <- mdy(dane$release_date)
dane$month <- month(dane$release_date)

ui <- navbarPage(
  title = "Wybór strony",
  
  tabPanel("Opis",
           fluidPage(
             titlePanel(p("Analiza filmów znajdujących się w serwisie", span(img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/69/IMDB_Logo_2016.svg/1280px-IMDB_Logo_2016.svg.png", height = 50, width = 100)))),
             
             sidebarLayout(position = "right",
                           sidebarPanel(p("W celu uproszczenia rozpatrywanego zbioru danych przeprowadzona została selekcja obserwacji (treści znajdujących się w serwisie IMDB)."),
                                        p("Wybrane zostały wyłącznie filmy wydane od 1970 roku, posiadające co najmniej 50 tysięcy opinii."),
                                        p("Dodatkowo wybrane zostały tylko te filmy, które posiadały budżet oraz dochód wyrażone w dolarze amerykańskim.")),
                           mainPanel(p("Celem projektu jest odkrycie i zaprezentowanie ciekawych zależności istniejących między informacjami opisującymi filmy w serwisie IMDB."),
                                     p("Dane zostały pozyskane z zasobów serwisu IMDB, które są udostępnione i codziennie aktualizowane przez ten serwis. W celu wzbogacenia zawartości projektu wykonany został również web scraping. Pozwolił on na zgromadzenie informacji o filmach, które nie są domyślnie dostarczane użytkownikom przez serwis IMDB w plikach do pobrania. Są to między innymi budżety oraz dochody filmów."),
                                     p("Informacje przedstawione na wykresach przedstawiają stan na dzień 12 listopada 2023."))
             )
           )
  ),
  tabPanel("Zbiór danych",
           fluidPage(
             h1("Zbiór danych"),
             dataTableOutput("dane")
           )
  ),
  tabPanel("Rozkłady",
           fluidPage(
             h1("Rozkłady"),
             selectInput("wyborPodzialu", "Wybierz podział", choices = c("Ujęcie roczne", "Ujęcie miesięczne")),
             conditionalPanel(
               condition = "input.wyborPodzialu == 'Ujęcie miesięczne'",
               sliderInput("selectedYearRange", "Wybierz zakres lat", 
                           min = min(dane$startYear), max = max(dane$startYear),
                           value = c(min(dane$startYear), max(dane$startYear)), step = 1)
             ),
             plotlyOutput("rozkladLiczbyFilmow"),
             plotOutput("rozkladOcen"),
             plotlyOutput("rozkladKolowy"),
             plotOutput("rozkladGatunkowBar")
           )
  ),
  tabPanel("Reżyserzy",
           fluidPage(
             h1("Reżyserzy"),
             tabsetPanel(
               tabPanel("Wykres",
             selectInput("czyGatunki", "Czy pokazywać podział na gatunki?", choices = c("Nie", "Tak")),
             selectInput("Dir1liczba", "Wybierz liczbę reżyserów", choices = c(5, 10, 15, 20, 25)),
             sliderInput("selectedYearRangeDir1", "Wybierz zakres lat", 
                         min = min(dane$startYear), max = max(dane$startYear),
                         value = c(min(dane$startYear), max(dane$startYear)), step = 1),
             conditionalPanel(
               condition = "input.czyGatunki == 'Nie'",
               checkboxGroupInput("gatunkiDir1", "Wybierz gatunki:",
                                  choices = unique(dane$genre1),
                                  selected = unique(dane$genre1))
             ),
             plotlyOutput("plotDir1")),
             tabPanel("Tabela", dataTableOutput("daneDir1"))),
             selectInput("czySrednia", "Statystyka:", choices = c("Średnia", "Suma")),
             selectInput("Dir2liczba", "Wybierz liczbę reżyserów", choices = c(5, 10, 15, 20, 25)),
             sliderInput("selectedYearRangeDir2", "Wybierz zakres lat", 
                         min = min(dane$startYear), max = max(dane$startYear),
                         value = c(min(dane$startYear), max(dane$startYear)), step = 1),
             checkboxGroupInput("gatunkiDir2", "Wybierz gatunki:",
                                choices = unique(dane$genre1),
                                selected = unique(dane$genre1)),
             selectInput("sortowanieDir2", "Sortuj według:", choices = c("Dochód", "Budżet")),
             plotlyOutput("plotDir2"),
             plotOutput("plotDir2Box"),
             plotlyOutput("plotDirBudgetGross"),
             selectInput("sortowanieDir3", "Sortowanie po ocenie:", choices = c("Tak", "Nie")),
             plotOutput("plotDir3"),
             plotlyOutput("plotDir4")
           )
  ),
  tabPanel("Gatunki filmowe",
           fluidPage(
             sliderInput("selectedYearRangeGen1", "Wybierz zakres lat", 
                         min = min(dane$startYear), max = max(dane$startYear),
                         value = c(min(dane$startYear), max(dane$startYear)), step = 1),
             plotOutput("plotGen1"),
             plotOutput("plotGen2"),
             plotOutput("plotGen5"),
             plotlyOutput("plotGen3"),
             plotlyOutput("plotGen4")
           )
  ),
  tabPanel("Długość filmu",
           fluidPage(
             sliderInput("selectedYearRangeLen", "Wybierz zakres lat", 
                         min = min(dane$startYear), max = max(dane$startYear),
                         value = c(min(dane$startYear), max(dane$startYear)), step = 1),
             sliderInput("selectedLen", "Wybierz zakres długości filmu", 
                         min = min(dane$runtimeMinutes), max = max(dane$runtimeMinutes),
                         value = c(min(dane$runtimeMinutes), max(dane$runtimeMinutes)), step = 1),
             plotlyOutput("plotLen1"),
             plotOutput("plotLen2Box"),
             selectInput("sortowanieLen3Box", "Sortowanie po długości:", choices = c("Malejąco", "Rosnąco")),
             selectInput("lenliczba", "Wybierz liczbę reżyserów", choices = c(5, 10, 15, 20, 25)),
             plotOutput("plotLen3Box")

          
           )
  )
)  

server <- function(input, output) {
  output$dane <- renderDataTable({dane})
  
  output$rozkladLiczbyFilmow <- renderPlotly({
    if (input$wyborPodzialu == "Ujęcie roczne") {
      gg <- ggplot(dane, aes(x = startYear)) +
        geom_histogram(color = "#4B4B4B", size = 1, fill = "#DBA506", bins = 53) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 170)) +
        xlab("Rok") +
        ylab("Liczba filmow") +
        ggtitle("Rozklad liczby filmow wedlug roku wydania") +
        theme_bw()
      
      ggplotly(gg)
    } else if (input$wyborPodzialu == "Ujęcie miesięczne") {
      
      dane_miesiace <- dane
      if (!is.null(input$selectedYearRange)) {
        dane_miesiace <- dane_miesiace %>%
          filter(startYear %in% seq(input$selectedYearRange[1], input$selectedYearRange[2]))
      }
      
      dane_miesiace <- dane_miesiace %>%
        group_by(month) %>%
        summarise(liczba_filmow = n()) %>%
        arrange(month)
      dane_miesiace <- na.omit(dane_miesiace)
      
      gg <- ggplot(dane_miesiace, aes(y = liczba_filmow, x = factor(month))) +
        geom_bar(stat = "identity", color = "#4B4B4B", size = 1, fill = "#DBA506") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(dane_miesiace$liczba_filmow) + 20)) +
        scale_x_discrete(labels = c("Stycznen", "Luty", "Marzec", "Kwiecien", "Maj", "Czerwiec", "Lipiec", "Sierpien", "Wrzesien", "Pazdziernik", "Listopad", "Grudzien")) +
        xlab("Miesiąc") +
        ylab("Liczba filmow") +
        ggtitle("Liczba filmow wydanych w danym miesiącu") +
        theme_bw(base_size = 12)
      
      ggplotly(gg)
    }
  })
  
  output$rozkladOcen <- renderPlot({
    dane_miesiace_oceny <- dane
    if (!is.null(input$selectedYearRange)) {
      dane_miesiace_oceny <- dane_miesiace_oceny %>%
        filter(startYear %in% seq(input$selectedYearRange[1], input$selectedYearRange[2]))
    }
    gg2 <- ggplot(dane_miesiace_oceny, aes(x = floor(averageRating))) +
      geom_histogram(color = "#4B4B4B", size = 1, fill = "#DBA506", binwidth = 1) +
      scale_y_log10(expand = c(0, 0), limits = c(1, 2000)) +
      scale_x_continuous(breaks = 1:10, labels = 1:10, limits = c(0, 11)) +
      xlab("Ocena [w skali od 1 do 10]") +
      ylab("Liczba filmow") +
      ggtitle("Liczba filmow wedlug oceny") +
      theme_bw(base_size = 16)
    
    print(gg2)
    
  })
  
  output$rozkladKolowy <- renderPlotly({
  dane_wykres_kolowy3 <- dane[dane$startYear %in% seq(input$selectedYearRange[1], input$selectedYearRange[2]), ]
  dane_wykres_kolowy1 <- table(dane_wykres_kolowy3$genre1)[c(1,2,4,5,6,8,11)]
  dane_wykres_kolowy2 <- sum(table(dane_wykres_kolowy3$genre1)[c(3,7,9,10,12,13,14,15,16)])
  dane_wykres_kolowy <- c(dane_wykres_kolowy1, dane_wykres_kolowy2)
  names(dane_wykres_kolowy)[length(dane_wykres_kolowy)] <- "Other"
  odsetki <- (dane_wykres_kolowy / sum(dane_wykres_kolowy)) * 100
  nazwy<- names(dane_wykres_kolowy)
  plot_ly(labels = ~nazwy, values = ~dane_wykres_kolowy, type = 'pie',
          marker = list(colors = c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f",
                                   "#6a994e", "#2f4550", "#bb745a", "#f7c1bb", "#fb8500",
                                   "#8338ec", "#004e89", "#00ad44", "#bc9cb0", "#42342d",
                                   "#a82b87")),
          textinfo = 'label+percent', textposition = 'outside',
          hoverinfo = 'text+percent', 
          hovertext = ~paste(names(dane_wykres_kolowy), "<br>", nazwy)) %>%
    layout(title = "Udzial poszczegolnych gatunkow", showlegend = FALSE)
  })
  
  output$plotDir1 <- renderPlotly({
    if (input$czyGatunki == "Tak") {
      
      dane_rezyserzy1 <- dane
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2])
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        group_by(primaryName1) %>%
        summarise(liczba_filmow = n()) %>%
        arrange(desc(liczba_filmow))
      dane_rezyserzy1 <- dane_rezyserzy1[1:input$Dir1liczba,]
      dane_rezyserzy1_join <- dane %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2]) %>%
        select(c("primaryName1", "primaryTitle", "genre1"))
      dane_rezyserzy1 <- left_join(dane_rezyserzy1, dane_rezyserzy1_join, by = "primaryName1")
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        arrange(desc(liczba_filmow))
      dane_rezyserzy1$primaryName1 <- factor(dane_rezyserzy1$primaryName1, levels = rev(unique(dane_rezyserzy1$primaryName1)))
      ggplotly(ggplot(dane_rezyserzy1, aes(y = primaryName1)) +
        geom_bar(stat = "count", color="#4B4B4B", size = 1, aes(fill = genre1)) +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(dane_rezyserzy1$liczba_filmow) + 3))  +
        scale_fill_manual(name = "Gatunek", values =  c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f",
                                                        "#6a994e", "#2f4550", "#bb745a", "#f7c1bb", "#fb8500",
                                                        "#8338ec", "#004e89", "#00ad44", "#bc9cb0", "#42342d",
                                                        "#a82b87")) +
        xlab("Liczba filmow") +
        ylab("Rezyser") +
        ggtitle("Liczba filmow wybranych rezyserow") +
        theme_bw())
      
      
      
    } else if (input$czyGatunki == "Nie") {
      
      dane_rezyserzy1 <- dane
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2] & genre1 %in% input$gatunkiDir1)
      
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        group_by(primaryName1) %>%
        summarise(liczba_filmow = n()) %>%
        arrange(desc(liczba_filmow))
      dane_rezyserzy1 <- dane_rezyserzy1[1:input$Dir1liczba,]
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        arrange(liczba_filmow)
      dane_rezyserzy1$primaryName1 <- factor(dane_rezyserzy1$primaryName1, levels = dane_rezyserzy1$primaryName1)
      ggplotly(ggplot(dane_rezyserzy1, aes(y = primaryName1, x = liczba_filmow)) +
        geom_bar(stat = "identity", color="#4B4B4B", size = 1, fill="#DBA506") +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(dane_rezyserzy1$liczba_filmow) + 3))  +
        scale_fill_discrete(name = "Gatunek") +
        xlab("Liczba filmow") +
        ylab("Rezyser") +
        ggtitle("Liczba filmow wybranych rezyserow") +
        theme_bw())
    }
  })
  
  output$daneDir1 <- renderDataTable({
    if (input$czyGatunki == "Tak") {
      dane_rezyserzy1 <- dane
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2])
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        group_by(primaryName1) %>%
        summarise(liczba_filmow = n()) %>%
        arrange(desc(liczba_filmow))
      dane_rezyserzy1 <- dane_rezyserzy1[1:input$Dir1liczba,]
      dane_rezyserzy1_join <- dane %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2]) %>%
        select(c("startYear","primaryName1", "primaryTitle", "genre1"))
      dane_rezyserzy1 <- left_join(dane_rezyserzy1, dane_rezyserzy1_join, by = "primaryName1")
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        arrange(desc(liczba_filmow)) %>%
        select(-c("liczba_filmow"))
      print(na.omit(dane_rezyserzy1))
    } else if (input$czyGatunki == "Nie") {
      dane_rezyserzy1 <- dane
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2] & genre1 %in% input$gatunkiDir1)
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        group_by(primaryName1) %>%
        summarise(liczba_filmow = n()) %>%
        arrange(desc(liczba_filmow))
      dane_rezyserzy1 <- dane_rezyserzy1[1:input$Dir1liczba,]
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        arrange(liczba_filmow)
      dane_rezyserzy1_join <- dane %>%
        filter(startYear >= input$selectedYearRangeDir1[1] & startYear <= input$selectedYearRangeDir1[2] & genre1 %in% input$gatunkiDir1) %>%
        select(c("startYear","primaryName1", "primaryTitle", "genre1"))
      dane_rezyserzy1 <- left_join(dane_rezyserzy1, dane_rezyserzy1_join, by = "primaryName1")
      dane_rezyserzy1 <- dane_rezyserzy1 %>%
        arrange(desc(liczba_filmow)) %>%
        select(-c("liczba_filmow"))
      print(na.omit(dane_rezyserzy1))
    }
    })
  
  output$plotDir2 <- renderPlotly({
    if (input$czySrednia == "Średnia") {
      if (input$sortowanieDir2 == "Dochód"){
        dane_rezyserzy2 <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          group_by(primaryName1) %>%
          summarise(sredni_dochod = mean(gross),
                    sredni_budzet = mean(budget),
                    suma_dochod = sum(gross),
                    suma_budzet = sum(budget)) %>%
          arrange(desc(sredni_dochod))
        dane_rezyserzy2 <- dane_rezyserzy2[1:input$Dir2liczba,]
        dane_rezyserzy2 <- dane_rezyserzy2 %>%
          arrange(sredni_dochod)
        dane_rezyserzy2$primaryName1 <- factor(dane_rezyserzy2$primaryName1, levels = dane_rezyserzy2$primaryName1)
        ggplotly(ggplot(dane_rezyserzy2, aes(y = primaryName1)) +
                   geom_bar(aes(x = sredni_dochod, fill="Sredni dochod filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                   geom_bar(aes(x = sredni_budzet, fill="Sredni budzet filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                   scale_x_continuous(expand = c(0,0),
                                      limits = c(0, (max(dane_rezyserzy2$sredni_dochod)*1.1))) +
                   xlab("Wielkość [$]") +
                   ylab("Rezyser") +
                   ggtitle("Srednie dochody i budzety filmow wybranych rezyserow") +
                   scale_fill_manual(name = "Wartosci", 
                                     values = c("Sredni dochod filmow" = "#DBA506", "Sredni budzet filmow" = "#128bb5"),
                                     labels = c("Sredni dochod filmow", "Sredni budzet filmow")) +
                   theme_bw())
      }else{
      dane_rezyserzy2 <- dane %>%
        filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
        group_by(primaryName1) %>%
        summarise(sredni_dochod = mean(gross),
                  sredni_budzet = mean(budget),
                  suma_dochod = sum(gross),
                  suma_budzet = sum(budget)) %>%
        arrange(desc(sredni_budzet))
      dane_rezyserzy2 <- dane_rezyserzy2[1:input$Dir2liczba,]
      dane_rezyserzy2 <- dane_rezyserzy2 %>%
        arrange(sredni_budzet)
      dane_rezyserzy2$primaryName1 <- factor(dane_rezyserzy2$primaryName1, levels = dane_rezyserzy2$primaryName1)
      ggplotly(ggplot(dane_rezyserzy2, aes(y = primaryName1)) +
        geom_bar(aes(x = sredni_dochod, fill="Sredni dochod filmow"), stat = "identity", color="#4B4B4B", size = 1) +
        geom_bar(aes(x = sredni_budzet, fill="Sredni budzet filmow"), stat = "identity", color="#4B4B4B", size = 1) +
        scale_x_continuous(expand = c(0,0),
                           limits = c(0, (max(dane_rezyserzy2$sredni_dochod)*1.1))) +
        xlab("Wielkość [$]") +
        ylab("Rezyser") +
        ggtitle("Srednie dochody i budzety filmow wybranych rezyserow") +
        scale_fill_manual(name = "Wartosci", 
                          values = c("Sredni dochod filmow" = "#DBA506", "Sredni budzet filmow" = "#128bb5"),
                          labels = c("Sredni dochod filmow", "Sredni budzet filmow")) +
        theme_bw())
      }
    } else if (input$czySrednia == "Suma") {
      if (input$sortowanieDir2 == "Dochód"){
        dane_rezyserzy2 <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          group_by(primaryName1) %>%
          summarise(sredni_dochod = mean(gross),
                    sredni_budzet = mean(budget),
                    suma_dochod = sum(gross),
                    suma_budzet = sum(budget)) %>%
          arrange(desc(suma_dochod))
        dane_rezyserzy2 <- dane_rezyserzy2[1:input$Dir2liczba,]
        dane_rezyserzy2 <- dane_rezyserzy2 %>%
          arrange(suma_dochod)
        dane_rezyserzy2$primaryName1 <- factor(dane_rezyserzy2$primaryName1, levels = dane_rezyserzy2$primaryName1)
        ggplotly(ggplot(dane_rezyserzy2, aes(y = primaryName1)) +
                   geom_bar(aes(x = suma_dochod, fill="Suma dochodow filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                   geom_bar(aes(x = suma_budzet, fill="Suma budzetow filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                   scale_x_continuous(expand = c(0,0),
                                      limits = c(0, (max(dane_rezyserzy2$suma_dochod)*1.1))) +
                   xlab("Wielkość [$]") +
                   ylab("Rezyser") +
                   ggtitle("Suma dochodow i budzetow filmow wybranych rezyserow") +
                   scale_fill_manual(name = "Wartosci", 
                                     values = c("Suma dochodow filmow" = "#DBA506", "Suma budzetow filmow" = "#128bb5"),
                                     labels = c("Suma dochodow filmow", "Suma budzetow filmow")) +
                   theme_bw())
      }else{
      dane_rezyserzy2 <- dane %>%
        filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
        group_by(primaryName1) %>%
        summarise(sredni_dochod = mean(gross),
                  sredni_budzet = mean(budget),
                  suma_dochod = sum(gross),
                  suma_budzet = sum(budget)) %>%
        arrange(desc(suma_budzet))
      dane_rezyserzy2 <- dane_rezyserzy2[1:input$Dir2liczba,]
      dane_rezyserzy2 <- dane_rezyserzy2 %>%
        arrange(suma_budzet)
      dane_rezyserzy2$primaryName1 <- factor(dane_rezyserzy2$primaryName1, levels = dane_rezyserzy2$primaryName1)
      ggplotly(ggplot(dane_rezyserzy2, aes(y = primaryName1)) +
                 geom_bar(aes(x = suma_dochod, fill="Suma dochodow filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                 geom_bar(aes(x = suma_budzet, fill="Suma budzetow filmow"), stat = "identity", color="#4B4B4B", size = 1) +
                 scale_x_continuous(expand = c(0,0),
                                    limits = c(0, (max(dane_rezyserzy2$suma_dochod)*1.1))) +
                 xlab("Wielkość [$]") +
                 ylab("Rezyser") +
                 ggtitle("Suma dochodow i budzetow filmow wybranych rezyserow") +
                 scale_fill_manual(name = "Wartosci", 
                                   values = c("Suma dochodow filmow" = "#DBA506", "Suma budzetow filmow" = "#128bb5"),
                                   labels = c("Suma dochodow filmow", "Suma budzetow filmow")) +
                 theme_bw())
      }
    }
  })
  
  output$plotDir2Box <- renderPlot({
    if (input$sortowanieDir2 == "Dochód"){
      if (input$czySrednia == "Średnia") {
      dane_rezyserzy2Box <- dane %>%
        filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
        group_by(primaryName1) %>%
        summarise(sredni_dochod = mean(gross),
                  sredni_budzet = mean(budget),
                  suma_dochod = sum(gross),
                  suma_budzet = sum(budget),
                  liczba_filmow = n()) %>%
        arrange(desc(sredni_dochod))
      dane_rezyserzy2Box <- dane_rezyserzy2Box[1:input$Dir2liczba,]
      dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
        arrange(sredni_dochod)
      dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = dane_rezyserzy2Box$primaryName1)
      dane_rezyserzy2Box_join <- dane %>%
        filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
        select(c("primaryName1", "primaryTitle","budget", "gross"))
      dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
        left_join(dane_rezyserzy2Box_join, by = "primaryName1")
      dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = unique(dane_rezyserzy2Box$primaryName1))
      
      ggplot(dane_rezyserzy2Box, aes(x = gross, y = primaryName1)) +
                 geom_boxplot(aes(fill = liczba_filmow), color = "#4B4B4B", outlier.shape = 21,
                              outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
        scale_fill_gradient(low = "white", high = "#DBA506", name = "Liczba filmow") +
        xlab("Dochody filmow [$]") +
        ylab("Rezyser") +
        ggtitle("Dochody filmow wybranych rezyserow") +
        geom_vline(xintercept = mean(dane_rezyserzy2Box$gross, na.rm = TRUE),
                    lty = 2, color = "#4B4B4B",) +
        theme_bw(base_size = 16)
      }else{
        dane_rezyserzy2Box <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          group_by(primaryName1) %>%
          summarise(sredni_dochod = mean(gross),
                    sredni_budzet = mean(budget),
                    suma_dochod = sum(gross),
                    suma_budzet = sum(budget),
                    liczba_filmow = n()) %>%
          arrange(desc(suma_dochod))
        dane_rezyserzy2Box <- dane_rezyserzy2Box[1:input$Dir2liczba,]
        dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
          arrange(suma_dochod)
        dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = dane_rezyserzy2Box$primaryName1)
        dane_rezyserzy2Box_join <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          select(c("primaryName1", "primaryTitle","budget", "gross"))
        dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
          left_join(dane_rezyserzy2Box_join, by = "primaryName1")
        dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = unique(dane_rezyserzy2Box$primaryName1))
        
        ggplot(dane_rezyserzy2Box, aes(x = gross, y = primaryName1)) +
          geom_boxplot(aes(fill = liczba_filmow), color = "#4B4B4B", outlier.shape = 21,
                       outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
          scale_fill_gradient(low = "white", high = "#DBA506", name = "Liczba filmow") +
          xlab("Dochody filmow [$]") +
          ylab("Rezyser") +
          ggtitle("Dochody filmow wybranych rezyserow") +
          geom_vline(xintercept = mean(dane_rezyserzy2Box$gross, na.rm = TRUE),
                     lty = 2, color = "#4B4B4B",) +
          theme_bw(base_size = 16)
      }
      
      
      }else{
        if (input$czySrednia == "Średnia") {
        dane_rezyserzy2Box <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          group_by(primaryName1) %>%
          summarise(sredni_dochod = mean(gross),
                    sredni_budzet = mean(budget),
                    suma_dochod = sum(gross),
                    suma_budzet = sum(budget),
                    liczba_filmow = n()) %>%
          arrange(desc(sredni_budzet))
        dane_rezyserzy2Box <- dane_rezyserzy2Box[1:input$Dir2liczba,]
        dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
          arrange(sredni_budzet)
        dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = dane_rezyserzy2Box$primaryName1)
        dane_rezyserzy2Box_join <- dane %>%
          filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
          select(c("primaryName1", "primaryTitle","budget", "gross"))
        dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
          left_join(dane_rezyserzy2Box_join, by = "primaryName1")
        dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = unique(dane_rezyserzy2Box$primaryName1))
        
        ggplot(dane_rezyserzy2Box, aes(x = gross, y = primaryName1)) +
          geom_boxplot(aes(fill = liczba_filmow), color = "#4B4B4B", outlier.shape = 21,
                       outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
          scale_fill_gradient(low = "white", high = "#DBA506", name = "Liczba filmow") +
          xlab("Dochody filmow [$]") +
          ylab("Rezyser") +
          ggtitle("Dochody filmow wybranych rezyserow") +
          geom_vline(xintercept = mean(dane_rezyserzy2Box$gross, na.rm = TRUE),
                     lty = 2, color = "#4B4B4B",) +
          theme_bw(base_size = 16)
        }else{
          dane_rezyserzy2Box <- dane %>%
            filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
            group_by(primaryName1) %>%
            summarise(sredni_dochod = mean(gross),
                      sredni_budzet = mean(budget),
                      suma_dochod = sum(gross),
                      suma_budzet = sum(budget),
                      liczba_filmow = n()) %>%
            arrange(desc(suma_budzet))
          dane_rezyserzy2Box <- dane_rezyserzy2Box[1:input$Dir2liczba,]
          dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
            arrange(suma_budzet)
          dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = dane_rezyserzy2Box$primaryName1)
          dane_rezyserzy2Box_join <- dane %>%
            filter(startYear >= input$selectedYearRangeDir2[1] & startYear <= input$selectedYearRangeDir2[2] & genre1 %in% input$gatunkiDir2) %>%
            select(c("primaryName1", "primaryTitle","budget", "gross"))
          dane_rezyserzy2Box <- dane_rezyserzy2Box %>%
            left_join(dane_rezyserzy2Box_join, by = "primaryName1")
          dane_rezyserzy2Box$primaryName1 <- factor(dane_rezyserzy2Box$primaryName1, levels = unique(dane_rezyserzy2Box$primaryName1))
          
          ggplot(dane_rezyserzy2Box, aes(x = gross, y = primaryName1)) +
            geom_boxplot(aes(fill = liczba_filmow), color = "#4B4B4B", outlier.shape = 21,
                         outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
            scale_fill_gradient(low = "white", high = "#DBA506", name = "Liczba filmow") +
            xlab("Dochody filmow [$]") +
            ylab("Rezyser") +
            ggtitle("Dochody filmow wybranych rezyserow") +
            geom_vline(xintercept = mean(dane_rezyserzy2Box$gross, na.rm = TRUE),
                       lty = 2, color = "#4B4B4B",) +
            theme_bw(base_size = 16)
        }
      }
  })
  
  
  
  output$plotDir3 <- renderPlot({
    dane_rezyserzy3 <- dane
    if (!is.null(input$selectedYearRangeDir2)) {
      dane_rezyserzy3 <- dane_rezyserzy3 %>%
        filter(startYear %in% seq(input$selectedYearRangeDir2[1], input$selectedYearRangeDir2[2]) & genre1 %in% input$gatunkiDir2)
    }
    if(input$sortowanieDir3 == "Tak"){
      dane_rezyserzy3 <- dane_rezyserzy3 %>%
        group_by(primaryName1) %>%
        summarise(srednia_ocena = mean(averageRating),
                  srednia_liczba_ocen = mean(numVotes)) %>%
        arrange(desc(srednia_ocena))
      dane_rezyserzy3 <- dane_rezyserzy3[1:input$Dir2liczba,]
      dane_rezyserzy3 <- dane_rezyserzy3 %>%
        arrange(srednia_ocena)
    }else{
    dane_rezyserzy3 <- dane_rezyserzy3 %>%
      group_by(primaryName1) %>%
      summarise(srednia_ocena = mean(averageRating),
                srednia_liczba_ocen = mean(numVotes)) %>%
      arrange(desc(srednia_liczba_ocen))
    dane_rezyserzy3 <- dane_rezyserzy3[1:input$Dir2liczba,]
    dane_rezyserzy3 <- dane_rezyserzy3 %>%
      arrange(srednia_liczba_ocen)
  }
    dane_rezyserzy3$primaryName1 <- factor(dane_rezyserzy3$primaryName1, levels = dane_rezyserzy3$primaryName1)
    skalowanieDir3 <- max(dane_rezyserzy3$srednia_liczba_ocen) * 1.3
    dane_rezyserzy3$srednia_ocena_skalowana <- dane_rezyserzy3$srednia_ocena/10 * skalowanieDir3
    ggplot(dane_rezyserzy3, aes(y = primaryName1)) +
      geom_bar(aes(x = srednia_ocena_skalowana, fill="Srednia ocena filmow"), stat = "identity", color="#4B4B4B", size = 1) +
      geom_bar(aes(x = srednia_liczba_ocen, fill="Srednia liczba ocen filmow"), stat = "identity", color="#4B4B4B", size = 1) +
      scale_x_continuous(expand = c(0,0), limits = c(0, skalowanieDir3),
                         sec.axis = dup_axis(name = "Srednia ocena filmow", breaks = c(0, 0.25*skalowanieDir3, 0.5*skalowanieDir3, 0.75*skalowanieDir3, skalowanieDir3),
                                             labels = c(0, 2.5, 5, 7.5, 10)))  +
      
      xlab("Srednia liczba ocen filmow") +
      ylab("Rezyser") +
      ggtitle("Srednia ocena i liczba ocen wybrancyh rezyserow") +
      scale_fill_manual(name = "Wartosci", 
                        values = c("Srednia ocena filmow" = "#DBA506", "Srednia liczba ocen filmow" = "#128bb5"),
                        labels = c("Srednia ocena filmow", "Srednia liczba ocen filmow")) +
      theme_bw(base_size = 16)
    
  })
  
  output$plotDirBudgetGross <- renderPlotly({
    dane_rezyserzy_BudgetGross <- dane
    if (!is.null(input$selectedYearRangeDir2)) {
      dane_rezyserzy_BudgetGross <- dane_rezyserzy_BudgetGross %>%
        filter(startYear %in% seq(input$selectedYearRangeDir2[1], input$selectedYearRangeDir2[2]) & genre1 %in% input$gatunkiDir2)
    }
    dane_rezyserzy_BudgetGross$genre1 <- ifelse(dane_rezyserzy_BudgetGross$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_rezyserzy_BudgetGross$genre1)
    
    dane_rezyserzy_BudgetGross <- na.omit(dane_rezyserzy_BudgetGross)
    lm_coef <- coef(lm(gross ~ budget, data = dane_rezyserzy_BudgetGross))
    ggplotly(ggplot(dane_rezyserzy_BudgetGross, aes(x = budget, y = gross, text = paste(primaryTitle, "<br>",
                                                                                        "Budzet: $", round(budget), "<br>",
                                                                                        "Dochod: $", round(gross), "<br>",
                                                                                        "Gatunek: ", genre1, "<br>",
                                                                                        "Ocena: ", averageRating))) +
      geom_point(color = "#4B4B4B", shape=21, alpha = 0.7, aes(fill = genre1), size = 2) +
      scale_y_continuous() +
      scale_x_continuous() +
      geom_abline(intercept = lm_coef[1], slope = lm_coef[2] ,color = "#4B4B4B") +
      xlab("Budzet filmu [$]") +
      ylab("Dochod filmu [$]") +
      scale_fill_manual(name = "Gatunek", values =  c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f",
                                                        "#6a994e", "#2f4550", "#bb745a", "#f7c1bb")) +
      ggtitle(paste("Zaleznosc miedzy budzetami a dochodami filmow -",
                    paste("korelacja:", round(cor(dane_rezyserzy_BudgetGross$budget,
                    dane_rezyserzy_BudgetGross$gross), 2)))) +
      theme_bw(), tooltip = c("text"))
    
  })
  
  
  output$plotDir4 <- renderPlotly({
    dane_rezyserzy_votesRating <- dane
    if (!is.null(input$selectedYearRangeDir2)) {
      dane_rezyserzy_votesRating <- dane_rezyserzy_votesRating %>%
        filter(startYear %in% seq(input$selectedYearRangeDir2[1], input$selectedYearRangeDir2[2]) & genre1 %in% input$gatunkiDir2)
    }
    dane_rezyserzy_votesRating <- na.omit(dane_rezyserzy_votesRating)
    lm_coef <- coef(lm(averageRating ~ log10(numVotes), data = dane_rezyserzy_votesRating))
    ggplotly(ggplot(dane_rezyserzy_votesRating, aes(x = numVotes, y = averageRating, text = paste(primaryTitle, "<br>",
                                                                                        "Budzet: $", round(budget), "<br>",
                                                                                        "Dochod: $", round(gross), "<br>",
                                                                                        "Liczba ocen: ", round(numVotes), "<br>",
                                                                                        "Ocena: ", averageRating))) +
               geom_point(color = "#4B4B4B", shape=21, fill = "#DBA506", alpha = 0.7) +
               scale_y_continuous(limits = c(0, 10.3), breaks = 1:10, labels = 1:10) +
               scale_x_log10() +
               geom_abline(intercept = lm_coef[1], slope = lm_coef[2] ,color = "#4B4B4B") +
               xlab("Liczba ocen filmu") +
               ylab("Ocena filmu [w skali od 1 do 10]") +
               ggtitle(paste("Zaleznosc miedzy liczba ocen a koncowa ocena -",
                             paste("korelacja:", round(cor(dane_rezyserzy_votesRating$numVotes,
                                                           dane_rezyserzy_votesRating$averageRating), 2)))) +
               theme_bw(), tooltip = c("text"))
    
  })
  
  output$plotGen1 <- renderPlot({
    dane_genre1 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_genre1 <- dane_genre1 %>%
        filter(startYear %in% seq(input$selectedYearRangeGen1[1], input$selectedYearRangeGen1[2]))
    }
    dane_genre1$genre1 <- ifelse(dane_genre1$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_genre1$genre1)
    ggplot(dane_genre1, aes(x = genre1, y = budget)) +
      geom_boxplot(fill = "#128bb5", color = "#4B4B4B", outlier.shape = 21,
                   outlier.color = "#4B4B4B", outlier.fill = "#128bb5") +
      xlab("Gatunek filmu") +
      ylab("Budzet filmu [$]") +
      ggtitle("Budzety wybranych gatunkow filmowych") +
      scale_y_log10() +
      geom_hline(yintercept = (mean(dane_genre1$budget, na.rm = TRUE)),
                 lty = 2, color = "#4B4B4B",) +
      theme_bw(base_size = 15)
    
  })
  
  output$plotGen2 <- renderPlot({
    dane_genre2 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_genre2 <- dane_genre2 %>%
        filter(startYear %in% seq(input$selectedYearRangeGen1[1], input$selectedYearRangeGen1[2]))
    }
    dane_genre2$genre1 <- ifelse(dane_genre2$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_genre2$genre1)
    ggplot(dane_genre2, aes(x = genre1, y = gross)) +
      geom_boxplot(fill = "#DBA506", color = "#4B4B4B", outlier.shape = 21,
                   outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
      xlab("Gatunek filmu") +
      ylab("Dochod filmu [$]") +
      ggtitle("Dochody wybranych gatunkow filmowych") +
      scale_y_log10() +
      geom_hline(yintercept = (mean(dane_genre2$gross, na.rm = TRUE)),
                 lty = 2, color = "#4B4B4B",) +
      theme_bw(base_size = 15)
    
  })
  
  output$plotGen3 <- renderPlotly({
    dane_genre3 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_genre3 <- dane_genre3 %>%
        filter(startYear %in% seq(input$selectedYearRangeGen1[1], input$selectedYearRangeGen1[2]))
    }
    dane_genre3$genre1 <- ifelse(dane_genre3$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_genre3$genre1)
    dane_genre3 <- dane_genre3 %>%
      group_by(startYear, genre1) %>%
      summarise(suma_budzetu = sum(budget),
                suma_dochodu = sum(gross))
    ggplotly(ggplot(dane_genre3, aes(x = startYear, y = suma_budzetu, fill = genre1)) +
               geom_area(alpha = 0.6) +
               scale_fill_manual(name = "Gatunek", values =  c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f", "#6a994e", "#2f4550", "#bb745a")) +
               labs(title = "Suma budzetu roznych gatunkow filmowych w czasie",
                    x = "Rok",
                    y = "Budzet [$]") +
               theme_bw())
    
    
  })
  
  output$plotGen4 <- renderPlotly({
    dane_genre4 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_genre4 <- dane_genre4 %>%
        filter(startYear %in% seq(input$selectedYearRangeGen1[1], input$selectedYearRangeGen1[2]))
    }
    dane_genre4$genre1 <- ifelse(dane_genre4$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_genre4$genre1)
    dane_genre4 <- dane_genre4 %>%
      group_by(startYear, genre1) %>%
      summarise(suma_budzetu = sum(budget),
                suma_dochodu = sum(gross))
    ggplotly(ggplot(dane_genre4, aes(x = startYear, y = suma_dochodu, fill = genre1)) +
               geom_area(alpha = 0.6) +
               scale_fill_manual(name = "Gatunek",values =  c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f", "#6a994e", "#2f4550", "#bb745a")) +
               labs(title = "Suma dochodu roznych gatunkow filmowych w czasie",
                    x = "Rok",
                    y = "Dochod [$]") +
               theme_bw())
    
    
  })
  
  output$plotLen1 <- renderPlotly({
    dane_len1 <- dane
    if (!is.null(input$selectedYearRangeLen & input$selectedLen)) {
      dane_len1 <- dane_len1 %>%
        filter(startYear %in% seq(input$selectedYearRangeLen[1], input$selectedYearRangeLen[2]) & runtimeMinutes %in% seq(input$selectedLen[1], input$selectedLen[2]))
    }
    dane_len1 <- na.omit(dane_len1)
    lm_coef <- coef(lm(averageRating ~ runtimeMinutes, data = dane_len1))
    ggplotly(ggplot(dane_len1, aes(x = runtimeMinutes, y = averageRating, text = paste(primaryTitle, "<br>",
                                                                                                  "Budzet: $", round(budget), "<br>",
                                                                                                  "Dochod: $", round(gross), "<br>",
                                                                                                  "Liczba ocen: ", round(numVotes), "<br>",
                                                                                                  "Ocena: ", averageRating, "<br>",
                                                                                                  "Dlugosc:", runtimeMinutes, "minut"))) +
               geom_point(color = "#4B4B4B", shape=21, fill = "#DBA506", alpha = 0.7) +
               scale_y_continuous(limits = c(0, 10.3), breaks = 1:10, labels = 1:10) +
               geom_abline(intercept = lm_coef[1], slope = lm_coef[2] ,color = "#4B4B4B") +
               xlab("Dlugosc filmu [minuty]") +
               ylab("Ocena filmu [w skali od 1 do 10]") +
               ggtitle(paste("Zaleznosc miedzy dlugoscia filmu a jego koncowa ocena -",
                             paste("korelacja:", round(cor(dane_len1$runtimeMinutes,
                                                           dane_len1$averageRating), 2)))) +
               theme_bw(), tooltip = c("text"))
    
  })
  
  output$plotLen2Box <- renderPlot({
    dane_len2 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_len2 <- dane_len2 %>%
        filter(startYear %in% seq(input$selectedYearRangeLen[1], input$selectedYearRangeLen[2]))
    }
    dane_len2$genre1 <- ifelse(dane_len2$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_len2$genre1)
    ggplot(dane_len2, aes(x = genre1, y = runtimeMinutes)) +
      geom_boxplot(fill = "#DBA506", color = "#4B4B4B", outlier.shape = 21,
                   outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
      xlab("Gatunek filmu") +
      ylab("Dlugosc filmu [minuty]") +
      ggtitle("Dlugosc wybranych gatunkow filmowych") +
      #scale_y_log10() +
      geom_hline(yintercept = (mean(dane_len2$runtimeMinutes, na.rm = TRUE)),
                 lty = 2, color = "#4B4B4B",) +
      theme_bw(base_size = 15)
    
  })
  
  output$plotLen3Box <- renderPlot({
    if(input$sortowanieLen3Box == "Malejąco"){
  dane_len3 <- dane %>%
    filter(startYear >= input$selectedYearRangeLen[1] & startYear <= input$selectedYearRangeLen[2]) %>%
    group_by(primaryName1) %>%
    summarise(srednia_dlugosc = mean(runtimeMinutes),
              liczba_filmow = n()) %>%
    arrange(desc(srednia_dlugosc))
  dane_len3 <- dane_len3[1:input$lenliczba,]
  dane_len3 <- dane_len3 %>%
    arrange(srednia_dlugosc)
  dane_len3$primaryName1 <- factor(dane_len3$primaryName1, levels = dane_len3$primaryName1)
  dane_len3_join <- dane %>%
    filter(startYear >= input$selectedYearRangeLen[1] & startYear <= input$selectedYearRangeLen[2]) %>%
    select(c("primaryName1", "primaryTitle","runtimeMinutes"))
  dane_len3 <- dane_len3 %>%
    left_join(dane_len3_join, by = "primaryName1")
  dane_len3$primaryName1 <- factor(dane_len3$primaryName1, levels = unique(dane_len3$primaryName1))
  }else{
    dane_len3 <- dane %>%
      filter(startYear >= input$selectedYearRangeLen[1] & startYear <= input$selectedYearRangeLen[2]) %>%
      group_by(primaryName1) %>%
      summarise(srednia_dlugosc = mean(runtimeMinutes),
                liczba_filmow = n()) %>%
      arrange(srednia_dlugosc)
    dane_len3 <- dane_len3[1:input$lenliczba,]
    dane_len3 <- dane_len3 %>%
      arrange(desc(srednia_dlugosc))
    dane_len3$primaryName1 <- factor(dane_len3$primaryName1, levels = dane_len3$primaryName1)
    dane_len3_join <- dane %>%
      filter(startYear >= input$selectedYearRangeLen[1] & startYear <= input$selectedYearRangeLen[2]) %>%
      select(c("primaryName1", "primaryTitle","runtimeMinutes"))
    dane_len3 <- dane_len3 %>%
      left_join(dane_len3_join, by = "primaryName1")
    dane_len3$primaryName1 <- factor(dane_len3$primaryName1, levels = unique(dane_len3$primaryName1))
  }
  ggplot(dane_len3, aes(x = runtimeMinutes, y = primaryName1)) +
    geom_boxplot(aes(fill = liczba_filmow), color = "#4B4B4B", outlier.shape = 21,
                 outlier.color = "#4B4B4B", outlier.fill = "#DBA506") +
    scale_fill_gradient(low = "white", high = "#DBA506", name = "Liczba filmow") +
    xlab("Dlugosc filmu [minuty]") +
    ylab("Rezyser") +
    ggtitle("Dlugosci filmow wybranych rezyserow") +
    geom_vline(xintercept = mean(dane_len3$runtimeMinutes, na.rm = TRUE),
               lty = 2, color = "#4B4B4B",) +
    theme_bw(base_size = 16)
  })
  
  output$rozkladGatunkowBar <- renderPlot({
    dane_rozklad_gatunkow <- dane
    if (!is.null(input$selectedYearRange)) {
      dane_rozklad_gatunkow <- dane_rozklad_gatunkow %>%
        filter(startYear %in% seq(input$selectedYearRange[1], input$selectedYearRange[2]))
    }
    dane_rozklad_gatunkow$genre1 <- ifelse(dane_rozklad_gatunkow$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_rozklad_gatunkow$genre1)
    dane_rozklad_gatunkow <- dane_rozklad_gatunkow %>%
      count(startYear, genre1) %>%
      group_by(startYear) %>%
      mutate(prop = n / sum(n))
    ggplot(dane_rozklad_gatunkow, aes(x = startYear, y = prop, fill = genre1)) +
               geom_area(position = 'fill', alpha = 0.6) +
               scale_y_continuous(expand = c(0,0), labels = scales::percent_format(scale = 100)) +
               scale_x_continuous(expand = c(0,0)) +
               scale_fill_manual(name = "Gatunek",values =  c("#7a7b7b", "#DBA506", "#fff3b0", "#128bb5", "#c1121f", "#6a994e", "#2f4550", "#bb745a")) +
               labs(title = "Udzial roznych gatunkow filmowych w czasie",
                    x = "Rok",
                    y = "Odsetek") +
               theme_bw(base_size = 16)
    
    
  })
  
  output$plotGen5 <- renderPlot({
    dane_plotGen5 <- dane
    if (!is.null(input$selectedYearRangeGen1)) {
      dane_plotGen5 <- dane_plotGen5 %>%
        filter(startYear %in% seq(input$selectedYearRangeGen1[1], input$selectedYearRangeGen1[2]))
    }
    dane_plotGen5$genre1 <- ifelse(dane_plotGen5$genre1 %in% c("Animation", "Documentary", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"), "Other",dane_plotGen5$genre1)
    
    dane_plotGen5 <- dane_plotGen5 %>%
      group_by(genre1) %>%
      summarise(srednia_budzet = mean(budget, na.rm = TRUE),
                srednia_dochod = mean(gross, na.rm = TRUE))
    
    ggplot(dane_plotGen5, aes(x = genre1)) +
      geom_polygon(aes(y = srednia_dochod, group = 1, fill = "Sredni dochod"), color = "#4B4B4B", alpha = 0.5) +
      geom_polygon(aes(y = srednia_budzet, group = 1, fill = "Sredni budzet"), color = "#4B4B4B", alpha = 0.5) +
      coord_polar() +
      xlab("") +
      ylab("") +
      ggtitle("Sredni budzet i dochod w dolarach dla kazdego gatunku") +
      scale_fill_manual(name = "Wartosc", values = c("Sredni budzet" = "#128bb5", "Sredni dochod" = "#DBA506")) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
    
    
  })
  
}

shinyApp(ui = ui, server = server)
