#library(shiny)
# pacotes de interesse
pacotes = c("chron", "tidyverse","dplyr","readxl", "shinythemes","shinycssloaders")
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel(h1(strong("Infiltração de água no solo: modelo de Kostiakov"),
                              align = "center", style = "font-family: 'Cinzel'")),
                tabsetPanel(tabPanel(h4("Estimação", style = "font-family: 'Cinzel'"), id = "estimacao",
  headerPanel(h4(strong("Estimação por MQO e MNR"),
                 align = "lefter", style = "font-family: 'Cinzel'" )),
  sidebarLayout(
    sidebarPanel(
      fileInput("file",NULL,  buttonLabel = "Buscar",
                label = h4('Insira seus dados:', style = "font-family: 'Cinzel'"),
                accept =  c(".xlsx",".csv"), multiple = TRUE,
                placeholder = "Nenhum arquivo selecionado"),
      #checkboxInput("header", "Header", TRUE),
      hr(),
      fluidRow(column(12, verbatimTextOutput("value"))),
      #radioButtons("bot", label = h4("Escolha a Vairável:"),
       #            choices = list("IA" = 1, "VIM" = 2, "VI" = 3), 
        #           selected = 1),
      selectInput("bot", label = h4('Escolha a variável:', style = "font-family: 'Cinzel'"), 
                  choices = c("IA", "VIM", "VIA"), 
                  selected = "IA"),
      selectInput("select", label = h4('Escolha o método:', style = "font-family: 'Cinzel'"), 
                  choices = c("MQO", "MNR", "AMBOS"), 
                  selected = "AMBOS"),
    ),
    mainPanel(
      plotOutput("plot",   width = "100%",
                 height = "800px")
    ),
  )),
  tabPanel(h4("Informações", style = "font-family: 'Cinzel'"), id = "instrucao",
           p(h3("Leitura dos dados", style = "font-family: 'Cinzel'" )),
           p("Para que seu banco de dados seja lido corretamente é indicado que esteja
             em formato .xlsx. Verifique se as colunas de seu banco de dados estejam corretamente
             nomeadas como Horário, Leitura, QR, respectivamente."),
           p(h3("Variáveis:", style = "font-family: 'Cinzel'")),
           p("IA - Infiltração de água no solo;"),
           p("VIM - Velocidade de Infiltração Média;"),
           p("VIA - Velocidade de Infiltração Aproximada."),
           p(h3("Métodos:", style = "font-family: 'Cinzel'")),
           p("MQO - Mínimos Quadrados Ordinários;"),
           p("MNR - Método de Newton Rapson;"),
           p("AMBOS - Ambos os métodos"),
           p(h3("Autores:",  style = "font-family: 'Cinzel'")),
           p("Rodrigo Domiciano Marques - ",
             a("Marques", href = " http://lattes.cnpq.br/4910468693231396")),
           p("Francisco Canindé Assis de Oliveira - ",
             a("Oliveira", href = " http://lattes.cnpq.br/2686787940801444")),
           p("Mateus Ormondes de Magalhães - ",
             a("Magalhães", href = " http://lattes.cnpq.br/8304916129250402")),
           p("Bárbara Nivalda Palharini Alvim Sousa - ",
             a("Sousa", href = "http://lattes.cnpq.br/5567036140487734")),
           p("Cristian Marcelo Villegas Lobos - ",
            a("Lobos", href = "http://lattes.cnpq.br/7868115035277497"))
           )
)
)
# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    #validate(need(ext == "csv", "Por favor carregue um arquivo .csv"))
    validate(need(ext == "xlsx", "Por favor carregue um arquivo .xlsx"))
    read_excel(input$file$datapath)
    #read.csv2(input$file$datapath)
  })
  
  output$value <- renderPrint({
    req(data()) 
    
    #conversão de horas em minutos
    kostiakov <- function(horario,
                          leitura,
                          QR,
                          metodo = "OLS",
                          Eq.VIm = T,
                          Eq.VIa = F,
                          VIB = F)
      
    {
      #contagem da passagem do tempo
      #library(chron)
      ch <- times(horario)
      horas <- 60 * hours(ch) + minutes(ch)
      #intervalo de tempo
      IT <- c(0,diff(horas))
      
      #tempo acumulado
      acum <- as.vector(NULL)
      for(i in 2:length(horas)){
        acum[i] <- horas[i]-horas[1]   
      }
      acum
      TA <- acum
      
      #Infiltração (Leitura: Leitura na régua)
      infil <- as.vector (NULL)
      for (i in 1:length(leitura)) {
        infil[i] <- leitura[i]-leitura[i+1]   
      }
      
      Infiltracao <- c(0,as.numeric(na.omit(infil+QR[1:length(QR)])))
      
      #Infiltração acumulada
      IA <- NULL
      for (i in 2:length(Infiltracao)) {
        IA[i] <- sum(Infiltracao[1:i])   
      }
      
      #Velocidade média de infiltração
      VIm <- c(NA,round(as.numeric(na.omit(IA)/na.omit(TA)*60),2))
      
      #Velocidade de infiltração aproximada
      VIa <- round(Infiltracao/IT*60,2)
      
      #Velocidade de Infiltração Basal (VIB)
      vib <- VIa[length(VIa)]
      
      tab <- data.frame(Horário = horario,
                        IT,
                        TA,
                        Leitura = leitura,
                        QR,
                        I = Infiltracao,
                        IA,
                        VIm,
                        VIa)
      
      
      In <- na.omit(IA)
      Y <- log(In)
      t <- na.omit(TA)
      X <- log(t)
      
      met <- tolower(metodo)
      aux <- switch(met,
                    "mqo" = "MQO",
                    "mq" = "MQO",
                    "ls" = "MQO",
                    "ols" = "MQO",
                    "mnr" = "MNR",
                    "nr" = "MNR")
      
      if(!any(aux == (c("MQO", "MNR")))) {
        stop("Unsupported method")
      }
      
      #Modelagem linear
      par <- coef(lm(Y~X))
      if (aux == "MQO") {
        Beta <- par
        #Parâmetros da equação de infiltração
        k <- exp(Beta[1])
        a <- Beta[2]
      } else {
        Beta <- coef(nls(IA ~ k * I(TA^a),
                         start = list(k = exp(par[1]), a = par[2]),
                         trace = F))
        k <- Beta[1]
        a <- Beta[2]
      }
      
      
      
      #Parâmetros da equação de velocidade média (VIm = I/T*60 = 60*k*T^(a-1)))
      dk <- 60*k
      da <- a-1 
      
      #Parâmetros da equação de velocidade aproximada (VI = 60*k*a*T^(a-1))
      ka <- 60*k*a
      
      saida <- list(Resumo = tab)
      
      saida$coef <- list(k = as.numeric(k),
                         a = as.numeric(a))
      
      saida$Eq.IA <- paste("I", "[", aux, "]", "==", 
                           round(k, 3),
                           " * t^",
                           round(a, 3),
                           sep = "")
      
      if (Eq.VIm == TRUE) {
        saida$VIm <- paste("VIm", "[", aux, "]", "==",
                           round(dk, 3),
                           " * t^",
                           round(da, 3),
                           sep = "")
      }
      
      if (Eq.VIa == TRUE) {
        saida$VIa <- paste("VIa", "[", aux, "]", "==",
                           round(ka, 3),
                           " * t^",
                           round(da, 3),
                           sep = "")
      }
      
      SQRes.IA <- sum((k*na.omit(TA)^a-na.omit(IA))^2)
      SQRes.Vim <- sum(((60*k*(na.omit(TA)^(a-1)))-(na.omit(IA)/na.omit(TA)*60))^2)
      SQRes.Via <- sum((60*a*k*(na.omit(TA)[-1]^(a-1)) - (diff(na.omit(IA))/diff(na.omit(TA))*60))^2)
      
      saida$SQRes <- data.frame(SQRes.IA, SQRes.Vim, SQRes.Via)
      
      return(saida)
    }
    
    kost1 <- with(data(), kostiakov(horario = Horário,
                                              leitura = Leitura,
                                              QR = QR,
                                              metodo = "MQ",
                                              Eq.VIm = T,
                                              Eq.VIa = T)) 
    kost2 <- with(data(), kostiakov(horario = Horário,
                                              leitura = Leitura,
                                              QR = QR,
                                              metodo = "NR",
                                              Eq.VIm = T,
                                              Eq.VIa = T))
    df <- kost1$Resumo
    str(data())
  })


output$plot <- renderPlot({
  req(data())
  
  #conversão de horas em minutos
  kostiakov <- function(horario,
                        leitura,
                        QR,
                        metodo = "OLS",
                        Eq.VIm = T,
                        Eq.VIa = F,
                        VIB = F)
    
  {
    #contagem da passagem do tempo
    #library(chron)
    ch <- times(horario)
    horas <- 60 * hours(ch) + minutes(ch)
    #intervalo de tempo
    IT <- c(0,diff(horas))
    
    #tempo acumulado
    acum <- as.vector(NULL)
    for(i in 2:length(horas)){
      acum[i] <- horas[i]-horas[1]   
    }
    acum
    TA <- acum
    
    #Infiltração (Leitura: Leitura na régua)
    infil <- as.vector (NULL)
    for (i in 1:length(leitura)) {
      infil[i] <- leitura[i]-leitura[i+1]   
    }
    
    Infiltracao <- c(0,as.numeric(na.omit(infil+QR[1:length(QR)])))
    
    #Infiltração acumulada
    IA <- NULL
    for (i in 2:length(Infiltracao)) {
      IA[i] <- sum(Infiltracao[1:i])   
    }
    
    #Velocidade média de infiltração
    VIm <- c(NA,round(as.numeric(na.omit(IA)/na.omit(TA)*60),2))
    
    #Velocidade de infiltração aproximada
    VIa <- round(Infiltracao/IT*60,2)
    
    #Velocidade de Infiltração Basal (VIB)
    vib <- VIa[length(VIa)]
    
    tab <- data.frame(Horário = horario,
                      IT,
                      TA,
                      Leitura = leitura,
                      QR,
                      I = Infiltracao,
                      IA,
                      VIm,
                      VIa)
    
    
    In <- na.omit(IA)
    Y <- log(In)
    t <- na.omit(TA)
    X <- log(t)
    
    met <- tolower(metodo)
    aux <- switch(met,
                  "mqo" = "MQO",
                  "mq" = "MQO",
                  "ls" = "MQO",
                  "ols" = "MQO",
                  "mnr" = "MNR",
                  "nr" = "MNR")
    
    if(!any(aux == (c("MQO", "MNR")))) {
      stop("Unsupported method")
    }
    
    #Modelagem linear
    par <- coef(lm(Y~X))
    if (aux == "MQO") {
      Beta <- par
      #Parâmetros da equação de infiltração
      k <- exp(Beta[1])
      a <- Beta[2]
    } else {
      Beta <- coef(nls(IA ~ k * I(TA^a),
                       start = list(k = exp(par[1]), a = par[2]),
                       trace = F))
      k <- Beta[1]
      a <- Beta[2]
    }
    
    
    
    #Parâmetros da equação de velocidade média (VIm = I/T*60 = 60*k*T^(a-1)))
    dk <- 60*k
    da <- a-1 
    
    #Parâmetros da equação de velocidade aproximada (VI = 60*k*a*T^(a-1))
    ka <- 60*k*a
    
    saida <- list(Resumo = tab)
    
    saida$coef <- list(k = as.numeric(k),
                       a = as.numeric(a))
    
    saida$Eq.IA <- paste("I", "[", aux, "]", "==", 
                         round(k, 3),
                         " * t^",
                         round(a, 3),
                         sep = "")
    
    if (Eq.VIm == TRUE) {
      saida$VIm <- paste("VIm", "[", aux, "]", "==",
                         round(dk, 3),
                         " * t^",
                         round(da, 3),
                         sep = "")
    }
    
    if (Eq.VIa == TRUE) {
      saida$VIa <- paste("VIa", "[", aux, "]", "==",
                         round(ka, 3),
                         " * t^",
                         round(da, 3),
                         sep = "")
    }
    
    SQRes.IA <- sum((k*na.omit(TA)^a-na.omit(IA))^2)
    SQRes.Vim <- sum(((60*k*(na.omit(TA)^(a-1)))-(na.omit(IA)/na.omit(TA)*60))^2)
    SQRes.Via <- sum((60*a*k*(na.omit(TA)[-1]^(a-1)) - (diff(na.omit(IA))/diff(na.omit(TA))*60))^2)
    
    saida$SQRes <- data.frame(SQRes.IA, SQRes.Vim, SQRes.Via)
    
    return(saida)
  }
  kost1 <- with(data(), kostiakov(horario = Horário,
                                  leitura = Leitura,
                                  QR = QR,
                                  metodo = "MQ",
                                  Eq.VIm = T,
                                  Eq.VIa = T)) 
  kost2 <- with(data(), kostiakov(horario = Horário,
                                  leitura = Leitura,
                                  QR = QR,
                                  metodo = "NR",
                                  Eq.VIm = T,
                                  Eq.VIa = T))
  df <- kost1$Resumo
  #Gráfico 1
  f1 <- function(x) {
    kost1$coef$k*x^kost1$coef$a
  }
  
  f2 <- function(x) {
    kost2$coef$k*x^kost2$coef$a
  }
  
  e1 <- kost1$Eq.IA
  e2 <- kost2$Eq.IA
  
  s1.1 <- paste("SQRes ==", round(kost1$SQRes[1],2))
  s1.2 <- paste("SQRes ==", round(kost2$SQRes[1],2))
  #Gráfico 2
  f1.2 <- function(x) {
    60*kost1$coef$k*x^(kost1$coef$a-1)
  }
  
  f2.2 <- function(x) {
    60*kost2$coef$k*x^(kost2$coef$a-1)
  }
  
  e1.2 <- kost1$VIm
  e2.2 <- kost2$VIm
  s2.1 <- paste("SQRes ==", round(kost1$SQRes[2],2))
  s2.2 <- paste("SQRes ==", round(kost2$SQRes[2],2))
  #Gráfico 3
  f1.3 <- function(x) {
    60*kost1$coef$a*kost1$coef$k*x^(kost1$coef$a-1)
  }
  
  f2.3 <- function(x) {
    60*kost2$coef$a*kost2$coef$k*x^(kost2$coef$a-1)
  }
  
  e1.3 <- kost1$VIa
  e2.3 <- kost2$VIa
  s3.1 <- paste("SQRes ==", round(kost1$SQRes[3],2))
  s3.2 <- paste("SQRes ==", round(kost2$SQRes[3],2))
  
  df.2 <- data.frame(x = df$TA[-1], y = diff(df$IA)/diff(df$TA)*60)
  
  switch (input$bot,
          "IA" = switch (input$select,
                         "MQO" = ggplot(data = df,
                                        mapping = aes(x = TA,
                                                      y = IA)) +
                           geom_point(size = 3) +
                           geom_function(fun =  f1,
                                         size = 1.5,
                                         aes(colour = "MQO")) +
                           annotate("text", 50, 112, label = e1, parse = TRUE, size = 7) +
                           annotate("text", 150, 112, label = s1.1, parse = TRUE, size = 7) +
                           labs(x = "Tempo (min)",
                                y = "Infiltração acumulada (mm) ",
                                title = "Infiltração de água no solo",
                                caption = "Fonte: Gondim et al (2012)") +
                           scale_colour_manual("Método", values = c("red", "blue")) +
                           theme_minimal() +
                           theme(plot.title = element_text(hjust = 0.5, size = 16),
                                 plot.caption = element_text(hjust = 1, size = 12),
                                 axis.line = element_line(colour = "black"),
                                 axis.text = element_text(size = 16, colour = "black"),
                                 axis.title = element_text(size = 16, colour = "black"),
                                 legend.position = "right",
                                 legend.text = element_text(size = 16),
                                 legend.title = element_text(size = 12, hjust = 0.5),
                                 plot.background = element_rect(colour = "black", size = 12)),
                         "MNR" = ggplot(data = df,
                                        mapping = aes(x = TA,
                                                      y = IA)) +
                           geom_point(size = 3) +
                           geom_function(fun = f2,
                                         size = 1.5,
                                         aes(colour = "MNR")) +
                           annotate("text", 50, 112, label = e2, parse = TRUE, size = 7) +
                           annotate("text", 150, 112, label = s1.2, parse = TRUE, size = 7) +
                           labs(x = "Tempo (min)",
                                y = "Infiltração acumulada (mm) ",
                                title = "Infiltração de água no solo",
                                caption = "Fonte: Gondim et al (2012)") +
                           scale_colour_manual("Método", values = c("blue","red")) +
                           theme_minimal() +
                           theme(plot.title = element_text(hjust = 0.5, size = 16),
                                 plot.caption = element_text(hjust = 1, size = 12),
                                 axis.line = element_line(colour = "black"),
                                 axis.text = element_text(size = 16, colour = "black"),
                                 axis.title = element_text(size = 16, colour = "black"),
                                 legend.position = "right",
                                 legend.text = element_text(size = 16),
                                 legend.title = element_text(size = 12, hjust = 0.5),
                                 plot.background = element_rect(colour = "black", size = 5)),
                         "AMBOS" = ggplot(data = df,
                                          mapping = aes(x = TA,
                                                        y = IA)) +
                           geom_point(size = 3) +
                           geom_function(fun =  f1,
                                         size = 1.5,
                                         aes(colour = "MQO")) +
                           geom_function(fun = f2,
                                         size = 1.5,
                                         aes(colour = "MNR")) +
                           annotate("text", 50, 90, label = e1, parse = TRUE, size = 7) +
                           annotate("text", 50, 112, label = e2, parse = TRUE, size = 7) +
                           annotate("text", 150, 90, label = s1.1, parse = TRUE, size = 7) +
                           annotate("text", 150, 112, label = s1.2, parse = TRUE, size = 7) +
                           labs(x = "Tempo (min)",
                                y = "Infiltração acumulada (mm) ",
                                title = "Infiltração de água no solo",
                                caption = "Fonte: Gondim et al (2012)") +
                           scale_colour_manual("Método", values = c("red", "blue")) +
                           theme_minimal() +
                           theme(plot.title = element_text(hjust = 0.5, size = 16),
                                 plot.caption = element_text(hjust = 1, size = 12),
                                 axis.line = element_line(colour = "black"),
                                 axis.text = element_text(size = 16, colour = "black"),
                                 axis.title = element_text(size = 16, colour = "black"),
                                 legend.position = "right",
                                 legend.text = element_text(size = 16),
                                 legend.title = element_text(size = 12, hjust = 0.5),
                                 plot.background = element_rect(colour = "black", size = 5))
          ),
          "VIM" = switch (input$select,
                          "MQO" = ggplot(data = df,
                                         mapping = aes(x = TA,
                                                       y = IA/TA*60)) +
                            geom_point(size = 3) +
                            geom_function(fun =  f1.2,
                                          size = 1.5,
                                          aes(colour = "MQO")) +
                            annotate("text", 50, 112, label = e1.2, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s2.1, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração Média  (cm/h)",
                                 title = "Velocidade de Infiltração Média",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("red", "blue")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5)),
                          "MNR" = ggplot(data = df,
                                         mapping = aes(x = TA,
                                                       y = IA/TA*60)) +
                            geom_point(size = 3) +
                            geom_function(fun = f2.2,
                                          size = 1.5,
                                          aes(colour = "MNR")) +
                            annotate("text", 50, 112, label = e2.2, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s2.2, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração Média  (cm/h)",
                                 title = "Velocidade de Infiltração Média",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("blue","red")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5)),
                          "AMBOS" = ggplot(data = df,
                                           mapping = aes(x = TA,
                                                         y = IA/TA*60)) +
                            geom_point(size = 3) +
                            geom_function(fun =  f1.2,
                                          size = 1.5,
                                          aes(colour = "MQO")) +
                            geom_function(fun = f2.2,
                                          size = 1.5,
                                          aes(colour = "MNR")) +
                            annotate("text", 50, 90, label = e1.2, parse = TRUE, size = 7) +
                            annotate("text", 50, 112, label = e2.2, parse = TRUE, size = 7) +
                            annotate("text", 150, 90, label = s2.1, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s2.2, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração Média  (cm/h)",
                                 title = "Velocidade de Infiltração Média",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("red", "blue")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5))
          ),
          "VIA" = switch (input$select,
                          "MQO" = ggplot(data = df.2,
                                         mapping = aes(x = x,
                                                       y = y)) +
                            geom_point(size = 3) +
                            geom_function(fun =  f1.3,
                                          size = 1.5,
                                          aes(colour = "MQO")) +
                            annotate("text", 50, 112, label = e1.3, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s3.1, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração (cm/h)",
                                 title = "Velocidade de Infiltração Aproximada",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("red", "blue")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5)),
                          "MNR" = ggplot(data = df.2,
                                         mapping = aes(x = x,
                                                       y = y)) +
                            geom_point(size = 3) +
                            geom_function(fun = f2.3,
                                          size = 1.5,
                                          aes(colour = "MNR")) +
                            annotate("text", 50, 112, label = e2.3, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s3.2, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração (cm/h)",
                                 title = "Velocidade de Infiltração Aproximada",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("blue", "red")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5)),
                          "AMBOS" = ggplot(data = df.2,
                                           mapping = aes(x = x,
                                                         y = y)) +
                            geom_point(size = 3) +
                            geom_function(fun =  f1.3,
                                          size = 1.5,
                                          aes(colour = "MQO")) +
                            geom_function(fun = f2.3,
                                          size = 1.5,
                                          aes(colour = "MNR")) +
                            annotate("text", 50, 90, label = e1.3, parse = TRUE, size = 7) +
                            annotate("text", 50, 112, label = e2.3, parse = TRUE, size = 7) +
                            annotate("text", 150, 90, label = s3.1, parse = TRUE, size = 7) +
                            annotate("text", 150, 112, label = s3.2, parse = TRUE, size = 7) +
                            labs(x = "Tempo (min)",
                                 y = "Velocidade de Infiltração (cm/h)",
                                 title = "Velocidade de Infiltração Aproximada",
                                 caption = "Fonte: Gondim et al (2012)") +
                            scale_colour_manual("Método", values = c("red", "blue")) +
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5, size = 16),
                                  plot.caption = element_text(hjust = 1, size = 12),
                                  axis.line = element_line(colour = "black"),
                                  axis.text = element_text(size = 16, colour = "black"),
                                  axis.title = element_text(size = 16, colour = "black"),
                                  legend.position = "right",
                                  legend.text = element_text(size = 16),
                                  legend.title = element_text(size = 12, hjust = 0.5),
                                  plot.background = element_rect(colour = "black", size = 5))
          )
  )

})
}
# Run the app ----
shinyApp(ui = ui, server = server)