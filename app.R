# Packages ----------------------------------------------------------------             

library(shiny)
library(scales)

# UI ----------------------------------------------------------------             

ui <- navbarPage(
  
  "Calculadoras de Distribuições de Probabilidade",
  
  navbarMenu("Distribuições Contínuas",
             
# Normal ------------------------------------------------------------------
             
             tabPanel(
               
               "Distribuição Normal",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "xlower",
                            label = "Limite inferior de x",
                            value = -Inf
                          ),
                          
                          numericInput(
                            inputId = "xupper",
                            label = "Limite superior de x",
                            value = Inf
                          ),
                          
                          numericInput(
                            inputId = "mu",
                            label = "Média",
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = "sd",
                            label = "Desvio padrão",
                            value = 1,
                            min = .00000000001
                          ),
                          
                          checkboxInput(
                            inputId = "tail_norm",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("norm"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot")
                          
                        )
                 )
               )
             ),
             
             
# Q-Norm ------------------------------------------------------------------
             
             tabPanel(
               
               "Quantis da Distribuição Normal",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "mu_q",
                            label = "Média",
                            value = 0
                          ),
                          
                          numericInput(
                            inputId = "sd_q",
                            label = "Desvio padrão",
                            value = 1,
                            min = .00000000001
                          ),
                          
                          checkboxInput(
                            inputId = "tail_q_norm",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("norm_q"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_q")
                        )
                 )
               )
             ),
             
# t-Student ---------------------------------------------------------------
             
             tabPanel(
               
               "Distribuição t-Student",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_t_lower",
                            label = "Limite inferior de x",
                            value = -Inf
                          ),
                          
                          numericInput(
                            inputId = "x_t_upper",
                            label = "Limite superior de x",
                            value = Inf
                          ),
                          
                          numericInput(
                            inputId = "df_t",
                            label = "Graus de liberdade",
                            value = 5,
                            min = 2
                          ),
                          
                          checkboxInput(
                            inputId = "tail_t",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("t"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_t")
                          
                        )
                 )
               )
             ),
             
             
# Q-t ------------------------------------------------------------------
             
             tabPanel(
               
               "Quantis da Distribuição t-Student",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_t",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "df_qt",
                            label = "Graus de liberdade",
                            value = 9,
                            min = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_q_t",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("t_q"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qt")
                        )
                 )
               )
             ),
             
# X-Square ----------------------------------------------------------------
             
             
             tabPanel(
               
               "Distribuição Chi quadrado",
               
               fluidPage(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_chisq_lower",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_chisq_upper",
                            label = "Limite superior de x",
                            value = 5,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "df_chisq",
                            label = "Graus de liberdade",
                            value = 2,
                            min = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_chisq",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("chisq"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )          
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_chisq")
                          
                        )
                 )
               )
             ),
             
# Q-X ------------------------------------------------------------------
             
             tabPanel(
               
               "Quantis da Distribuição Chi-Square",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_qchisq",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "df_qchisq",
                            label = "Graus de liberdade",
                            value = 1,
                            min = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_qchisq",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("chisq_q"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qchisq")
                        )
                 )
               )
             ),
             
# Exponencial -------------------------------------------------------------
             
             tabPanel(
               
               "Distribuição Exponencial",
               
               fluidPage(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_exp_lower",
                            label = "Limite inferior de x",
                            value = -Inf,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_exp_upper",
                            label = "Limite superior de x",
                            value = Inf,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "lambda_exp",
                            label = "Taxa",
                            value = 2,
                            min = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_exp",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("exp"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )          
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_exp")
                        )
                 )
               )
             ),
             
             
# Q-Exp ------------------------------------------------------------------
             
             tabPanel(
               
               "Quantis da Distribuição Exponencial",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_qexp",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "lambda_qexp",
                            label = "Lambda",
                            value = 1,
                            min = .00000000001
                          ),
                          
                          checkboxInput(
                            inputId = "tail_q_exp",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("exp_q"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qexp")
                        )
                 )
               )
             ),
             
# Gamma -------------------------------------------------------------------
             
             tabPanel(
               
               "Distribuição Gama",
               
               fluidPage(
                 
                 column(3,
                        
                        wellPanel(
                          numericInput(
                            inputId = "x_gamma_lower",
                            label = "Limite inferior de x",
                            value = -Inf,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_gamma_upper",
                            label = "Limite superior de x",
                            value = Inf,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "gamma_alpha",
                            label = "Alfa",
                            value = 2,
                            min = .0001
                          ),
                          
                          numericInput(
                            inputId = "gamma_beta",
                            label = "Beta",
                            value = 4,
                            min = .0001
                          ),
                          
                          checkboxInput(
                            inputId = "tail_gamma",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("gamma"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )          
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_gamma")
                        )
                 )
               )
             ),
             
# Q-Gamma ------------------------------------------------------------------
             
             tabPanel(
               
               "Quantis da Distribuição Gamma",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_qgamma",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "alpha_q",
                            label = "Alpha",
                            value = 1,
                            min = .00000000001
                          ),
                          
                          numericInput(
                            inputId = "beta_q",
                            label = "Beta",
                            value = .2,
                            min = .00000000001
                          ),
                          
                          checkboxInput(
                            inputId = "tail_q_gamma",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("gamma_q"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qgamma")
                        )
                 )
               )
             )),
  
# Discretas ---------------------------------------------------------------
  
  navbarMenu("Distribuições Discretas",
             
# Binomial ----------------------------------------------------------------             
             
             tabPanel(
               
               "Distribuição Binomial",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_binom_lower",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_binom_upper",
                            label = "Limite superior de x",
                            value = 1
                          ),
                          
                          numericInput(
                            inputId = "n_binom",
                            label = "Número de ensaios (n)",
                            value = 1,
                            min = 1
                          ),
                          
                          numericInput(
                            inputId = "p_binom",
                            label = "Probabilidade de sucesso (p)",
                            value = .5,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_binom",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("binom"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_binom")
                          
                        )
                 )
               )
             ),
             
# Q-Binomial ----------------------------------------------------------------             
             
             tabPanel(
               
               "Quantis da Distribuição Binomial",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_binomq",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "n_qbinom",
                            label = "Número de ensaios (n)",
                            value = 1,
                            min = 1
                          ),
                          
                          numericInput(
                            inputId = "p_qbinom",
                            label = "Probabilidade de sucesso (p)",
                            value = .5,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_qbinom",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("qbinom"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qbinom")
                          
                        )
                 )
               )
             ),
             
# Poisson ----------------------------------------------------------------             
             
             tabPanel(
               
               "Distribuição Poisson",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_pois_lower",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_pois_upper",
                            label = "Limite superior de x",
                            value = 1
                          ),
                          
                          numericInput(
                            inputId = "lambda_pois",
                            label = withMathJax("$$\\lambda$$"),
                            value = 1,
                            min = 0
                          ),
                          
                          checkboxInput(
                            inputId = "tail_pois",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("pois"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_pois")
                          
                        )
                 )
               )
             ),
             
# Q-Poisson ----------------------------------------------------------------             
             
             tabPanel(
               
               "Quantis da Distribuição Poisson",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_pois",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "lambda_qpois",
                            label = withMathJax("$$\\lambda$$"),
                            value = 1,
                            min = 0
                          ),
                          
                          checkboxInput(
                            inputId = "tail_qpois",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("qpois"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qpois")
                          
                        )
                 )
               )
             ),
             
# Geom 1 ----------------------------------------------------------------             
             
             tabPanel(
               
               "Distribuição Geométrica (Ensaios antes do primeiro sucesso)",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_geom1_lower",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_geom1_upper",
                            label = "Limite superior de x",
                            value = 1
                          ),
                          
                          numericInput(
                            inputId = "p_geom1",
                            label = "Probabilidade de sucesso (p)",
                            value = .2,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_geom1",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("geom1"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_geom1")
                          
                        )
                 )
               )
             ),
             
# Q-Geom 1 ----------------------------------------------------------------             
             
             tabPanel(
               
               "Quantis da Distribuição Geométrica (Ensaios antes do primeiro sucesso)",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_qgeom1",
                            label = "Limite inferior de x",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "p_geom1q",
                            label = "Probabilidade de sucesso (p)",
                            value = .2,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_qgeom1",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("qgeom1"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qgeom1")
                          
                        )
                 )
               )
             ),
             
# Geom 2 ----------------------------------------------------------------             
             
             tabPanel(
               
               "Distribuição Geométrica (Ensaios até o primeiro sucesso)",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_geom2_lower",
                            label = "Limite inferior de x",
                            value = 1,
                            min = 1
                          ),
                          
                          numericInput(
                            inputId = "x_geom2_upper",
                            label = "Limite superior de x",
                            value = 2
                          ),
                          
                          numericInput(
                            inputId = "p_geom2",
                            label = "Probabilidade de sucesso (p)",
                            value = .2,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_geom2",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("geom2"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_geom2")
                          
                        )
                 )
               )
             ),
             
# Q-Geom 2 ----------------------------------------------------------------             
             
             tabPanel(
               
               "Quantis da Distribuição Geométrica (Ensaios até o primeiro sucesso)",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "p_qgeom2",
                            label = "Probabilidade",
                            value = .5,
                            min = 0,
                            max = 1,
                            step = .01
                          ),
                          
                          numericInput(
                            inputId = "p_geom2q",
                            label = "Probabilidade de sucesso (p)",
                            value = .2,
                            min = 0,
                            max = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_qgeom2",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("qgeom2"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_qgeom2")
                          
                        )
                 )
               )
             ),
             
# HyperGeom ----------------------------------------------------------------             
             
             tabPanel(
               
               "Distribuição Hipergeométrica",
               
               fluidRow(
                 
                 column(3,
                        
                        wellPanel(
                          
                          numericInput(
                            inputId = "x_hyper_lower",
                            label = "Limite inferior de x",
                            value = 0,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "x_hyper_upper",
                            label = "Limite superior de x",
                            value = 1
                          ),
                          
                          numericInput(
                            inputId = "m_hyper",
                            label = "Total de elementos de interesse (m)",
                            value = 3,
                            min = 0
                          ), 
                          
                          numericInput(
                            inputId = "r_hyper",
                            label = "Total de elementos que não são de interesse (r)",
                            value = 5,
                            min = 0
                          ),
                          
                          numericInput(
                            inputId = "k_hyper",
                            label = "Total de elementos extraídos (k)",
                            value = 1,
                            min = 1
                          ),
                          
                          checkboxInput(
                            inputId = "tail_hyper",
                            label = "Cauda inferior (P[X < x])",
                            value = TRUE
                          ),
                          
                          verbatimTextOutput("hyper"),
                          
                          submitButton(
                            text = "Calcular!"
                          )
                        )
                 ),
                 
                 column(9,
                        
                        wellPanel(
                          
                          plotOutput("plot_hyper")
                          
                        )
                 )
               )
             ),

# Q-HyperGeom ----------------------------------------------------------------             

tabPanel(
  
  "Quantis da Distribuição Hipergeométrica",
  
  fluidRow(
    
    column(3,
           
           wellPanel(
             
             numericInput(
               inputId = "p_hyper",
               label = "Probabilidade",
               value = 0,
               min = 0,
               step = .01
             ),
             
             numericInput(
               inputId = "m_qhyper",
               label = "Total de elementos de interesse (m)",
               value = 3,
               min = 0
             ), 
             
             numericInput(
               inputId = "r_qhyper",
               label = "Total de elementos que não são de interesse (r)",
               value = 5,
               min = 0
             ),
             
             numericInput(
               inputId = "k_qhyper",
               label = "Total de elementos extraídos (k)",
               value = 1,
               min = 1
             ),
             
             checkboxInput(
               inputId = "tail_qhyper",
               label = "Cauda inferior (P[X < x])",
               value = TRUE
             ),
             
             verbatimTextOutput("qhyper"),
             
             submitButton(
               text = "Calcular!"
             )
           )
    ),
    
    column(9,
           
           wellPanel(
             
             plotOutput("plot_qhyper")
             
           )
    )
  )
)

  )
)

# Server ----------------------------------------------------------------             

server <- function(input, output){
  
  # Normal - Plot ----------------------------------------------------------------               
  
  output$plot <- renderPlot({
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$xlower)){
        if(input$tail_norm == FALSE){
          lower <- input$xlower
          upper <- input$mu + 4*input$sd
        }
        else{
          lower <- input$mu - 4*input$sd
          upper <- input$xlower
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$xupper)){
        if(input$tail_norm == FALSE){
          lower <- input$xupper
          upper <- input$mu + 4*input$sd
        }
        else{
          lower <- input$mu - 4*input$sd
          upper <- input$xupper
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    y <- dnorm(x, input$mu, input$sd)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    crv_norm <- curve(dnorm(x, input$mu, input$sd), xlim = c(input$mu - 4*input$sd, input$mu + 4*input$sd), ylab = "Probabilidade")
    
    polygon(x = xcoord,
            y = ycoord,
            col = col, border = border)
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      lab <- pnorm(upper, input$mu, input$sd) - pnorm(lower, input$mu, input$sd)
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        lab <- 1 - pnorm(lower, input$mu, input$sd, input$tail_norm == FALSE)
      }
      else if(is.finite(input$xupper)){
        upper <- input$xupper
        lab <- pnorm(upper, input$mu, input$sd, input$tail_norm)
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lab <- 1
      }
    }
    
    text(x = median(crv_norm$x),
         y = max(crv_norm$y)/2,
         labels = round(lab, 4),
         cex = 2.5)
    
  })
  
  # Normal - Output ----------------------------------------------------------------             
  
  output$norm <- renderPrint({
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      pnorm(upper, input$mu, input$sd) - pnorm(lower, input$mu, input$sd)
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        1 - pnorm(lower, input$mu, input$sd, input$tail_norm == FALSE)
      }
      else if(is.finite(input$xupper)){
        upper <- input$xupper
        pnorm(upper, input$mu, input$sd, input$tail_norm)
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
      }
    }
  })
  
  # Q-Norm - Plot ----------------------------------------------------------------             
  
  output$plot_q <- renderPlot({
    
    crv_q <- curve(dnorm(x, input$mu_q, input$sd_q), xlim = c(input$mu_q - 4*input$sd_q, input$mu_q + 4*input$sd_q), ylab = "Probabilidade")
    
    y_q <- qnorm(input$p, input$mu_q, input$sd_q, lower.tail = input$tail_q_norm)
    
    if(input$tail_q_norm == FALSE){
      x_q <- seq(from = qnorm(input$p, input$mu_q, input$sd_q, lower.tail = FALSE), to = input$mu_q + 4*input$sd_q, length.out = 1000)
    }
    else{
      x_q <- seq(from = input$mu_q - 4*input$sd_q, to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dnorm(x_q, input$mu_q, input$sd_q)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 2.5)
    
  })
  
  # Q-Norm - Output ----------------------------------------------------------------             
  
  output$norm_q <- renderPrint({
    
    qnorm(input$p, input$mu_q, input$sd_q, lower.tail = input$tail_q_norm)
    
  })
  
  # t-Student - Plot ----------------------------------------------------------------             
  
  output$plot_t <- renderPlot({
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_t_lower)){
        if(input$tail_t){
          lower <- -6
          upper <- input$x_t_lower
        }
        else{
          lower <- input$x_t_lower
          upper <- 6
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_t_upper)){
        if(input$tail_t){
          lower <- -6
          upper <- input$x_t_upper
        }
        else{
          lower <- input$x_t_upper
          upper <- 6
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lower <- -6
        upper <- 6
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_t <- curve(dt(x, input$df_t), xlim = c(-4 * input$df_t/(input$df_t - 2), 4 * input$df_t/(input$df_t - 2)), ylab = "Probabilidade")
    
    y <- dt(x, input$df_t)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      lab <- pt(upper, input$df_t) - pt(lower, input$df_t)
    }
    else{
      if(is.finite(input$x_t_lower)){
        lower <- input$x_t_lower
        lab <- 1 - pt(lower, input$df_t, lower = input$tail_t == FALSE)
      }
      else if(is.finite(input$x_t_upper)){
        upper <- input$x_t_upper
        lab <- pt(upper, input$df_t, lower = input$tail_t)
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lab <- 1
      }
    }
    
    text(x = median(crv_t$x),
         y = max(crv_t$y)/2,
         labels = round(lab, 4),
         cex = 2.5)
    
  })
  
  # t-Student - Output ----------------------------------------------------------------             
  
  output$t <- renderPrint({
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      pt(upper, input$df_t) - pt(lower, input$df_t)
    }
    else{
      if(is.finite(input$x_t_lower)){
        lower <- input$x_t_lower
        1 - pt(lower, input$df_t, lower = input$tail_t == FALSE)
      }
      else if(is.finite(input$x_t_upper)){
        upper <- input$x_t_upper
        pt(upper, input$df_t, lower = input$tail_t)
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
      }
    }
    
  })
  
  # Q-t - Plot ----------------------------------------------------------------             
  
  output$plot_qt <- renderPlot({
    
    crv_qt <- curve(dt(x, input$df_qt), xlim = c(-4, 4), ylab = "Probabilidade")
    
    y_q <- qt(input$p_t, input$df_qt, lower.tail = input$tail_q_t)
    
    if(input$tail_q_t == FALSE){
      x_q <- seq(from = qt(input$p_t, input$df_qt, lower.tail = FALSE), to = qt(.99999, input$df_qt), length.out = 1000)
    } else {
      x_q <- seq(from = qt(.00001, input$df_qt), to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dt(x_q, input$df_qt)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 2.5)
    
  })
  
  # Q-t - Output ----------------------------------------------------------------             
  
  output$t_q <- renderPrint({
    
    qt(input$p_t, input$df_t, lower.tail = input$tail_q_t)
    
  })
  
  # X-Square - Plot ----------------------------------------------------------------             
  
  output$plot_chisq <- renderPlot({
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        if(input$tail_chisq){
          lower <- 0
          upper <- input$x_chisq_lower
        }
        else{
          lower <- input$x_chisq_lower
          upper <- input$df_chisq * 8
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_chisq_upper)){
        if(input$tail_chisq){
          lower <- 0
          upper <- input$x_chisq_upper
        }
        else{
          lower <- input$x_chisq_upper
          upper <- input$df_chisq * 8
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lower <- 0
        upper <- input$df_chisq * 8
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_chisq <- curve(dchisq(x, input$df_chisq), xlim = c(0, 8 * input$df_chisq), ylab = "Probabilidade")
    
    x <- seq(from = lower, to = upper, length.out = 1000)
    
    y <- dchisq(x, input$df_chisq)
    
    if(input$df_chisq == 1 & x[1] == 0){
      y[1] <- 0
    }
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      lab <- pchisq(upper, input$df_chisq) - pchisq(lower, input$df_chisq)
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        lower <- input$x_chisq_lower
        lab <- 1 - pchisq(lower, input$df_chisq, lower = input$tail_chisq == FALSE)
      }
      else if(is.finite(input$x_chisq_upper)){
        upper <- input$x_chisq_upper
        lab <- pchisq(upper, input$df_chisq, lower = input$tail_chisq)
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lab <- 1
      }
    }
    
    text(x = median(crv_chisq$x),
         y = max(crv_chisq$y)/2,
         labels = round(lab, 4),
         cex = 2.5)
    
  })
  
  # X-Square - Output ----------------------------------------------------------------             
  
  output$chisq <- renderPrint({
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      pchisq(upper, input$df_chisq) - pchisq(lower, input$df_chisq)
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        lower <- input$x_chisq_lower
        1 - pchisq(lower, input$df_chisq, lower = input$tail_chisq == FALSE)
      }
      else if(is.finite(input$x_chisq_upper)){
        upper <- input$x_chisq_upper
        pchisq(upper, input$df_chisq, lower = input$tail_chisq)
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
        pchisq(upper, input$df_chisq, lower = input$tail_chisq) - pchisq(lower, input$df_chisq, lower = input$tail_chisq)
      }
    }
    
  })
  
  # Q-X - Plot ----------------------------------------------------------------             
  
  output$plot_qchisq <- renderPlot({
    
    if(input$df_qchisq == 1){
      chisq_x <- seq(from = 0, to = 5, length.out = 1000)
      
      chisq_y <- dchisq(chisq_x, input$df_qchisq)
      
      chisq_y[which(chisq_y > 1)] <- 0
      
      plot(chisq_y ~ chisq_x, t = "l", ylim = c(0, 2))
    } else {  
    crv_q <- curve(dchisq(x, input$df_qchisq), xlim = c(qchisq(.00001, input$df_qchisq), qchisq(.99999, input$df_qchisq)),
                   ylab = "Probabilidade")
    }
    
    y_q <- qchisq(input$p_qchisq, input$df_qchisq, lower.tail = input$tail_qchisq)
    
    if(input$tail_qchisq == FALSE){
      x_q <- seq(from = qchisq(input$p_qchisq, input$df_qchisq, lower.tail = FALSE), to = qchisq(.99999, input$df_qchisq), length.out = 1000)
    } else {
      x_q <- seq(from = qchisq(.00001, input$df_qchisq), to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dchisq(x_q, input$df_qchisq)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 2.5)
    
  })
  
  # Q-X - Output ----------------------------------------------------------------             
  
  output$chisq_q <- renderPrint({
    
    qchisq(input$p_qchisq, input$df_qchisq, lower.tail = input$tail_qchisq)
    
  })
  
  # Expon - Plot ----------------------------------------------------------------             
  
  output$plot_exp <- renderPlot({
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_exp_lower)){
        if(input$tail_exp){
          lower <- 0
          upper <- input$x_exp_lower
        } else {
          lower <- input$x_exp_lower
          upper <- input$lambda_exp
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      } else if (is.finite(input$x_exp_upper)){
        if(input$tail_exp){
          lower <- 0
          upper <- input$x_exp_upper
        } else {
          lower <- input$x_exp_upper
          upper <- input$lambda_exp
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lower <- 0
        upper <- input$lambda_exp
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_exp <- curve(dexp(x, input$lambda_exp), xlim = c(0, input$lambda_exp), ylab = "Probabilidade")
    
    y <- dexp(x, input$lambda_exp)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      lab <- pexp(upper, input$lambda_exp) - pexp(lower, input$lambda_exp)
    } else {
      if(is.finite(input$x_exp_lower)){
        lower <- input$x_exp_lower
        lab <- 1 - pexp(lower, input$lambda_exp, lower = input$tail_exp == FALSE)
      } else if (is.finite(input$x_exp_upper)){
        upper <- input$x_exp_upper
        lab <- pexp(upper, input$lambda_exp, lower = input$tail_exp)
      } else if (input$lambda_exp == 0){
        lab <- 0
      } else if (!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lab <- 1
      }
    }
    
    text(x = median(crv_exp$x),
         y = max(crv_exp$y)/2,
         labels = round(lab, 4),
         cex = 2.5)
    
  })
  
  # Expon - Output ----------------------------------------------------------------             
  
  output$exp <- renderPrint({
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      pexp(upper, input$lambda_exp) - pexp(lower, input$lambda_exp)
    }
    else{
      if(is.finite(input$x_exp_lower)){
        lower <- input$x_exp_lower
        1 - pexp(lower, input$lambda_exp, lower = input$tail_exp == FALSE)
      }
      else if(is.finite(input$x_exp_upper)){
        upper <- input$x_exp_upper
        pexp(upper, input$lambda_exp, lower = input$tail_exp)
      }
      else if(!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lower <- 0
        upper <- input$mu + 4*input$sd
        pexp(upper, input$lambda_exp, lower = input$tail_exp) - pexp(lower, input$lambda_exp, lower = input$tail_exp)
      }
    }
    
  })
  
  
  # Q-Expon - Plot ----------------------------------------------------------------             
  
  output$plot_qexp <- renderPlot({
    
    crv_q <- curve(dexp(x, input$lambda_qexp), xlim = c(qexp(.00001, input$lambda_qexp), qexp(.99999, input$lambda_qexp)),
                   ylab = "Probabilidade")
    
    y_q <- qexp(input$p_qexp, input$lambda_qexp, lower.tail = input$tail_q_exp)
    
    if(input$tail_q_exp == FALSE){
      x_q <- seq(from = qexp(input$p_qexp, input$lambda_qexp, lower.tail = FALSE), to = qexp(.99999, input$lambda_qexp), length.out = 1000)
    }
    else{
      x_q <- seq(from = qexp(.00001, input$lambda_qexp), to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dexp(x_q, input$lambda_qexp)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 2.5)
    
  })
  
  # Q-Expon - Output ----------------------------------------------------------------             
  
  output$exp_q <- renderPrint({
    
    qexp(input$p_qexp, input$lambda_qexp, lower.tail = input$tail_q_exp)
    
  })
  
  # Gamma - Plot ----------------------------------------------------------------             
  
  output$plot_gamma <- renderPlot({
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        if(input$tail_gamma){
          lower <- 0
          upper <- input$x_gamma_lower
        }
        else{
          lower <- input$x_gamma_lower
          upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_gamma_upper)){
        if(input$tail_gamma){
          lower <- 0
          upper <- input$x_gamma_upper
        }
        else{
          lower <- input$x_gamma_upper
          upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lower <- 0
        upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_gamma <- curve(dgamma(x, shape = input$gamma_alpha, scale = input$gamma_beta),
                       xlim = c(0, qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)),
                       ylab = "Probabilidade")
    
    x <- seq(from = lower, to = upper, length.out = 1000)
    
    y <- dgamma(x, shape = input$gamma_alpha, scale = input$gamma_beta)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      lab <- pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta) -
        pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta)
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        lower <- input$x_gamma_lower
        lab <- 1 - pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma == FALSE)
      }
      else if(is.finite(input$x_gamma_upper)){
        upper <- input$x_gamma_upper
        lab <- pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
      else if(input$gamma_alpha == 0 | input$gamma_beta == 0){
        lab <- 0
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lab <- 1
      }
    }
    
    text(x = median(crv_gamma$x),
         y = max(crv_gamma$y)/2,
         labels = round(lab, 4),
         cex = 2.5)
    
  })
  
  # Gamma - Output ----------------------------------------------------------------             
  
  output$gamma <- renderPrint({
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta) -
        pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta)
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        lower <- input$x_gamma_lower
        1 - pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma == FALSE)
      }
      else if(is.finite(input$x_gamma_upper)){
        upper <- input$x_gamma_upper
        pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lower <- 0
        upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma) -
          pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
    }
    
  })
  
  
  # Q-Gamma - Plot ----------------------------------------------------------------             
  
  output$plot_qgamma <- renderPlot({
    
    crv_q <- curve(dgamma(x, shape = input$alpha_q, scale = input$beta_q),
                   xlim = c(qgamma(.00001, shape = input$alpha_q, scale = input$beta_q),
                            qgamma(.99999, shape = input$alpha_q, scale = input$beta_q)),
                   ylab = "Probabilidade")
    
    y_q <- qgamma(input$p_qgamma, shape = input$alpha_q, scale = input$beta_q, lower.tail = input$tail_q_gamma)
    
    if(input$tail_q_gamma == FALSE){
      x_q <- seq(from = qgamma(input$p_qgamma, shape = input$alpha_q, scale = input$beta_q, lower.tail = FALSE),
                 to = qgamma(.99999, shape = input$alpha_q, scale = input$beta_q), length.out = 1000)
    }
    else{
      x_q <- seq(from = qgamma(.00001, shape = input$alpha_q, scale = input$beta_q), to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dgamma(x_q, shape = input$alpha_q, scale = input$beta_q)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 2.5)
    
  })
  
  # Q-Gamma - Output ----------------------------------------------------------------             
  
  output$gamma_q <- renderPrint({
    
    qgamma(input$p_qgamma, shape = input$alpha_q, scale = input$beta_q, lower.tail = input$tail_q_gamma)
    
  })
  
  # Binomial - Plot ----------------------------------------------------------------             
  
  output$plot_binom <- renderPlot({
    
    if(is.finite(input$x_binom_lower) & is.finite(input$x_binom_upper)){
      lower <- input$x_binom_lower
      upper <- input$x_binom_upper
      if(upper - lower < 5){
        x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
      } else {
        x <- seq(from = lower, to = upper, by = 1)
      }
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_binom_lower)){
        if(input$tail_binom){
          lower <- 0
          upper <- input$x_binom_lower
        }
        else{
          lower <- input$x_binom_lower
          upper <- qbinom(.999, size = input$n_binom, prob = input$p_binom)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_binom_upper)){
        if(input$tail_binom){
          lower <- 0
          upper <- input$x_binom_upper
        }
        else{
          lower <- input$x_binom_upper
          upper <- qbinom(.999, size = input$n_binom, prob = input$p_binom)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_binom_lower) & !is.finite(input$x_binom_lower)){
        lower <- 0
        upper <- qbinom(.999, size = input$n_binom, prob = input$p_binom)
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col = NA
        border = NA
      }
    }
    
    h.plot_binom <- plot(dbinom(x, size = input$n_binom, prob = input$p_binom) ~ x, t = "h", axes = FALSE,
                         ylab = "Probabilidade", xlim = c(min(x), max(x)), xlab = "X")
    axis(1, x); axis(2, h.plot_binom); box()
    
    xcoord <- c(lower:upper)
    
    y <- dbinom(xcoord, size = input$n_binom, prob = input$p_binom)
    ycoord <- y
    
    lines(x = xcoord,
          y = ycoord,
          col = "red", lwd = 2, t = "h")
    
    if(is.finite(input$x_binom_lower) & is.finite(input$x_binom_upper)){
      lower <- input$x_binom_lower
      upper <- input$x_binom_upper
      lab <- sum(dbinom(c(lower:upper), input$n_binom, input$p_binom))
    } else{
      if(is.finite(input$x_binom_lower)){
        lower <- input$x_binom_lower
        lab <- 1 - pbinom(lower, size = input$n_binom, prob = input$p_binom, lower = input$tail_binom == FALSE)
      }
      else if(is.finite(input$x_binom_upper)){
        upper <- input$x_binom_upper
        lab <- pbinom(upper, size = input$n_binom, prob = input$p_binom, lower = input$tail_binom)
      }
      else if(input$binom_alpha == 0 | input$binom_beta == 0){
        lab <- 0
      }
      else if(!is.finite(input$x_binom_lower) & !is.finite(input$x_binom_lower)){
        lab <- 1
      }
    }
    
    legend("topright", legend = round(lab, 4), bty = "n", cex = 2.5)
    
    tit <- ifelse(test = is.finite(input$x_binom_lower) & is.finite(input$x_binom_upper) &
                    !input$x_binom_lower == input$x_binom_upper, 
                  yes = paste0("P[", input$x_binom_lower, " < X < ", input$x_binom_upper, "]"),
                  no = ifelse(test = is.finite(input$x_binom_lower) & is.finite(input$x_binom_upper) & 
                                input$x_binom_lower == input$x_binom_upper, 
                              yes = paste0("P[X = ", input$x_binom_lower, "]"), 
                              no = ifelse(test = is.finite(input$x_binom_lower),
                                          yes = ifelse(test = input$tail_binom,
                                                       yes = paste0("P[X < ", input$x_binom_lower, "]"),
                                                       no = paste0("1 - P[X < ", input$x_binom_lower, "]")),
                                          no = ifelse(test = is.finite(input$x_binom_lower), 
                                                      yes = ifelse(test = input$tail_binom,
                                                                   yes = paste0("P[X < ", input$x_binom_lower, "]"),
                                                                   no = paste0("1 - P[X <", input$x_binom_lower, "]")),
                                                      no = ""))))
    
    title(tit)
    
  })
  
  # Binomial - Output ----------------------------------------------------------------             
  
  output$binom <- renderPrint({
    
    if(is.finite(input$x_binom_lower) & is.finite(input$x_binom_upper)){
      lower <- input$x_binom_lower
      upper <- input$x_binom_upper
      lab <- sum(dbinom(c(lower:upper), input$n_binom, input$p_binom))
    } else {
      if(is.finite(input$x_binom_lower)){
        lower <- input$x_binom_lower
        lab <- 1 - pbinom(lower, size = input$n_binom, prob = input$p_binom, lower = input$tail_binom == FALSE)
      } else if (is.finite(input$x_binom_upper)){
        upper <- input$x_binom_upper
        lab <- pbinom(upper, size = input$n_binom, prob = input$p_binom, lower = input$tail_binom)
      }
    }
    
    lab
    
  })
  
  
  # Q-Binom - Plot ----------------------------------------------------------
  
  output$plot_qbinom <- renderPlot({
    
    med_seq <- input$n_qbinom*input$p_qbinom
    
    seqx <- seq(from = round((med_seq - 7)*(med_seq >= 7), 0), to = round(med_seq + 7, 0), by = 1)
    
    plot_bars <- plot(dbinom(seqx, input$n_qbinom, input$p_qbinom) ~ seqx, t = "h", lwd = 2, ylab = "Probabilidades", xlab = "X")
    
    q_plot <- qbinom(input$p_binomq, input$n_qbinom, input$p_qbinom, input$tail_qbinom)
    
    if(input$tail_qbinom){
      seqx2 <- seq(from = min(seqx), to = q_plot, by = 1)
    } else {
      seqx2 <- seq(from = q_plot + 1, to = max(seqx), by = 1)
    }
    
    
    lines(dbinom(seqx2, input$n_qbinom, input$p_qbinom) ~ seqx2, t = "h", col = "red", lwd = 2)
    
  })
  
  # Q-Binom - Output --------------------------------------------------------
  
  output$qbinom <-renderPrint({
    qbinom(input$p_binomq, input$n_qbinom, input$p_qbinom, input$tail_qbinom)
  })
  
  # Poisson - Plot ----------------------------------------------------------------             
  
  output$plot_pois <- renderPlot({
    
    if(is.finite(input$x_pois_lower) & is.finite(input$x_pois_upper)){
      lower <- input$x_pois_lower
      upper <- input$x_pois_upper
      if(upper - lower < 5){
        x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
      } else {
        x <- seq(from = lower, to = upper, by = 1)
      }
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_pois_lower)){
        if(input$tail_pois){
          lower <- 0
          upper <- input$x_pois_lower
        }
        else{
          lower <- input$x_pois_lower
          upper <- qpois(.999, lambda = input$lambda_pois)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_pois_upper)){
        if(input$tail_pois){
          lower <- 0
          upper <- input$x_pois_upper
        }
        else{
          lower <- input$x_pois_upper
          upper <- qpois(.999, lambda = input$lambda_pois)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_pois_lower) & !is.finite(input$x_pois_lower)){
        lower <- 0
        upper <- qpois(.999, lambda = input$lambda_pois)
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col = NA
        border = NA
      }
    }
    
    h.plot_pois <- plot(dpois(x, lambda = input$lambda_pois) ~ x, t = "h", axes = FALSE,
                        ylab = "Probabilidade", xlim = c(min(x), max(x)), xlab = "X")
    axis(1, x); axis(2, h.plot_pois); box()
    
    xcoord <- c(lower:upper)
    
    y <- dpois(xcoord, lambda = input$lambda_pois)
    ycoord <- y
    
    lines(x = xcoord,
          y = ycoord,
          col = "red", lwd = 2, t = "h")
    
    if(is.finite(input$x_pois_lower) & is.finite(input$x_pois_upper)){
      lower <- input$x_pois_lower
      upper <- input$x_pois_upper
      lab <- sum(dpois(c(lower:upper), lambda = input$lambda_pois))
    } else {
      if(is.finite(input$x_pois_lower)){
        lower <- input$x_pois_lower
        lab <- 1 - ppois(lower, lambda = input$lambda_pois, lower = input$tail_pois == FALSE)
      }
      else if(is.finite(input$x_pois_upper)){
        upper <- input$x_pois_upper
        lab <- ppois(upper, lambda = input$lambda_pois, lower = input$tail_pois)
      } 
      else if(!is.finite(input$x_pois_lower) & !is.finite(input$x_pois_lower)){
        lab <- 1
      }
    }
    
    legend("topright", legend = round(lab, 4), bty = "n", cex = 2.5)
    
    tit <- ifelse(test = is.finite(input$x_pois_lower) & is.finite(input$x_pois_upper) &
                    !input$x_pois_lower == input$x_pois_upper, 
                  yes = paste0("P[", input$x_pois_lower, " < X < ", input$x_pois_upper, "]"),
                  no = ifelse(test = is.finite(input$x_pois_lower) & is.finite(input$x_pois_upper) & 
                                input$x_pois_lower == input$x_pois_upper, 
                              yes = paste0("P[X = ", input$x_pois_lower, "]"), 
                              no = ifelse(test = is.finite(input$x_pois_lower),
                                          yes = ifelse(test = input$tail_pois,
                                                       yes = paste0("P[X < ", input$x_pois_lower, "]"),
                                                       no = paste0("1 - P[X < ", input$x_pois_lower, "]")),
                                          no = ifelse(test = is.finite(input$x_pois_lower), 
                                                      yes = ifelse(test = input$tail_pois,
                                                                   yes = paste0("P[X < ", input$x_pois_lower, "]"),
                                                                   no = paste0("1 - P[X <", input$x_pois_lower, "]")),
                                                      no = ""))))
    
    title(tit)
    
  })
  
  # Poisson - Output ----------------------------------------------------------------             
  
  output$pois <- renderPrint({
    
    if(is.finite(input$x_pois_lower) & is.finite(input$x_pois_upper)){
      lower <- input$x_pois_lower
      upper <- input$x_pois_upper
      lab <- sum(dpois(c(lower:upper), lambda = input$lambda_pois))
    } else {
      if(is.finite(input$x_pois_lower)){
        lower <- input$x_pois_lower
        lab <- 1 - ppois(lower, lambda = input$lambda_pois, lower = input$tail_pois == FALSE)
      }
      else if(is.finite(input$x_pois_upper)){
        upper <- input$x_pois_upper
        lab <- ppois(upper, lambda = input$lambda_pois, lower = input$tail_pois)
      } 
      else if(!is.finite(input$x_pois_lower) & !is.finite(input$x_pois_lower)){
        lab <- 1
      }
    }
    
    lab
    
  })
  
  
  
  # Q-Pois - Plot ----------------------------------------------------------
  
  output$plot_qpois <- renderPlot({
    
    med_seq <- input$lambda_qpois
    
    seqx <- seq(from = round((med_seq - 7)*(med_seq >= 7), 0), to = round(med_seq + 7, 0), by = 1)
    
    plot_bars <- plot(dpois(seqx, input$lambda_qpois) ~ seqx, t = "h", lwd = 2, ylab = "Probabilidades", xlab = "X")
    
    q_plot <- qpois(input$p_pois, input$lambda_qpois, input$tail_qpois)
    
    if(input$tail_qpois){
      seqx2 <- seq(from = min(seqx), to = q_plot, by = 1)
    } else {
      seqx2 <- seq(from = q_plot + 1, to = max(seqx), by = 1)
    }
    
    
    lines(dpois(seqx2, input$lambda_qpois) ~ seqx2, t = "h", col = "red", lwd = 2)
    
  })
  
  # Q-Pois - Output --------------------------------------------------------
  
  output$q_qpois <-renderPrint({
    qpois(input$p_qpois, input$tail_qpois)
  })
  
  # Geom 1 - Plot ----------------------------------------------------------------             
  
  output$plot_geom1 <- renderPlot({
    
    if(is.finite(input$x_geom1_lower) & is.finite(input$x_geom1_upper)){
      lower <- input$x_geom1_lower
      upper <- input$x_geom1_upper
      if(upper - lower < 5){
        x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
      } else {
        x <- seq(from = lower, to = upper, by = 1)
      }
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_geom1_lower)){
        if(input$tail_geom1){
          lower <- 0
          upper <- input$x_geom1_lower
        }
        else{
          lower <- input$x_geom1_lower
          upper <- qgeom(.999, prob = input$p_geom1)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_geom1_upper)){
        if(input$tail_geom1){
          lower <- 0
          upper <- input$x_geom1_upper
        }
        else{
          lower <- input$x_geom1_upper
          upper <- qgeom(.999, prob = input$p_geom1)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_geom1_lower) & !is.finite(input$x_geom1_lower)){
        lower <- 0
        upper <- qgeom(.999, prob = input$p_geom1)
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col = NA
        border = NA
      }
    }
    
    h.plot_geom1 <- plot(dgeom(x, prob = input$p_geom1) ~ x, t = "h", axes = FALSE,
                         ylab = "Probabilidade", xlim = c(min(x), max(x)), xlab = "X")
    axis(1, x); axis(2, h.plot_geom1); box()
    
    xcoord <- c(lower:upper)
    
    y <- dgeom(xcoord, prob = input$p_geom1)
    ycoord <- y
    
    lines(x = xcoord,
          y = ycoord,
          col = "red", lwd = 2, t = "h")
    
    if(is.finite(input$x_geom1_lower) & is.finite(input$x_geom1_upper)){
      lower <- input$x_geom1_lower
      upper <- input$x_geom1_upper
      lab <- sum(dgeom(c(lower:upper), input$p_geom1))
    } else{
      if(is.finite(input$x_geom1_lower)){
        lower <- input$x_geom1_lower
        lab <- 1 - pgeom(lower, prob = input$p_geom1, lower = input$tail_geom1 == FALSE)
      }
      else if(is.finite(input$x_geom1_upper)){
        upper <- input$x_geom1_upper
        lab <- pgeom(upper, prob = input$p_geom1, lower = input$tail_geom1)
      }
      else if(!is.finite(input$x_geom1_lower) & !is.finite(input$x_geom1_lower)){
        lab <- 1
      }
    }
    
    legend("topright", legend = round(lab, 4), bty = "n", cex = 2.5)
    
    tit <- ifelse(test = is.finite(input$x_geom1_lower) & is.finite(input$x_geom1_upper) &
                    !input$x_geom1_lower == input$x_geom1_upper, 
                  yes = paste0("P[", input$x_geom1_lower, " < X < ", input$x_geom1_upper, "]"),
                  no = ifelse(test = is.finite(input$x_geom1_lower) & is.finite(input$x_geom1_upper) & 
                                input$x_geom1_lower == input$x_geom1_upper, 
                              yes = paste0("P[X = ", input$x_geom1_lower, "]"), 
                              no = ifelse(test = is.finite(input$x_geom1_lower),
                                          yes = ifelse(test = input$tail_geom1,
                                                       yes = paste0("P[X < ", input$x_geom1_lower, "]"),
                                                       no = paste0("1 - P[X < ", input$x_geom1_lower, "]")),
                                          no = ifelse(test = is.finite(input$x_geom1_lower), 
                                                      yes = ifelse(test = input$tail_geom1,
                                                                   yes = paste0("P[X < ", input$x_geom1_lower, "]"),
                                                                   no = paste0("1 - P[X <", input$x_geom1_lower, "]")),
                                                      no = ""))))
    
    title(tit)
    
  })
  
  # Geom 1 - Output ----------------------------------------------------------------             
  
  output$geom1 <- renderPrint({
    
    if(is.finite(input$x_geom1_lower) & is.finite(input$x_geom1_upper)){
      lower <- input$x_geom1_lower
      upper <- input$x_geom1_upper
      lab <- sum(dgeom(c(lower:upper), input$p_geom1))
    } else{
      if(is.finite(input$x_geom1_lower)){
        lower <- input$x_geom1_lower
        lab <- 1 - pgeom(lower, prob = input$p_geom1, lower = input$tail_geom1 == FALSE)
      }
      else if(is.finite(input$x_geom1_upper)){
        upper <- input$x_geom1_upper
        lab <- pgeom(upper, prob = input$p_geom1, lower = input$tail_geom1)
      }
      else if(!is.finite(input$x_geom1_lower) & !is.finite(input$x_geom1_lower)){
        lab <- 1
      }
    }
    
    lab
    
  })
  
  
  
  # Q-Geom 1 - Plot ----------------------------------------------------------
  
  output$plot_qgeom1 <- renderPlot({
    
    med_seq <- (1 - input$p_geom1q)/input$p_geom1q
    
    seqx <- seq(from = round((med_seq - 7)*(med_seq >= 7), 0), to = round(med_seq + 7, 0), by = 1)
    
    plot_bars <- plot(dgeom(seqx, input$p_geom1q) ~ seqx, t = "h", lwd = 2, ylab = "Probabilidades", xlab = "X")
    
    q_plot <- qgeom(input$p_qgeom1, input$p_geom1q, input$tail_qgeom1)
    
    if(input$tail_qgeom1){
      seqx2 <- seq(from = min(seqx), to = q_plot, by = 1)
    } else {
      seqx2 <- seq(from = q_plot + 1, to = max(seqx), by = 1)
    }
    
    
    lines(dgeom(seqx2, input$p_geom1q) ~ seqx2, t = "h", col = "red", lwd = 2)
    
  })
  
  # Q-Geom 1 - Output --------------------------------------------------------
  
  output$qgeom1 <-renderPrint({
    qgeom(input$p_qgeom1, input$p_geom1q, input$tail_qgeom1)
  })
  
  # Geom 2 - Plot ----------------------------------------------------------------             
  
  output$plot_geom2 <- renderPlot({
    
    if(is.finite(input$x_geom2_lower) & is.finite(input$x_geom2_upper)){
      lower <- input$x_geom2_lower
      upper <- input$x_geom2_upper
      if(upper - lower < 5){
        x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
      } else {
        x <- seq(from = lower, to = upper, by = 1)
      }
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_geom2_lower)){
        if(input$tail_geom2){
          lower <- 0
          upper <- input$x_geom2_lower
        } else {
          lower <- input$x_geom2_lower
          upper <- qgeom(.999, prob = input$p_geom2)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      } else if (is.finite(input$x_geom2_upper)){
        if(input$tail_geom2){
          lower <- 0
          upper <- input$x_geom2_upper
        } else {
          lower <- input$x_geom2_upper
          upper <- qgeom(.999, prob = input$p_geom2)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      } else if (!is.finite(input$x_geom2_lower) & !is.finite(input$x_geom2_lower)){
        lower <- 0
        upper <- qgeom(.999, prob = input$p_geom2)
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col = NA
        border = NA
      }
    }
    
    h.plot_geom2 <- plot(dgeom(x, prob = input$p_geom2)/(1-input$p_geom2) ~ x, t = "h", axes = FALSE,
                         ylab = "Probabilidade", xlim = c(min(x), max(x)), xlab = "X")
    axis(1, x); axis(2, h.plot_geom2); box()
    
    xcoord <- c(lower:upper)
    
    y <- dgeom(xcoord, prob = input$p_geom2)/(1-input$p_geom2)
    ycoord <- y
    
    lines(x = xcoord,
          y = ycoord,
          col = "red", lwd = 2, t = "h")
    
    if(is.finite(input$x_geom2_lower) & is.finite(input$x_geom2_upper)){
      lower <- input$x_geom2_lower
      upper <- input$x_geom2_upper
      lab <- sum(dgeom(c(lower:upper), input$p_geom2)/(1-input$p_geom2))
    } else{
      if(is.finite(input$x_geom2_lower)){
        lower <- input$x_geom2_lower
        lab <- 1 - pgeom(lower, prob = input$p_geom2, lower = input$tail_geom2 == FALSE)/(1-input$p_geom2)
      }
      else if(is.finite(input$x_geom2_upper)){
        upper <- input$x_geom2_upper
        lab <- pgeom(upper, prob = input$p_geom2, lower = input$tail_geom2)/(1-input$p_geom2)
      }
      else if(!is.finite(input$x_geom2_lower) & !is.finite(input$x_geom2_lower)){
        lab <- 1
      }
    }
    
    legend("topright", legend = round(lab, 4), bty = "n", cex = 2.5)
    
    tit <- ifelse(test = is.finite(input$x_geom2_lower) & is.finite(input$x_geom2_upper) &
                    !input$x_geom2_lower == input$x_geom2_upper, 
                  yes = paste0("P[", input$x_geom2_lower, " < X < ", input$x_geom2_upper, "]"),
                  no = ifelse(test = is.finite(input$x_geom2_lower) & is.finite(input$x_geom2_upper) & 
                                input$x_geom2_lower == input$x_geom2_upper, 
                              yes = paste0("P[X = ", input$x_geom2_lower, "]"), 
                              no = ifelse(test = is.finite(input$x_geom2_lower),
                                          yes = ifelse(test = input$tail_geom2,
                                                       yes = paste0("P[X < ", input$x_geom2_lower, "]"),
                                                       no = paste0("1 - P[X < ", input$x_geom2_lower, "]")),
                                          no = ifelse(test = is.finite(input$x_geom2_lower), 
                                                      yes = ifelse(test = input$tail_geom2,
                                                                   yes = paste0("P[X < ", input$x_geom2_lower, "]"),
                                                                   no = paste0("1 - P[X <", input$x_geom2_lower, "]")),
                                                      no = ""))))
    
    title(tit)
    
  })
  
  # Geom 2 - Output ----------------------------------------------------------------             
  
  output$geom2 <- renderPrint({
    
    if(is.finite(input$x_geom2_lower) & is.finite(input$x_geom2_upper)){
      lower <- input$x_geom2_lower
      upper <- input$x_geom2_upper
      lab <- sum(dgeom(c(lower:upper), input$p_geom2)/(1-input$p_geom2))
    } else{
      if(is.finite(input$x_geom2_lower)){
        lower <- input$x_geom2_lower
        lab <- 1 - pgeom(lower, prob = input$p_geom2, lower = input$tail_geom2 == FALSE)/(1-input$p_geom2)
      }
      else if(is.finite(input$x_geom2_upper)){
        upper <- input$x_geom2_upper
        lab <- pgeom(upper, prob = input$p_geom2, lower = input$tail_geom2)/(1-input$p_geom2)
      }
      else if(!is.finite(input$x_geom2_lower) & !is.finite(input$x_geom2_lower)){
        lab <- 1
      }
    }
    
    lab
    
  })
  
  
  
  # Q-Geom 2 - Plot ----------------------------------------------------------
  
  output$plot_qgeom2 <- renderPlot({
    
    med_seq <- 1/input$p_geom2q
    
    seqx <- seq(from = round((med_seq - 7)*(med_seq >= 7), 0), to = round(med_seq + 7, 0), by = 1)
    
    plot_bars <- plot(dgeom(seqx, input$p_geom2q)/(1-input$p_geom2q) ~ seqx, t = "h", lwd = 2, ylab = "Probabilidades", xlab = "X")
    
    q_plot <- qgeom(input$p_qgeom2, input$p_geom2q, input$tail_qgeom2) - 1
    
    if(input$tail_qgeom2){
      seqx2 <- seq(from = min(seqx), to = q_plot, by = 1)
    } else {
      seqx2 <- seq(from = q_plot + 1, to = max(seqx), by = 1)
    }
    
    
    lines(dgeom(seqx2, input$p_geom2q)/(1 - input$p_geom2q) ~ seqx2, t = "h", col = "red", lwd = 2)
    
  })
  
  # Q-Geom 2 - Output --------------------------------------------------------
  
  output$qgeom2 <-renderPrint({
    qgeom(input$p_qgeom2, input$p_geom2q, input$tail_qgeom2) - 1
  })
  
  # HyperGeom - Plot ----------------------------------------------------------------             
  
  output$plot_hyper <- renderPlot({
    
    if(is.finite(input$x_hyper_lower) & is.finite(input$x_hyper_upper)){
      lower <- input$x_hyper_lower
      upper <- input$x_hyper_upper
      if(upper - lower < 5){
        x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
      } else {
        x <- seq(from = lower, to = upper, by = 1)
      }
      col <- "red"
      border <- "black"
    } else {
      if(is.finite(input$x_hyper_lower)){
        if(input$tail_hyper){
          lower <- 0
          upper <- input$x_hyper_lower
        }
        else{
          lower <- input$x_hyper_lower
          upper <- qhyper(.999, input$m_hyper, input$r_hyper, input$k_hyper)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_hyper_upper)){
        if(input$tail_hyper){
          lower <- 0
          upper <- input$x_hyper_upper
        }
        else{
          lower <- input$x_hyper_upper
          upper <- qhyper(.999, input$m_hyper, input$r_hyper, input$k_hyper)
        }
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_hyper_lower) & !is.finite(input$x_hyper_lower)){
        lower <- 0
        upper <- qhyper(.999, input$m_hyper, input$r_hyper, input$k_hyper)
        
        if(upper - lower < 5){
          x <- seq(from = ifelse(lower < 5, 0, lower - 5), to = upper + 5, by = 1)
        } else {
          x <- seq(from = lower, to = upper, by = 1)
        }
        
        col = NA
        border = NA
      }
    }
    
    h.plot_hyper <- plot(dhyper(x, input$m_hyper, input$r_hyper, input$k_hyper) ~ x, t = "h", axes = FALSE,
                         ylab = "Probabilidade", xlim = c(min(x), max(x)), xlab = "X")
    axis(1, x); axis(2, h.plot_hyper); box()
    
    xcoord <- c(lower:upper)
    
    y <- dhyper(xcoord, input$m_hyper, input$r_hyper, input$k_hyper)
    ycoord <- y
    
    lines(x = xcoord,
          y = ycoord,
          col = "red", lwd = 2, t = "h")
    
    if(is.finite(input$x_hyper_lower) & is.finite(input$x_hyper_upper)){
      lower <- input$x_hyper_lower
      upper <- input$x_hyper_upper
      lab <- sum(dhyper(c(lower:upper), input$m_hyper, input$r_hyper, input$k_hyper))
    } else{
      if(is.finite(input$x_hyper_lower)){
        lower <- input$x_hyper_lower
        lab <- 1 - phyper(lower, input$m_hyper, input$r_hyper, input$k_hyper, lower = input$tail_hyper == FALSE)
      }
      else if(is.finite(input$x_hyper_upper)){
        upper <- input$x_hyper_upper
        lab <- phyper(upper, input$m_hyper, input$r_hyper, input$k_hyper, lower = input$tail_hyper)
      }
      else if(!is.finite(input$x_hyper_lower) & !is.finite(input$x_hyper_lower)){
        lab <- 1
      }
    }
    
    legend("topright", legend = lab, bty = "n", cex = 2.5)
    
    tit <- ifelse(test = is.finite(input$x_hyper_lower) & is.finite(input$x_hyper_upper) &
                    !input$x_hyper_lower == input$x_hyper_upper, 
                  yes = paste0("P[", input$x_hyper_lower, " < X < ", input$x_hyper_upper, "]"),
                  no = ifelse(test = is.finite(input$x_hyper_lower) & is.finite(input$x_hyper_upper) & 
                                input$x_hyper_lower == input$x_hyper_upper, 
                              yes = paste0("P[X = ", input$x_hyper_lower, "]"), 
                              no = ifelse(test = is.finite(input$x_hyper_lower),
                                          yes = ifelse(test = input$tail_hyper,
                                                       yes = paste0("P[X < ", input$x_hyper_lower, "]"),
                                                       no = paste0("1 - P[X < ", input$x_hyper_lower, "]")),
                                          no = ifelse(test = is.finite(input$x_hyper_lower), 
                                                      yes = ifelse(test = input$tail_hyper,
                                                                   yes = paste0("P[X < ", input$x_hyper_lower, "]"),
                                                                   no = paste0("1 - P[X <", input$x_hyper_lower, "]")),
                                                      no = ""))))
    
    title(tit)
    
  })
  
  # HyperGeom - Output ----------------------------------------------------------------             
  
  output$hyper <- renderPrint({
    
    if(is.finite(input$x_hyper_lower) & is.finite(input$x_hyper_upper)){
      lower <- input$x_hyper_lower
      upper <- input$x_hyper_upper
      lab <- sum(dhyper(c(lower:upper), input$m_hyper, input$r_hyper, input$k_hyper))
    } else{
      if(is.finite(input$x_hyper_lower)){
        lower <- input$x_hyper_lower
        lab <- 1 - phyper(lower, input$m_hyper, input$r_hyper, input$k_hyper, lower = input$tail_hyper == FALSE)
      }
      else if(is.finite(input$x_hyper_upper)){
        upper <- input$x_hyper_upper
        lab <- phyper(upper, input$m_hyper, input$r_hyper, input$k_hyper, lower = input$tail_hyper)
      }
      else if(!is.finite(input$x_hyper_lower) & !is.finite(input$x_hyper_lower)){
        lab <- 1
      }
    }
    
    lab
    
  })
  
  
  # Q-Hyper - Plot ----------------------------------------------------------
  
  output$plot_qhyper <- renderPlot({
    
    med_seq <- input$k_qhyper*input$m_qhyper/(input$r_qhyper + input$m_qhyper)
    
    seqx <- seq(from = round((med_seq - 7)*(med_seq >= 7), 0), to = input$k_qhyper, by = 1)
    
    plot_bars <- plot(dhyper(seqx, input$m_qhyper, input$r_qhyper, input$k_qhyper) ~ seqx, t = "h", lwd = 2, ylab = "Probabilidades",
                      xlab = "X")
    
    q_plot <- qhyper(input$p_hyper, input$m_qhyper, input$r_qhyper, input$k_qhyper, input$tail_qhyper)
    
    if(input$tail_qhyper){
      seqx2 <- seq(from = min(seqx), to = q_plot, by = 1)
    } else {
      seqx2 <- seq(from = q_plot + 1, to = max(seqx), by = 1)
    }
    
    
    lines(dhyper(seqx2, input$m_qhyper, input$r_qhyper, input$k_qhyper) ~ seqx2, t = "h", col = "red", lwd = 2)
    
  })
  
  # Q-Hyper - Output --------------------------------------------------------
  
  output$qhyper <-renderPrint({
    qhyper(input$p_hyper, input$m_qhyper, input$r_qhyper, input$k_qhyper, input$tail_qhyper)
  })
  
}

shinyApp(ui = ui, server = server)