# ============================================================
# Application Shiny : Percentage Error
# Basée sur le script de Cédric Hubas
# ============================================================

# Packages ----------------------------------------------------

required_packages <- c(
  "shiny",
  "ggplot2",
  "scales"
)

# Installation automatique des packages manquants
missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(ggplot2)
library(scales)


# ============================================================
# Fonction de calcul
# ============================================================

calculate_error <- function(N, n, p, conf) {
  
  # Transformation du niveau de confiance en quantile normal
  z <- qnorm(1 - (1 - conf) / 2)
  
  # Rapport population / échantillon
  threshold <- N / n
  
  # Erreur standard sans correction de population finie
  se <- sqrt((p * (1 - p)) / n)
  
  # Correction de population finie si la fraction
  # d'échantillonnage est supérieure à 5 %
  if (threshold <= 20) {
    
    fpc <- sqrt((N - n) / (N - 1))
    se <- se * fpc
    
    correction <- TRUE
    
  } else {
    
    correction <- FALSE
    
  }
  
  # Marge d'erreur
  error <- z * se
  
  # Intervalle de confiance
  lower <- max(0, p - error)
  upper <- min(1, p + error)
  
  list(
    z = z,
    error = error,
    lower = lower,
    upper = upper,
    correction = correction,
    sampling_fraction = n / N,
    threshold = threshold
  )
}


# ============================================================
# Interface utilisateur
# ============================================================

ui <- fluidPage(
  
  # CSS --------------------------------------------------------
  
  tags$head(
    
    tags$title("Percentage Error Calculator"),
    
    tags$style(HTML("
      
      body {
        background-color: #f5f5f5;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        max-width: 1400px;
        margin: auto;
      }

      .app-title {
        background: white;
        padding: 25px;
        margin-bottom: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .app-title h1 {
        margin-top: 0;
        margin-bottom: 8px;
      }

      .app-title p {
        color: #666;
        margin-bottom: 0;
      }

      .parameter-panel {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .result-box {
        background: white;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .result-value {
        font-size: 28px;
        font-weight: bold;
      }

      .info-box {
        padding: 15px;
        border-radius: 6px;
        margin-top: 15px;
      }

      .plot-container {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .footer {
        margin-top: 30px;
        margin-bottom: 30px;
        text-align: center;
        color: #777;
        font-size: 13px;
      }

    "))
  ),
  
  
  # ==========================================================
  # TITRE
  # ==========================================================
  
  div(
    class = "app-title",
    
    h1("Percentage Error Calculator"),
    
    p(
      "Calcul de la marge d'erreur d'une proportion ",
      "avec prise en compte de la correction de population finie.",
      "Modifier les paramètres sur le cadran de gauche et calculez la marge d'erreur en cliquant sur le bouton 'Calculer'."
    )
  ),
  
  
  # ==========================================================
  # CORPS DE L'APPLICATION
  # ==========================================================
  
  fluidRow(
    
    # ----------------------------------------------------------
    # COLONNE GAUCHE : PARAMÈTRES
    # ----------------------------------------------------------
    
    column(
      width = 3,
      
      div(
        class = "parameter-panel",
        
        h3("Paramètres"),
        
        numericInput(
          inputId = "N",
          label = "Population (N)",
          value = 600,
          min = 2,
          step = 1
        ),
        
        numericInput(
          inputId = "n",
          label = "Échantillon (n)",
          value = 300,
          min = 1,
          step = 1
        ),
        
        numericInput(
          inputId = "p",
          label = "Pourcentage observé (%)",
          value = 13.6,
          min = 0,
          max = 100,
          step = 0.1
        ),
        
        selectInput(
          inputId = "conf",
          label = "Niveau de confiance",
          choices = c(
            "90 %" = 0.90,
            "95 %" = 0.95,
            "99 %" = 0.99,
            "99,9 %" = 0.999
          ),
          selected = 0.99
        ),
        
        hr(),
        
        actionButton(
          inputId = "calculate",
          label = "Calculer",
          class = "btn-primary",
          width = "100%"
        ),
        
        br(),
        br(),
        
        helpText(
          strong("Règle : "),
          "la correction de population finie est appliquée ",
          "lorsque l'échantillon représente plus de 5 % de la population ",
          "(N/n ≤ 20)."
        )
      )
    ),
    
    
    # ----------------------------------------------------------
    # COLONNE CENTRALE : RESULTATS
    # ----------------------------------------------------------
    
    column(
      width = 3,
      
      div(
        class = "result-box",
        
        h4("Marge d'erreur"),
        
        uiOutput("error_value")
      ),
      
      div(
        class = "result-box",
        
        h4("Intervalle de confiance"),
        
        uiOutput("confidence_interval")
      ),
      
      div(
        class = "result-box",
        
        h4("Correction de population"),
        
        uiOutput("correction_status")
      ),
      
      div(
        class = "result-box",
        
        h4("Fraction d'échantillonnage"),
        
        uiOutput("sampling_fraction")
      )
    ),
    
    
    # ----------------------------------------------------------
    # COLONNE DROITE : GRAPHIQUE
    # ----------------------------------------------------------
    
    column(
      width = 6,
      
      div(
        class = "plot-container",
        
        h3("Marge d'erreur selon le pourcentage"),
        
        plotOutput(
          outputId = "error_plot",
          height = "500px"
        ),
        
        downloadButton(
          outputId = "download_plot",
          label = "Télécharger le graphique"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # INFORMATIONS
  # ==========================================================
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "info-box",
        style = "background-color: #eaf2f8;",
        
        h4("Méthode de calcul"),
        
        p(
          "La marge d'erreur est calculée comme le produit de ",
          "l'erreur standard par le quantile de la loi normale ",
          "correspondant au niveau de confiance choisi."
        ),
        
        p(
          strong("Sans correction : "),
          "SE = √[p(1 − p) / n]"
        ),
        
        p(
          strong("Avec correction de population finie : "),
          "SE = √[p(1 − p) / n] × √[(N − n) / (N − 1)]"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # FOOTER
  # ==========================================================
  
  div(
    class = "footer",
    
    "Percentage Error Calculator — Application Shiny"
  )
)


# ============================================================
# SERVEUR
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Calcul réactif
  # ----------------------------------------------------------
  
  results <- eventReactive(input$calculate, {
    
    # Vérification des valeurs
    validate(
      need(input$N >= 2, "La population doit être supérieure ou égale à 2."),
      need(input$n >= 1, "La taille de l'échantillon doit être supérieure à 0."),
      need(input$n <= input$N,
           "La taille de l'échantillon ne peut pas être supérieure à la population."),
      need(input$p >= 0 && input$p <= 100,
           "Le pourcentage doit être compris entre 0 et 100.")
    )
    
    # Conversion du pourcentage en proportion
    p <- input$p / 100
    
    calculate_error(
      N = input$N,
      n = input$n,
      p = p,
      conf = as.numeric(input$conf)
    )
  })
  
  
  # ----------------------------------------------------------
  # Marge d'erreur
  # ----------------------------------------------------------
  
  output$error_value <- renderUI({
    
    res <- results()
    
    div(
      class = "result-value",
      
      paste0(
        "± ",
        format(round(res$error * 100, 2),
               decimal.mark = ",",
               nsmall = 2),
        " %"
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Intervalle de confiance
  # ----------------------------------------------------------
  
  output$confidence_interval <- renderUI({
    
    res <- results()
    
    div(
      class = "result-value",
      
      paste0(
        format(round(res$lower * 100, 2),
               decimal.mark = ",",
               nsmall = 2),
        " % – ",
        
        format(round(res$upper * 100, 2),
               decimal.mark = ",",
               nsmall = 2),
        " %"
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Correction de population finie
  # ----------------------------------------------------------
  
  output$correction_status <- renderUI({
    
    res <- results()
    
    if (res$correction) {
      
      div(
        style = "color: #d35400; font-weight: bold;",
        
        "✓ Correction appliquée"
      )
      
    } else {
      
      div(
        style = "color: #2874a6; font-weight: bold;",
        
        "✓ Correction non nécessaire"
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # Fraction d'échantillonnage
  # ----------------------------------------------------------
  
  output$sampling_fraction <- renderUI({
    
    res <- results()
    
    div(
      class = "result-value",
      
      paste0(
        format(
          round(res$sampling_fraction * 100, 2),
          decimal.mark = ",",
          nsmall = 2
        ),
        " %"
      )
    )
  })
  
  
  # ==========================================================
  # GRAPHIQUE
  # ==========================================================
  
  output$error_plot <- renderPlot({
    
    res <- results()
    
    # Série de pourcentages de 0 à 100 %
    per <- seq(0, 1, by = 0.001)
    
    # Quantile normal
    z <- res$z
    
    # Erreur standard
    err <- z * sqrt((per * (1 - per)) / input$n)
    
    # Correction de population finie
    if (res$correction) {
      
      fpc <- sqrt(
        (input$N - input$n) /
          (input$N - 1)
      )
      
      err <- err * fpc
    }
    
    # Données pour ggplot
    plot_data <- data.frame(
      percentage = per * 100,
      error = err * 100
    )
    
    # Erreur correspondant au p choisi
    selected_error <- res$error * 100
    
    # Graphique
    ggplot(
      plot_data,
      aes(
        x = percentage,
        y = error
      )
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point(
        data = data.frame(
          percentage = input$p,
          error = selected_error
        ),
        aes(
          x = percentage,
          y = error
        ),
        size = 4
      ) +
      
      geom_vline(
        xintercept = input$p,
        linetype = "dashed"
      ) +
      
      geom_hline(
        yintercept = selected_error,
        linetype = "dashed"
      ) +
      
      annotate(
        "label",
        x = input$p,
        y = selected_error,
        label = paste0(
          format(
            round(input$p, 2),
            decimal.mark = ",",
            nsmall = 2
          ),
          " ± ",
          format(
            round(selected_error, 2),
            decimal.mark = ",",
            nsmall = 2
          ),
          " %"
        ),
        hjust = -0.05,
        vjust = -0.5
      ) +
      
      labs(
        title = paste0(
          "N = ",
          input$N,
          " | n = ",
          input$n
        ),
        
        subtitle = ifelse(
          res$correction,
          "Correction de population finie appliquée",
          "Correction de population finie non nécessaire"
        ),
        
        x = "Pourcentage testé p (%)",
        
        y = "Marge d'erreur (%)"
      ) +
      
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 10)
      ) +
      
      theme_bw(
        base_size = 14
      ) +
      
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40")
      )
  })
  
  
  # ==========================================================
  # TELECHARGEMENT DU GRAPHIQUE
  # ==========================================================
  
  output$download_plot <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "percentage_error_N",
        input$N,
        "_n",
        input$n,
        ".png"
      )
    },
    
    content = function(file) {
      
      res <- results()
      
      per <- seq(0, 1, by = 0.001)
      
      z <- res$z
      
      err <- z * sqrt(
        (per * (1 - per)) / input$n
      )
      
      if (res$correction) {
        
        fpc <- sqrt(
          (input$N - input$n) /
            (input$N - 1)
        )
        
        err <- err * fpc
      }
      
      plot_data <- data.frame(
        percentage = per * 100,
        error = err * 100
      )
      
      selected_error <- res$error * 100
      
      p <- ggplot(
        plot_data,
        aes(
          x = percentage,
          y = error
        )
      ) +
        
        geom_line(
          linewidth = 1
        ) +
        
        geom_point(
          data = data.frame(
            percentage = input$p,
            error = selected_error
          ),
          aes(
            x = percentage,
            y = error
          ),
          size = 4
        ) +
        
        geom_vline(
          xintercept = input$p,
          linetype = "dashed"
        ) +
        
        geom_hline(
          yintercept = selected_error,
          linetype = "dashed"
        ) +
        
        annotate(
          "label",
          x = input$p,
          y = selected_error,
          label = paste0(
            round(input$p, 2),
            " ± ",
            round(selected_error, 2),
            " %"
          )
        ) +
        
        labs(
          title = paste0(
            "Percentage Error — N = ",
            input$N,
            ", n = ",
            input$n
          ),
          
          subtitle = ifelse(
            res$correction,
            "Correction de population finie appliquée",
            "Correction de population finie non nécessaire"
          ),
          
          x = "Pourcentage testé p (%)",
          
          y = "Marge d'erreur (%)"
        ) +
        
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, 10)
        ) +
        
        theme_bw(
          base_size = 14
        )
      
      ggsave(
        filename = file,
        plot = p,
        width = 10,
        height = 6,
        dpi = 300
      )
    }
  )
}


# ============================================================
# LANCEMENT DE L'APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
