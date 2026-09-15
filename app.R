required_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "DT",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Installer først: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  )
}

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

source(
  file.path("R", "generate_demo_data.R"),
  encoding = "UTF-8"
)

data_path <- file.path("data", "simulated_lab_data.csv")

if (!file.exists(data_path)) {
  dir.create("data", recursive = TRUE, showWarnings = FALSE)
  demo_data <- generate_demo_data()
  write.csv(
    demo_data,
    data_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
} else {
  demo_data <- read.csv(
    data_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  demo_data <- add_quality_flags(demo_data)
}

demo_data$sample_date <- as.Date(demo_data$sample_date)
demo_data$received_date <- as.Date(demo_data$received_date)

theme_demo <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = "#17324d"
      ),
      plot.subtitle = element_text(colour = "#536579"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

metric_card <- function(
  title,
  value,
  note = NULL,
  colour = "#146c94"
) {
  div(
    class = "metric-card",
    style = paste0("border-top: 5px solid ", colour, ";"),
    div(class = "metric-title", title),
    div(class = "metric-value", value),
    if (!is.null(note)) {
      div(class = "metric-note", note)
    }
  )
}

issue_counts <- function(dat) {
  data.frame(
    avvik = c(
      "Duplikat",
      "Manglende verdi",
      "Ugyldig enhet",
      "PSA = 0",
      "PSA >1000",
      "Kode LI",
      "Avvik i FTI",
      "Forsinket mottak"
    ),
    antall = c(
      sum(dat$flag_duplicate, na.rm = TRUE),
      sum(dat$flag_missing, na.rm = TRUE),
      sum(dat$flag_unit, na.rm = TRUE),
      sum(dat$flag_zero, na.rm = TRUE),
      sum(dat$flag_high, na.rm = TRUE),
      sum(dat$flag_li, na.rm = TRUE),
      sum(dat$flag_fti, na.rm = TRUE),
      sum(dat$flag_delay, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background: #f4f7fa;
          color: #17324d;
        }
        .container-fluid {
          max-width: 1500px;
        }
        .demo-banner {
          background: #fff3cd;
          border: 1px solid #ffe69c;
          color: #664d03;
          padding: 10px 16px;
          border-radius: 8px;
          margin: 12px 0 18px 0;
          font-weight: 700;
          letter-spacing: 0.02em;
        }
        .metric-card {
          background: white;
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(25, 50, 77, 0.08);
          padding: 14px 16px;
          margin-bottom: 15px;
          min-height: 118px;
        }
        .metric-title {
          color: #607487;
          font-size: 13px;
          text-transform: uppercase;
          letter-spacing: 0.04em;
        }
        .metric-value {
          color: #17324d;
          font-size: 30px;
          font-weight: 750;
          margin-top: 6px;
        }
        .metric-note {
          color: #607487;
          font-size: 12px;
          margin-top: 3px;
        }
        .panel-box {
          background: white;
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(25, 50, 77, 0.08);
          padding: 16px;
          margin-bottom: 16px;
        }
        .well {
          background: white;
          border: 0;
          box-shadow: 0 2px 8px rgba(25, 50, 77, 0.08);
          border-radius: 10px;
        }
        .nav-tabs > li > a {
          color: #31566f;
          font-weight: 600;
        }
        .nav-tabs > li.active > a {
          color: #0b6e75;
        }
        h2, h3, h4 {
          color: #17324d;
        }
        .small-help {
          color: #607487;
          font-size: 12px;
        }
        "
      )
    )
  ),

  titlePanel(
    "Datakvalitetsdashboard – fiktive laboratoriedata"
  ),

  div(
    class = "demo-banner",
    "FIKTIVE DATA – KUN FOR DEMONSTRASJON. ",
    "Ingen reelle pasienter, sykehus eller prosjektdata er brukt."
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtre"),
      selectizeInput(
        "hospital",
        "Sykehus",
        choices = c(
          "Alle sykehus",
          sort(unique(demo_data$hospital))
        ),
        selected = "Alle sykehus",
        multiple = FALSE
      ),
      dateRangeInput(
        "date_range",
        "Prøvedato",
        start = min(demo_data$sample_date),
        end = max(demo_data$sample_date),
        min = min(demo_data$sample_date),
        max = max(demo_data$sample_date),
        separator = " til "
      ),
      sliderInput(
        "age",
        "Alder",
        min = min(demo_data$age),
        max = max(demo_data$age),
        value = range(demo_data$age),
        step = 1
      ),
      checkboxInput(
        "only_deviations",
        "Vis bare rader med avvik",
        value = FALSE
      ),
      actionButton(
        "reset_filters",
        "Nullstill filtre",
        class = "btn-primary"
      ),
      hr(),
      div(
        class = "small-help",
        "Alle filtre påvirker nøkkeltall, figurer og tabeller."
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",

        tabPanel(
          "Oversikt",
          br(),
          uiOutput("kpi_cards"),
          fluidRow(
            column(
              7,
              div(
                class = "panel-box",
                plotOutput(
                  "volume_plot",
                  height = "330px"
                )
              )
            ),
            column(
              5,
              div(
                class = "panel-box",
                plotOutput(
                  "quality_plot",
                  height = "330px"
                )
              )
            )
          ),
          div(
            class = "panel-box",
            h4("Hva viser løsningen?"),
            p(
              "Dashboardet gir først en rask oversikt, og lar ",
              "deretter brukeren finne hvilke typer avvik som ",
              "finnes, hvor de oppstår og hvilke rader som bør ",
              "følges opp."
            )
          )
        ),

        tabPanel(
          "Datakvalitet",
          br(),
          fluidRow(
            column(
              6,
              div(
                class = "panel-box",
                plotOutput(
                  "issue_plot",
                  height = "390px"
                )
              )
            ),
            column(
              6,
              div(
                class = "panel-box",
                h4("Kvalitet per sykehus"),
                DTOutput("hospital_table")
              )
            )
          ),
          div(
            class = "panel-box",
            h4("Automatisk vurdering"),
            uiOutput("quality_message")
          )
        ),

        tabPanel(
          "PSA-fordeling",
          br(),
          fluidRow(
            column(
              8,
              div(
                class = "panel-box",
                checkboxInput(
                  "log_x",
                  "Logaritmisk x-akse",
                  value = TRUE
                ),
                plotOutput(
                  "histogram",
                  height = "380px"
                )
              )
            ),
            column(
              4,
              div(
                class = "panel-box",
                h4("Percentiler"),
                tableOutput("percentile_table")
              ),
              div(
                class = "panel-box",
                h4("Kliniske terskler"),
                tableOutput("threshold_table")
              )
            )
          ),
          div(
            class = "panel-box",
            plotOutput(
              "boxplot",
              height = "360px"
            )
          )
        ),

        tabPanel(
          "Hormoner og konsistens",
          br(),
          fluidRow(
            column(
              6,
              div(
                class = "panel-box",
                plotOutput(
                  "hormone_plot",
                  height = "390px"
                )
              )
            ),
            column(
              6,
              div(
                class = "panel-box",
                plotOutput(
                  "fti_plot",
                  height = "390px"
                )
              )
            )
          ),
          div(
            class = "panel-box",
            h4("Kontroll av FTI"),
            p(
              "Forventet verdi i demonstrasjonen er definert som ",
              "10 × testosteron / SHBG. Punkter langt fra ",
              "diagonalen markeres som mulige beregningsavvik."
            )
          )
        ),

        tabPanel(
          "Avviksliste",
          br(),
          fluidRow(
            column(
              8,
              h4("Rader som bør følges opp")
            ),
            column(
              4,
              div(
                style = "text-align:right; margin-bottom:10px;",
                downloadButton(
                  "download_deviations",
                  "Last ned avviksliste",
                  class = "btn-primary"
                )
              )
            )
          ),
          DTOutput("deviation_table")
        ),

        tabPanel(
          "Metode og bruk",
          br(),
          div(
            class = "panel-box",
            h3("Formål"),
            p(
              "Dette er en intervjudemonstrasjon av en mulig ",
              "arbeidsflyt for datakvalitet i et medisinsk ",
              "kvalitetsregister."
            ),
            h4("Kontroller som utføres"),
            tags$ul(
              tags$li(
                "Kompletthet for sentrale variabler"
              ),
              tags$li("Duplikate prøve-ID-er"),
              tags$li("Ugyldige enheter"),
              tags$li(
                "Nullverdier og verdier over måleområdet"
              ),
              tags$li(
                "Spesialkoder i originalsvar"
              ),
              tags$li(
                "Intern konsistens i beregnet FTI"
              ),
              tags$li(
                "Forsinkelse mellom prøvetaking og mottak"
              )
            ),
            h4("Mulig bruk i praksis"),
            p(
              "Avvik kan oppsummeres per sykehus, undersøkes på ",
              "radnivå og eksporteres til en oppfølgingsliste. ",
              "Det gjør det enklere å kontakte riktig enhet og ",
              "følge utviklingen i datakvalitet over tid."
            ),
            h4("Viktig"),
            p(
              "Alle navn, ID-er, datoer og måleverdier i denne ",
              "applikasjonen er tilfeldig generert."
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_filters, {
    updateSelectizeInput(
      session,
      "hospital",
      selected = "Alle sykehus"
    )
    updateDateRangeInput(
      session,
      "date_range",
      start = min(demo_data$sample_date),
      end = max(demo_data$sample_date)
    )
    updateSliderInput(
      session,
      "age",
      value = range(demo_data$age)
    )
    updateCheckboxInput(
      session,
      "only_deviations",
      value = FALSE
    )
  })

  filtered <- reactive({
    req(input$hospital, input$date_range, input$age)

    dat <- demo_data %>%
      filter(
        sample_date >= as.Date(input$date_range[1]),
        sample_date <= as.Date(input$date_range[2]),
        age >= input$age[1],
        age <= input$age[2]
      )

    if (!identical(input$hospital, "Alle sykehus")) {
      dat <- dat %>%
        filter(hospital == input$hospital)
    }

    if (isTRUE(input$only_deviations)) {
      dat <- dat %>% filter(avvik_any)
    }

    dat
  })

  output$kpi_cards <- renderUI({
    dat <- filtered()
    n_records <- nrow(dat)
    n_samples <- n_distinct(dat$sample_id)
    completeness <- if (n_records == 0) {
      NA_real_
    } else {
      mean(
        complete.cases(
          dat[c("psa", "testosterone", "shbg")]
        )
      )
    }
    deviation_rate <- if (n_records == 0) {
      NA_real_
    } else {
      mean(dat$avvik_any)
    }

    fluidRow(
      column(
        3,
        metric_card(
          "Registreringer",
          comma(n_records, big.mark = " "),
          "Etter valgte filtre",
          "#146c94"
        )
      ),
      column(
        3,
        metric_card(
          "Unike prøver",
          comma(n_samples, big.mark = " "),
          "Basert på prøve-ID",
          "#0b6e75"
        )
      ),
      column(
        3,
        metric_card(
          "Komplette",
          ifelse(
            is.na(completeness),
            "–",
            percent(
              completeness,
              accuracy = 0.1,
              decimal.mark = ","
            )
          ),
          "PSA, testosteron og SHBG",
          "#2d8a57"
        )
      ),
      column(
        3,
        metric_card(
          "Med avvik",
          ifelse(
            is.na(deviation_rate),
            "–",
            percent(
              deviation_rate,
              accuracy = 0.1,
              decimal.mark = ","
            )
          ),
          "Minst ett automatisk flagg",
          ifelse(
            is.na(deviation_rate) ||
              deviation_rate <= 0.08,
            "#d97706",
            "#b42318"
          )
        )
      )
    )
  })

  output$volume_plot <- renderPlot({
    dat <- filtered()
    validate(
      need(nrow(dat) > 0, "Ingen data for valgte filtre.")
    )

    weekly <- dat %>%
      mutate(
        week = as.Date(cut(sample_date, breaks = "week"))
      ) %>%
      count(week)

    ggplot(weekly, aes(week, n)) +
      geom_area(fill = "#55a7b2", alpha = 0.35) +
      geom_line(colour = "#0b6e75", linewidth = 1) +
      labs(
        title = "Prøvevolum over tid",
        subtitle = "Antall registreringer per uke",
        x = NULL,
        y = "Antall"
      ) +
      theme_demo()
  })

  output$quality_plot <- renderPlot({
    dat <- filtered()
    validate(
      need(nrow(dat) > 0, "Ingen data for valgte filtre.")
    )

    summary_data <- dat %>%
      group_by(hospital) %>%
      summarise(
        quality = 100 * mean(!avvik_any),
        .groups = "drop"
      )

    ggplot(
      summary_data,
      aes(reorder(hospital, quality), quality)
    ) +
      geom_col(fill = "#2d8a57", width = 0.7) +
      coord_flip() +
      geom_text(
        aes(
          label = paste0(
            format(
              round(quality, 1),
              decimal.mark = ","
            ),
            "%"
          )
        ),
        hjust = -0.1,
        size = 3.8
      ) +
      scale_y_continuous(
        limits = c(0, 105),
        breaks = seq(0, 100, 20)
      ) +
      labs(
        title = "Andel uten registrerte avvik",
        x = NULL,
        y = "Prosent"
      ) +
      theme_demo()
  })

  output$issue_plot <- renderPlot({
    dat <- filtered()
    validate(
      need(nrow(dat) > 0, "Ingen data for valgte filtre.")
    )

    counts <- issue_counts(dat) %>%
      arrange(antall)

    ggplot(
      counts,
      aes(reorder(avvik, antall), antall)
    ) +
      geom_col(fill = "#d97706", width = 0.72) +
      coord_flip() +
      geom_text(
        aes(label = antall),
        hjust = -0.1,
        size = 3.8
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.14))
      ) +
      labs(
        title = "Automatisk identifiserte avvik",
        subtitle = "Én registrering kan ha flere avvik",
        x = NULL,
        y = "Antall flagg"
      ) +
      theme_demo()
  })

  output$hospital_table <- renderDT({
    dat <- filtered()

    summary_data <- dat %>%
      group_by(hospital) %>%
      summarise(
        Registreringer = n(),
        Komplette = percent(
          mean(
            !is.na(psa) &
              !is.na(testosterone) &
              !is.na(shbg)
          ),
          accuracy = 0.1,
          decimal.mark = ","
        ),
        Med_avvik = percent(
          mean(avvik_any),
          accuracy = 0.1,
          decimal.mark = ","
        ),
        Duplikater = sum(flag_duplicate),
        Forsinket = sum(flag_delay),
        .groups = "drop"
      )

    names(summary_data) <- c(
      "Sykehus",
      "Registreringer",
      "Komplette",
      "Med avvik",
      "Duplikater",
      "Forsinket"
    )

    datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })

  output$quality_message <- renderUI({
    dat <- filtered()
    validate(
      need(nrow(dat) > 0, "Ingen data for valgte filtre.")
    )

    hospital_summary <- dat %>%
      group_by(hospital) %>%
      summarise(
        deviation_rate = mean(avvik_any),
        .groups = "drop"
      ) %>%
      arrange(desc(deviation_rate))

    top_hospital <- hospital_summary$hospital[1]
    top_rate <- hospital_summary$deviation_rate[1]

    tags$p(
      "Høyest andel flaggede registreringer finnes ved ",
      tags$strong(top_hospital),
      " (",
      percent(
        top_rate,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "). Dette er et naturlig startpunkt for kontroll av ",
      "registreringspraksis og dialog med den aktuelle enheten."
    )
  })

  output$histogram <- renderPlot({
    dat <- filtered() %>%
      filter(
        !is.na(psa),
        psa > 0,
        psa < 1000
      )

    validate(
      need(nrow(dat) > 0, "Ingen gyldige PSA-verdier.")
    )

    p <- ggplot(dat, aes(psa)) +
      geom_histogram(
        bins = 45,
        fill = "#2b7a9b",
        colour = "white",
        linewidth = 0.2
      ) +
      labs(
        title = "Fordeling av PSA",
        subtitle = "PSA = 0 og >1000 vises i avvikslisten",
        x = "PSA (ng/mL-ekvivalent)",
        y = "Antall"
      ) +
      theme_demo()

    if (isTRUE(input$log_x)) {
      p <- p +
        scale_x_log10(
          labels = label_number(decimal.mark = ","),
          breaks = c(0.1, 0.3, 1, 3, 10, 30, 100)
        )
    }

    p
  })

  output$percentile_table <- renderTable({
    x <- filtered()$psa
    x <- x[
      is.finite(x) &
        x >= 0 &
        x < 1000
    ]

    validate(
      need(length(x) > 0, "Ingen gyldige verdier.")
    )

    q <- quantile(
      x,
      probs = c(0.025, 0.50, 0.90, 0.95, 0.975),
      na.rm = TRUE
    )

    data.frame(
      Percentil = c(
        "P2,5",
        "P50",
        "P90",
        "P95",
        "P97,5"
      ),
      PSA = format(
        round(as.numeric(q), 2),
        decimal.mark = ","
      )
    )
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "s")

  output$threshold_table <- renderTable({
    x <- filtered()$psa
    x <- x[is.finite(x)]

    validate(
      need(length(x) > 0, "Ingen gyldige verdier.")
    )

    thresholds <- c(3, 4, 10)

    data.frame(
      Terskel = paste0("PSA ≥ ", thresholds),
      Antall = vapply(
        thresholds,
        function(z) {
          sum(x >= z, na.rm = TRUE)
        },
        numeric(1)
      ),
      Andel = vapply(
        thresholds,
        function(z) {
          percent(
            mean(x >= z, na.rm = TRUE),
            accuracy = 0.1,
            decimal.mark = ","
          )
        },
        character(1)
      )
    )
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "s")

  output$boxplot <- renderPlot({
    dat <- filtered() %>%
      filter(
        !is.na(psa),
        psa > 0,
        psa < 1000
      ) %>%
      mutate(
        age_group = cut(
          age,
          breaks = c(44, 54, 64, 74, Inf),
          labels = c(
            "45–54",
            "55–64",
            "65–74",
            "75+"
          )
        )
      )

    validate(
      need(nrow(dat) > 0, "Ingen gyldige PSA-verdier.")
    )

    ggplot(
      dat,
      aes(age_group, psa, fill = age_group)
    ) +
      geom_boxplot(
        outlier.alpha = 0.18,
        show.legend = FALSE
      ) +
      scale_y_log10(
        labels = label_number(decimal.mark = ",")
      ) +
      scale_fill_brewer(palette = "Blues") +
      labs(
        title = "PSA etter aldersgruppe",
        subtitle = "Logaritmisk y-akse",
        x = "Aldersgruppe",
        y = "PSA"
      ) +
      theme_demo()
  })

  output$hormone_plot <- renderPlot({
    dat <- filtered() %>%
      filter(
        !is.na(testosterone),
        !is.na(shbg)
      )

    validate(
      need(
        nrow(dat) > 0,
        "Ingen komplette hormonverdier."
      )
    )

    ggplot(
      dat,
      aes(shbg, testosterone, colour = age)
    ) +
      geom_point(alpha = 0.35, size = 1.3) +
      scale_colour_viridis_c(option = "C") +
      labs(
        title = "Testosteron og SHBG",
        x = "SHBG (nmol/L)",
        y = "Testosteron (nmol/L)",
        colour = "Alder"
      ) +
      theme_demo()
  })

  output$fti_plot <- renderPlot({
    dat <- filtered() %>%
      filter(
        !is.na(fti),
        !is.na(expected_fti)
      )

    validate(
      need(
        nrow(dat) > 0,
        "Ingen komplette FTI-verdier."
      )
    )

    upper <- max(
      c(dat$fti, dat$expected_fti),
      na.rm = TRUE
    )

    ggplot(
      dat,
      aes(
        expected_fti,
        fti,
        colour = flag_fti
      )
    ) +
      geom_abline(
        slope = 1,
        intercept = 0,
        colour = "#607487",
        linetype = 2
      ) +
      geom_point(alpha = 0.55, size = 1.5) +
      scale_colour_manual(
        values = c(
          "FALSE" = "#2b7a9b",
          "TRUE" = "#b42318"
        ),
        labels = c("Nei", "Ja")
      ) +
      coord_cartesian(
        xlim = c(0, upper),
        ylim = c(0, upper)
      ) +
      labs(
        title = "Intern konsistens i FTI",
        subtitle = paste(
          "Eksportert verdi mot",
          "10 × testosteron / SHBG"
        ),
        x = "Forventet FTI",
        y = "Registrert FTI",
        colour = "Avvik"
      ) +
      theme_demo()
  })

  deviation_data <- reactive({
    filtered() %>%
      filter(avvik_any) %>%
      select(
        record_id,
        sample_id,
        hospital,
        sample_date,
        received_date,
        age,
        psa,
        psa_unit,
        orig_psa,
        testosterone,
        shbg,
        fti,
        expected_fti,
        avvikstype
      )
  })

  output$deviation_table <- renderDT({
    dat <- deviation_data()

    names(dat) <- c(
      "Rad-ID",
      "Prøve-ID",
      "Sykehus",
      "Prøvedato",
      "Mottaksdato",
      "Alder",
      "PSA",
      "PSA-enhet",
      "Originalsvar",
      "Testosteron",
      "SHBG",
      "FTI",
      "Forventet FTI",
      "Avvikstype"
    )

    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "tip",
        order = list(
          list(2, "asc"),
          list(3, "asc")
        )
      )
    )
  })

  output$download_deviations <- downloadHandler(
    filename = function() {
      paste0(
        "fiktiv_avviksliste_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv2(
        deviation_data(),
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
}

shinyApp(ui, server)
