library(shiny)
library(ggplot2)
library(kableExtra)
library(dplyr)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      @import url('https://fonts.googleapis.com/css2?family=DM+Serif+Display&family=DM+Mono:wght@400;500&display=swap');

      body {
        background: #0f0f13;
        color: #e8e4d9;
        font-family: 'DM Mono', monospace;
        margin: 0;
        padding: 0;
      }

      .app-header {
        background: linear-gradient(135deg, #0f0f13 0%, #1a1a24 100%);
        border-bottom: 1px solid #2a2a3a;
        padding: 28px 40px 20px;
        margin-bottom: 0;
      }

      .app-title {
        font-family: 'DM Serif Display', serif;
        font-size: 2rem;
        color: #f0ebe0;
        letter-spacing: -0.5px;
        margin: 0 0 4px;
      }

      .app-subtitle {
        font-size: 0.72rem;
        color: #6b6b85;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        margin: 0;
      }

      .main-layout {
        display: grid;
        grid-template-columns: 280px 1fr;
        gap: 0;
        min-height: calc(100vh - 90px);
      }

      .sidebar-panel {
        background: #13131c;
        border-right: 1px solid #22222e;
        padding: 28px 22px;
      }

      .sidebar-section-label {
        font-size: 0.65rem;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #5a5a72;
        margin: 22px 0 12px;
        padding-bottom: 6px;
        border-bottom: 1px solid #1e1e2a;
      }

      .sidebar-section-label:first-child { margin-top: 0; }

      .shiny-input-container {
        margin-bottom: 16px !important;
      }

      label {
        font-size: 0.72rem !important;
        color: #9090a8 !important;
        letter-spacing: 0.06em;
        text-transform: none;
        margin-bottom: 4px !important;
        font-family: 'DM Mono', monospace !important;
      }

      input[type='number'] {
        background: #0f0f16 !important;
        border: 1px solid #2a2a3a !important;
        color: #e8e4d9 !important;
        border-radius: 4px !important;
        font-family: 'DM Mono', monospace !important;
        font-size: 0.78rem !important;
        padding: 5px 8px !important;
      }

      .irs--shiny .irs-bar { background: #7b68ee !important; border-top: 1px solid #7b68ee !important; border-bottom: 1px solid #7b68ee !important; }
      .irs--shiny .irs-bar-edge { background: #7b68ee !important; border: 1px solid #7b68ee !important; }
      .irs--shiny .irs-handle { background: #a09af0 !important; border: 2px solid #7b68ee !important; box-shadow: 0 0 6px rgba(123,104,238,0.4) !important; }
      .irs--shiny .irs-single { background: #7b68ee !important; font-family: 'DM Mono', monospace !important; font-size: 0.7rem !important; }
      .irs--shiny .irs-line { background: #2a2a3a !important; border-color: #2a2a3a !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #555570 !important; font-family: 'DM Mono', monospace !important; font-size: 0.65rem !important; }

      .content-panel {
        background: #0f0f13;
        padding: 28px 32px;
        display: flex;
        flex-direction: column;
        gap: 24px;
      }

      .plot-card {
        background: #13131c;
        border: 1px solid #22222e;
        border-radius: 8px;
        padding: 20px;
      }

      .table-card {
        background: #13131c;
        border: 1px solid #22222e;
        border-radius: 8px;
        padding: 20px;
      }

      .card-label {
        font-size: 0.62rem;
        letter-spacing: 0.2em;
        text-transform: uppercase;
        color: #5a5a72;
        margin-bottom: 14px;
      }

      /* kable table styling */
      table {
        width: 100%;
        border-collapse: collapse;
        font-size: 0.78rem;
        font-family: 'DM Mono', monospace;
      }

      thead tr th {
        background: #1a1a26 !important;
        color: #9090a8 !important;
        font-size: 0.65rem !important;
        letter-spacing: 0.12em !important;
        text-transform: uppercase !important;
        padding: 10px 14px !important;
        border-bottom: 1px solid #2a2a3a !important;
        font-weight: 500 !important;
      }

      tbody tr td {
        padding: 9px 14px !important;
        border-bottom: 1px solid #1e1e2a !important;
        color: #c8c4b8 !important;
      }

      tbody tr:last-child td { border-bottom: none !important; }
      tbody tr:hover td { background: #1a1a24 !important; }

      /* color-coded row labels */
      .prior-color   { color: #f4a95a !important; font-weight: 500; }
      .data-color    { color: #5ab8f4 !important; font-weight: 500; }
      .post-color    { color: #a07bf4 !important; font-weight: 500; }
    "
    ))
  ),

  # Header
  div(
    class = "app-header",
    h1("Normal–Normal Bayesian Updater", class = "app-title"),
    p("Conjugate prior · Scaled likelihood · Posterior", class = "app-subtitle")
  ),

  # Body grid
  div(
    class = "main-layout",

    # ── Sidebar ───────────────────────────────────────────────────────────────
    div(
      class = "sidebar-panel",

      div(class = "sidebar-section-label", "Prior  μ ~ Normal(μ₀, σ₀)"),
      sliderInput(
        "mu0",
        "Prior mean μ₀",
        min = 80,
        max = 120,
        value = 98.6,
        step = 0.1
      ),
      sliderInput(
        "sigma0",
        "Prior SD σ₀",
        min = 0.1,
        max = 5,
        value = 0.3,
        step = 0.05
      ),

      div(class = "sidebar-section-label", "Likelihood  yᵢ | μ ~ Normal(μ, σ)"),
      sliderInput(
        "sigma",
        "Known σ",
        min = 0.1,
        max = 5,
        value = 1,
        step = 0.05
      ),
      sliderInput(
        "n",
        "Sample size n",
        min = 1,
        max = 500,
        value = 208,
        step = 1
      ),
      sliderInput(
        "ybar",
        "Sample mean ȳ",
        min = 80,
        max = 120,
        value = 97.7,
        step = 0.1
      )
    ),

    # ── Main content ──────────────────────────────────────────────────────────
    div(
      class = "content-panel",

      div(
        class = "plot-card",
        div(class = "card-label", "Density curves"),
        plotOutput("density_plot", height = "360px")
      ),

      div(
        class = "table-card",
        div(class = "card-label", "Summary table — precision · SD · mean"),
        tableOutput("summary_table")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Posterior parameters (Normal-Normal conjugate update)
  posterior <- reactive({
    tau0 <- input$sigma0 # prior SD
    sigma <- input$sigma
    n <- input$n
    mu0 <- input$mu0
    ybar <- input$ybar

    prec_prior <- 1 / tau0^2
    prec_data <- n / sigma^2
    prec_post <- prec_prior + prec_data

    tau_n <- 1 / sqrt(prec_post)
    mu_n <- (mu0 * prec_prior + ybar * prec_data) / prec_post

    list(
      prec_prior = prec_prior,
      sd_prior = tau0,
      mean_prior = mu0,
      prec_data = prec_data,
      sd_data = sigma / sqrt(n),
      mean_data = ybar,
      prec_post = prec_post,
      sd_post = tau_n,
      mean_post = mu_n
    )
  })

  # ── Plot ───────────────────────────────────────────────────────────────────
  output$density_plot <- renderPlot(
    {
      p <- posterior()

      # x range: cover all three distributions generously
      lo <- min(
        p$mean_prior - 4 * p$sd_prior,
        p$mean_data - 4 * p$sd_data,
        p$mean_post - 4 * p$sd_post
      )
      hi <- max(
        p$mean_prior + 4 * p$sd_prior,
        p$mean_data + 4 * p$sd_data,
        p$mean_post + 4 * p$sd_post
      )
      x <- seq(lo, hi, length.out = 800)

      d_prior <- dnorm(x, p$mean_prior, p$sd_prior)
      d_data <- dnorm(x, p$mean_data, p$sd_data)
      d_post <- dnorm(x, p$mean_post, p$sd_post)

      # Scale likelihood so area ≈ 1 for visual comparison (already a Normal density)
      df <- data.frame(
        x = rep(x, 3),
        dens = c(d_prior, d_data, d_post),
        dist = factor(
          rep(c("Prior", "Scaled Likelihood", "Posterior"), each = length(x)),
          levels = c("Prior", "Scaled Likelihood", "Posterior")
        )
      )

      pal <- c(
        "Prior" = "#f4a95a",
        "Scaled Likelihood" = "#5ab8f4",
        "Posterior" = "#a07bf4"
      )

      ggplot(df, aes(x = x, y = dens, colour = dist, fill = dist)) +
        geom_ribbon(aes(ymin = 0, ymax = dens), alpha = 0.12, linewidth = 0) +
        geom_line(linewidth = 1.05) +
        scale_colour_manual(values = pal, name = NULL) +
        scale_fill_manual(values = pal, name = NULL) +
        labs(x = "μ", y = "Density") +
        theme_minimal(base_family = "mono") +
        theme(
          plot.background = element_rect(fill = "#13131c", colour = NA),
          panel.background = element_rect(fill = "#13131c", colour = NA),
          panel.grid.major = element_line(colour = "#22222e", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "#6b6b85", size = 9),
          axis.title = element_text(colour = "#9090a8", size = 10),
          legend.position = "top",
          legend.text = element_text(
            colour = "#c8c4b8",
            size = 9,
            family = "mono"
          ),
          legend.background = element_rect(fill = "#13131c", colour = NA),
          legend.key = element_rect(fill = "#13131c", colour = NA)
        )
    },
    bg = "#13131c"
  )

  # ── Table ──────────────────────────────────────────────────────────────────
  output$summary_table <- renderTable(
    {
      p <- posterior()

      data.frame(
        ` ` = c("Precision", "SD", "Mean"),
        Prior = c(
          round(p$prec_prior, 3),
          round(p$sd_prior, 3),
          round(p$mean_prior, 3)
        ),
        `Data (Sample Mean)` = c(
          round(p$prec_data, 3),
          round(p$sd_data, 3),
          round(p$mean_data, 3)
        ),
        Posterior = c(
          round(p$prec_post, 3),
          round(p$sd_post, 3),
          round(p$mean_post, 3)
        ),
        check.names = FALSE
      )
    },
    striped = FALSE,
    hover = TRUE,
    bordered = FALSE,
    spacing = "m",
    align = "lrrr",
    width = "100%",
    rownames = FALSE
  )
}

shinyApp(ui, server)
