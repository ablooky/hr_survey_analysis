library(shiny)
library(plotly)

ui <- tagList(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@3.31.0/dist/tabler-icons.min.css"
    ),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$title("HR Analytics Dashboard")
  ),

  # ── Top Navbar ─────────────────────────────────────────────────────────────
  div(class = "navbar-custom",
    div(class = "navbar-brand-custom",
      div(class = "brand-icon",
        tags$i(class = "ti ti-chart-dots", style = "font-size:16px;")
      ),
      "HR Analytics"
    ),
    div(class = "navbar-right",
      span(class = "org-label", "Organization"),
      tags$select(
        id    = "org",
        class = "org-select",
        tags$option(value = "A", "Org A — NorthEast"),
        tags$option(value = "B", "Org B — West Coast"),
        tags$option(value = "C", "Org C — MidWest")
      )
    )
  ),

  # ── App layout ─────────────────────────────────────────────────────────────
  div(class = "app-layout",

    # ── Sidebar ───────────────────────────────────────────────────────────────
    div(class = "sidebar-custom",
      span(class = "sidebar-section-label", "Navigation"),

      div(class = "sidebar-item active", id = "nav_summary",
        onclick = "setPage('summary')",
        tags$i(class = "ti ti-layout-dashboard"),
        "Summary Statistics"
      ),
      div(class = "sidebar-item", id = "nav_demographics",
        onclick = "setPage('demographics')",
        tags$i(class = "ti ti-users"),
        "Demographics"
      ),
      div(class = "sidebar-item", id = "nav_engagement",
        onclick = "setPage('engagement')",
        tags$i(class = "ti ti-heart-rate-monitor"),
        "Employee Engagement"
      ),
      div(class = "sidebar-item", id = "nav_retention",
        onclick = "setPage('retention')",
        tags$i(class = "ti ti-user-check"),
        "Employment Retention"
      )
    ),

    # ── Main content ──────────────────────────────────────────────────────────
    div(class = "main-content",

      # ── SUMMARY ─────────────────────────────────────────────────────────────
      div(id = "page_summary",
        div(class = "page-header",
          h1(class = "page-title", "Summary Statistics"),
          p(class = "page-subtitle",
            "High-level workforce metrics and key performance indicators")
        ),

        div(class = "kpi-grid",
          div(class = "kpi-card blue",
            div(class = "kpi-icon", tags$i(class = "ti ti-users")),
            div(class = "kpi-value", textOutput("kpi_headcount", inline = TRUE)),
            div(class = "kpi-label", "Total headcount"),
            div(class = "kpi-delta up",
              tags$i(class = "ti ti-trending-up", style = "font-size:12px;"),
              " Active employees"
            )
          ),
          div(class = "kpi-card teal",
            div(class = "kpi-icon", tags$i(class = "ti ti-currency-dollar")),
            div(class = "kpi-value", textOutput("kpi_salary", inline = TRUE)),
            div(class = "kpi-label", "Avg. annual salary"),
            div(class = "kpi-delta up",
              tags$i(class = "ti ti-trending-up", style = "font-size:12px;"),
              " Across all levels"
            )
          ),
          div(class = "kpi-card amber",
            div(class = "kpi-icon", tags$i(class = "ti ti-clock")),
            div(class = "kpi-value", textOutput("kpi_tenure", inline = TRUE)),
            div(class = "kpi-label", "Avg. tenure (years)"),
            div(class = "kpi-delta",
              tags$i(class = "ti ti-minus", style = "font-size:12px;"),
              " Median workforce age"
            )
          ),
          div(class = "kpi-card red",
            div(class = "kpi-icon", tags$i(class = "ti ti-logout")),
            div(class = "kpi-value", textOutput("kpi_turnover", inline = TRUE)),
            div(class = "kpi-label", "Turnover rate"),
            div(class = "kpi-delta down",
              tags$i(class = "ti ti-trending-down", style = "font-size:12px;"),
              " Annual attrition"
            )
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Headcount by Department"),
                p(class = "chart-card-subtitle", "All active employees")
              ),
              span(class = "chart-badge", "Department")
            ),
            plotlyOutput("chart_dept", height = "260px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Salary Distribution"),
                p(class = "chart-card-subtitle", "By job level")
              ),
              span(class = "chart-badge", "Compensation")
            ),
            plotlyOutput("chart_salary_level", height = "260px")
          )
        ),

        div(class = "chart-card",
          div(class = "chart-card-header",
            div(
              p(class = "chart-card-title", "Performance Distribution"),
              p(class = "chart-card-subtitle", "Employee ratings across the organisation")
            )
          ),
          plotlyOutput("chart_performance", height = "220px")
        )
      ),

      # ── DEMOGRAPHICS ────────────────────────────────────────────────────────
      div(id = "page_demographics", style = "display:none;",
        div(class = "page-header",
          h1(class = "page-title", "Demographics"),
          p(class = "page-subtitle", "Workforce composition by gender, age, ethnicity and education")
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Gender breakdown"),
                p(class = "chart-card-subtitle", "All employees")
              )
            ),
            plotlyOutput("chart_gender", height = "240px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Age distribution"),
                p(class = "chart-card-subtitle", "Employee age histogram")
              )
            ),
            plotlyOutput("chart_age", height = "240px")
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Ethnicity"),
                p(class = "chart-card-subtitle", "Workforce diversity breakdown")
              )
            ),
            plotlyOutput("chart_ethnicity", height = "260px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Education level"),
                p(class = "chart-card-subtitle", "Highest qualification attained")
              )
            ),
            plotlyOutput("chart_education", height = "260px")
          )
        ),

        div(class = "chart-card",
          div(class = "chart-card-header",
            div(
              p(class = "chart-card-title", "Job level by gender"),
              p(class = "chart-card-subtitle",
                "Representation across levels")
            )
          ),
          plotlyOutput("chart_level_gender", height = "240px")
        )
      ),

      # ── ENGAGEMENT ──────────────────────────────────────────────────────────
      div(id = "page_engagement", style = "display:none;",
        div(class = "page-header",
          h1(class = "page-title", "Employee Engagement"),
          p(class = "page-subtitle",
            "Engagement scores, satisfaction, and training investment")
        ),

        div(class = "kpi-grid",
          div(class = "kpi-card blue",
            div(class = "kpi-icon", tags$i(class = "ti ti-mood-smile")),
            div(class = "kpi-value", textOutput("eng_score", inline = TRUE)),
            div(class = "kpi-label", "Avg. engagement score"),
            div(class = "kpi-delta up", "Out of 5.0")
          ),
          div(class = "kpi-card teal",
            div(class = "kpi-icon", tags$i(class = "ti ti-thumb-up")),
            div(class = "kpi-value", textOutput("sat_score", inline = TRUE)),
            div(class = "kpi-label", "Avg. satisfaction"),
            div(class = "kpi-delta up", "Out of 5.0")
          ),
          div(class = "kpi-card amber",
            div(class = "kpi-icon", tags$i(class = "ti ti-school")),
            div(class = "kpi-value", textOutput("train_hours", inline = TRUE)),
            div(class = "kpi-label", "Avg. training hours"),
            div(class = "kpi-delta", "Per employee / year")
          ),
          div(class = "kpi-card red",
            div(class = "kpi-icon", tags$i(class = "ti ti-award")),
            div(class = "kpi-value", textOutput("exceeds_pct", inline = TRUE)),
            div(class = "kpi-label", "Exceeds expectations"),
            div(class = "kpi-delta up", "High performers")
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Engagement score distribution"),
                p(class = "chart-card-subtitle", "All employees, 1–5 scale")
              )
            ),
            plotlyOutput("chart_eng_dist", height = "240px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Engagement by department"),
                p(class = "chart-card-subtitle", "Average score per team")
              )
            ),
            plotlyOutput("chart_eng_dept", height = "240px")
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Training hours by department"),
                p(class = "chart-card-subtitle", "Annual average")
              )
            ),
            plotlyOutput("chart_training", height = "240px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Engagement vs. satisfaction"),
                p(class = "chart-card-subtitle", "Scatter by job level")
              )
            ),
            plotlyOutput("chart_eng_sat", height = "240px")
          )
        )
      ),

      # ── RETENTION ───────────────────────────────────────────────────────────
      div(id = "page_retention", style = "display:none;",
        div(class = "page-header",
          h1(class = "page-title", "Employment Retention"),
          p(class = "page-subtitle",
            "Attrition risk, tenure analysis and retention drivers")
        ),

        div(class = "kpi-grid",
          div(class = "kpi-card red",
            div(class = "kpi-icon", tags$i(class = "ti ti-user-minus")),
            div(class = "kpi-value", textOutput("ret_left", inline = TRUE)),
            div(class = "kpi-label", "Employees departed"),
            div(class = "kpi-delta down", "This cycle")
          ),
          div(class = "kpi-card blue",
            div(class = "kpi-icon", tags$i(class = "ti ti-user-check")),
            div(class = "kpi-value", textOutput("ret_rate", inline = TRUE)),
            div(class = "kpi-label", "Retention rate"),
            div(class = "kpi-delta up", "Active workforce")
          ),
          div(class = "kpi-card teal",
            div(class = "kpi-icon", tags$i(class = "ti ti-clock")),
            div(class = "kpi-value", textOutput("ret_tenure", inline = TRUE)),
            div(class = "kpi-label", "Avg. tenure (stayers)"),
            div(class = "kpi-delta up", "Years of service")
          ),
          div(class = "kpi-card amber",
            div(class = "kpi-icon", tags$i(class = "ti ti-alert-triangle")),
            div(class = "kpi-value", textOutput("ret_risk_dept", inline = TRUE)),
            div(class = "kpi-label", "Highest-risk dept."),
            div(class = "kpi-delta down", "By attrition count")
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Attrition by department"),
                p(class = "chart-card-subtitle", "Percentage who left")
              ),
              span(class = "chart-badge", "Risk")
            ),
            plotlyOutput("chart_attr_dept", height = "260px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Attrition by job level"),
                p(class = "chart-card-subtitle", "Who is leaving?")
              )
            ),
            plotlyOutput("chart_attr_level", height = "260px")
          )
        ),

        div(class = "chart-grid-2",
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Tenure distribution"),
                p(class = "chart-card-subtitle", "Stayers vs. leavers")
              )
            ),
            plotlyOutput("chart_tenure_dist", height = "240px")
          ),
          div(class = "chart-card",
            div(class = "chart-card-header",
              div(
                p(class = "chart-card-title", "Salary vs. attrition"),
                p(class = "chart-card-subtitle",
                  "Compensation as a retention driver")
              )
            ),
            plotlyOutput("chart_salary_attr", height = "240px")
          )
        )
      )
    ) # /main-content
  ), # /app-layout

  # ── Navigation JS ───────────────────────────────────────────────────────────
  tags$script(HTML("
    var pages = ['summary', 'demographics', 'engagement', 'retention'];

    function setPage(p) {
      pages.forEach(function(page) {
        document.getElementById('page_' + page).style.display =
          page === p ? '' : 'none';
        var nav = document.getElementById('nav_' + page);
        if (nav) nav.className = 'sidebar-item' + (page === p ? ' active' : '');
      });
    }

    // Bind org dropdown to Shiny input
    document.querySelector('.org-select').addEventListener('change', function() {
      Shiny.setInputValue('org', this.value);
    });
  "))
)
