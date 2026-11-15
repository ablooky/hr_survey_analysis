# HR Analytics Dashboard — R Shiny

A modern, interactive HR analytics dashboard built with R Shiny and Plotly.

## Structure

```
hr_dashboard/
├── app.R            # Entry point — run this
├── R/
│   ├── data.R       # Synthetic HR data generator
│   ├── ui.R         # UI layout (navbar, sidebar, all four pages)
│   └── server.R     # Server logic (KPIs, all charts)
└── www/
    └── styles.css   # Custom modern styling
```

## Requirements

```r
install.packages(c("shiny", "plotly", "dplyr"))
```

## Run the app

```r
shiny::runApp("hr_dashboard")
# or from inside the directory:
shiny::runApp()
```

## Features

### Organization selector (top-right)
Switch between **Org A** (NorthEast, n≈320), **Org B** (West Coast, n≈210),
and **Org C** (MidWest, n≈150) — all charts update reactively.

### Four sidebar sections

| Section | What you'll find |
|---|---|
| **Summary Statistics** | Headcount, avg salary, tenure, turnover KPIs; dept headcount bar, salary-by-level box plots, performance pie |
| **Demographics** | Gender donut, age histogram, ethnicity bar, education bar, job-level-by-gender grouped bar |
| **Employee Engagement** | Engagement & satisfaction KPIs + histograms, dept-level engagement, training hours, engagement vs. satisfaction scatter |
| **Employment Retention** | Departed count, retention rate, stayers tenure, highest-risk dept; attrition by dept & level, tenure-distribution overlay, salary vs. attrition |

## Plug in your own data

Replace the data frame produced by `generate_hr_data()` in `R/data.R`
with your real dataset. The expected columns are:

| Column | Type | Description |
|---|---|---|
| `org` | chr | "A", "B", or "C" |
| `dept` | chr | Department name |
| `job_level` | chr | Junior / Mid / Senior / Lead / Manager / Director |
| `gender` | chr | Gender label |
| `ethnicity` | chr | Ethnicity label |
| `education` | chr | Highest qualification |
| `age` | int | Employee age |
| `tenure_years` | dbl | Years at company |
| `salary` | int | Annual salary USD |
| `engagement_score` | dbl | 1–5 scale |
| `satisfaction_score` | dbl | 1–5 scale |
| `performance` | chr | "Exceeds" / "Meets" / "Needs Improvement" |
| `training_hours` | int | Annual training hours |
| `left` | int | 1 = departed, 0 = active |
