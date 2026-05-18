# 📊 PISA ESCS Trend Analysis (2012–2018)

Exploratory data analysis of **socioeconomic inequality in education** across 72 countries using PISA data from the OECD.

The index used — **ESCS** (Economic, Social and Cultural Status) — measures the socioeconomic background of students based on parental education, occupational status, and home resources. It is one of the most widely used indicators in international education research.

---

## 📁 Project Structure

```
pisa-escs-analysis/
├── escs_trend.csv          # Source data (OECD PISA Trend ESCS)
├── pisa_escs_plots.R       # Main analysis script (all 7 visualisations)
├── index.html              # Interactive project website
└── README.md
```

---

## 📈 Visualisations

| # | Chart | Type |
|---|-------|------|
| 1 | ESCS distribution by country | Boxplot |
| 2 | ESCS trend over cycles 2012–2018 | Line chart |
| 3 | ESCS vs parental education | Scatter + regression |
| 4 | Within-country inequality | Violin + jitter |
| 5 | Global ESCS map | Choropleth |
| 6 | OECD vs Non-OECD comparison | Density plot |
| 7 | Correlation between ESCS components | Heatmap |

---

## 🔧 Tools & Packages

- **Language**: R 4.x
- **Core**: `tidyverse`, `ggplot2`, `dplyr`
- **Visualisation**: `ggcorrplot`, `ggrepel`
- **Mapping**: `rnaturalearth`, `rnaturalearthdata`

Install all dependencies:

```r
install.packages(c(
  "tidyverse", "ggcorrplot", "ggrepel",
  "rnaturalearth", "rnaturalearthdata"
))
```

---

## 🚀 How to Run

1. Clone this repository
2. Place `escs_trend.csv` in the project root
3. Open `pisa_escs_plots.R` in RStudio
4. Run all — plots will appear in the Plots pane

To export all charts as PNG or PDF, uncomment the `ggsave` / `pdf()` block at the bottom of the script.

---

## 📂 Data Source

**OECD PISA Trend ESCS** — recomputed ESCS values for PISA cycles 2012, 2015, and 2018, standardised to be comparable with PISA 2022.

- ~1.4 million student records
- 72 countries and economies
- 3 PISA cycles (2012 / 2015 / 2018)

→ [PISA 2022 Technical Report](https://www.oecd.org/pisa/data/pisa2022technicalreport/)  
→ [OECD PISA Data](https://www.oecd.org/pisa/data/)

---

## 💡 Key Findings

- **OECD countries** consistently show higher and more equal ESCS distributions compared to non-OECD economies
- **Parental education** (`paredint`) is the strongest individual predictor of overall ESCS
- Several **non-OECD countries** show bimodal ESCS distributions — suggesting a split between elite and disadvantaged student populations
- Between 2012 and 2018, the **gap between top and bottom countries widened slightly**

---

*Course project · R Programming & Data Analysis*
