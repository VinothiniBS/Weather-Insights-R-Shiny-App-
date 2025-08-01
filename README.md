# 🌦️ Weather Insights (R Shiny App)

**Live App** 👉 [Click to View](https://ejdump-vinothini-balasubramani.shinyapps.io/weatherinsights/)

Weather Insights is an interactive R Shiny application designed to analyze meteorological data across different locations, years, and seasons in Ireland.

---

## 🔍 Features

- 📊 Tab 1: App Overview and Feature Summary
- 🌦️ Tab 2: Analyze weather variables by **location** and **year**
- 🍂 Tab 3: Visualize seasonal weather patterns using **boxplots**
- 🎨 Styled interface with transparent panels

---

## 📁 Dataset

- Public weather datasets from three Irish weather stations:
  - Belmullet
  - Dublin Airport
  - Malin Head
- Variables include: `rain`, `maxtp`, `mintp`, `soil`, `wdsp`, `hg`, `sun`, `evap`

---

## 🛠️ Tech Stack

- R Shiny
- ggplot2
- dplyr
- purrr
- HTML/CSS custom styling

---

## 🚀 How to Run Locally

```r
# Clone the repo and run this in RStudio
shiny::runApp()
