# Earthquake Activity Analysis

## Overview
This project analyzes earthquake data over the past 30 days, exploring magnitude distributions, temporal trends, location-specific intensity, and spatial patterns in California, Nevada, and Alaska using R.

## Dependencies
- R (>= 4.0)
- Packages: `tidyverse`, `tmap`, `tmaptools`, `forcats`, `wordcloud`, `sf`, `lsr`, `tidycensus`
- Install: `install.packages(c("tidyverse", "tmap", "tmaptools", "forcats", "wordcloud", "sf", "lsr", "tidycensus"))`

## Data
- Source: Custom dataset `all_month` (e.g., `.csv` with earthquake data)
- Location: `/data/earthquakes.csv` (include file or link to source)

## Usage
1. Clone the repository: `git clone https://github.com/your-username/earthquake-activity-analysis.git`
2. Open R and set the working directory to the repo folder.
3. Load data (e.g., `all_month <- read.csv("data/earthquakes.csv")`).
4. Run the script: `source("scripts/earthquake_analysis.R")`
5. Outputs:
   - Plots in `plots/` (e.g., magnitude histogram, maps)
   - Statistical results in `results/statistical_summary.txt`

## Key Findings
- Magnitude distribution peaks between 0-7.
- Significant differences in magnitude between California and Nevada/Alaska (see t-tests).

## Author
Shipu Debnath  
MS Student in Geography, Texas Tech University  
[LinkedIn](https://linkedin.com/in/your-profile) | [Google Scholar](https://scholar.google.com/citations?user=your-id)

## License
MIT License
