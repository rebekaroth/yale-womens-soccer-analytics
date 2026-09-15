# Yale Women’s Soccer Analytics Dashboard

An interactive football analytics dashboard built in **R Shiny** for Yale Women’s Soccer coaches, using Wyscout data to support player evaluation, roster decisions, and opponent scouting.

## Project Overview

Yale Women’s Soccer had access to detailed performance data through Wyscout, but the coaching staff did not have an easy way to turn the raw data into actionable insights.

This project was developed as my senior thesis in Yale University’s Department of Statistics and Data Science. The goal was to create an accessible analytical tool that allows coaches to compare players, goalkeepers, and teams without needing to work directly with raw spreadsheets.

## Dashboard

The Shiny app contains three main sections:

- **Players** — compare Ivy League outfield players using scatter plots and radar charts
- **Goalkeepers** — evaluate goalkeepers using position-specific performance metrics
- **Teams** — compare Ivy League teams across attacking and defensive metrics

The dashboard includes interactive filtering and customizable visualizations designed to make performance comparisons intuitive for coaches.

## Key Features

- Interactive player and team selection
- Position-specific player comparisons
- Per-90 performance metrics
- Percentile-based radar charts
- League and positional benchmarks
- Team analysis across multiple seasons
- Recent-form filters for team performance
- Player, goalkeeper, and team-specific visualizations

The player scatter plots allow users to compare metrics such as finishing efficiency, passing, duels, shooting, possession impact, and dribbling. The dashboard also allows comparisons against league-wide or positional averages.
## Data Processing

The project required significant preprocessing before the data could be used effectively in the dashboard.

Key steps included:

- Combining individual Wyscout Excel files into structured datasets
- Standardizing detailed position labels into broader positional groups
- Filtering players with very limited playing time
- Converting raw statistics into **per-90 metrics**
- Creating success-rate variables from total and successful actions
- Restructuring team-level data to include opponent statistics

These steps were designed to make comparisons between players and teams more meaningful and consistent.

## Example Insights

The dashboard was designed not only for visualization, but also to help identify tactical patterns.

One example from the analysis showed that Yale maintained one of the highest possession rates in the Ivy League, while producing relatively few shots on target. Further analysis suggested that much of the possession occurred within the defensive unit rather than being converted into attacking opportunities.

## Technology

- **R**
- **Shiny**
- Data cleaning and transformation
- Interactive visualization
- Football performance analysis

## Data

The project uses proprietary **Wyscout** player and team performance data covering Ivy League teams and players.

Because the underlying dataset is not publicly available, the raw data is **not included in this repository**.

## Limitations

The main limitation of the project was the data pipeline. Without access to the Wyscout API, data updates had to be completed manually through downloaded files.

Future improvements could include:

- Automated data updates through API access
- Additional filtering options
- More advanced performance metrics
- Expanded benchmarking and comparison features

## Academic Work

This project was completed as my senior thesis in Yale University’s Department of Statistics and Data Science.

- **Written thesis:** [Add link]
- **Academic poster:** [Add link]

