# Capytool

Web access at: https://1s4aja-kashish-kumar.shinyapps.io/rshiny-app/

## Overview

- Capytool is an advanced R Shiny application developed for the STAT 4243 Project 3: A/B/C Testing.
- The app provides interactive data processing, visualization, and modeling tools, guiding users through every step from data upload to predictive modeling.
- Features three distinct user interface designs (A/B/C testing) to study user interaction patterns and effectiveness of different UI elements.

## User Testing Groups

The application implements a three-way testing approach to evaluate different user interface designs:

- **Group A (Control)**: Standard interface with blue buttons and conventional layout
- **Group B (Enhanced Visual Cues)**: Red buttons with intuitive icons for better visual guidance:
  - ➜ Next button
  - ✓ Apply Preprocessing button
  - ⚡ Feature Engineering button
  - ▶ Run Model button
- **Group C (Interactive Help)**: Standard blue buttons with comprehensive tooltips providing contextual help and explanations for each feature

Users are randomly assigned to one of these groups upon accessing the application, and their interactions are logged for analysis.

## Note

For comprehensive documentation about Capytool's features, please refer to this repo: https://github.com/hanvitC/rshiny-app.

## Installation

To run Capytool locally, ensure that you have R installed along with the required packages. Install them using the following command in R:

```r
install.packages(c("shiny", "shinyWidgets", "shinythemes", "dplyr", "DT", "readr", "readxl", "jsonlite", "plotly", "shinycssloaders", "caret", "randomForest", "gbm", "zip", "webshot", "reshape2"))
```

Additionally, install and configure PhantomJS for the webshot package:

```r
webshot::install_phantomjs()
```

## Usage

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/Capytool.git
   ```

2. **Open in RStudio:**  
   Open the project directory in RStudio.

3. **Run the App:**

   In the R console, run:

   ```r
   shiny::runApp("app.R")
   ```

## Files

- `app.R`: The main file containing the full Shiny application code.
- `data/`: Directory containing sample datasets used within the app (if applicable).

## Collaborators

- [Han Choi](https://github.com/hanvitC)
- [Kashish Kumar](https://github.com/kashishky)
- [Jimin Park](https://github.com/jp4632)
- [Shenghong Wu](https://github.com/Yang5356)

## Acknowledgments

- Special thanks to Instructor Alex Pijyan for guidance and support.
- Appreciation to the R community and the developers of the various R packages used in this project.
