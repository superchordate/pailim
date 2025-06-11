# Palestine vs. Israel Violence Data Visualization

An interactive R Shiny web application for visualizing and analyzing two parallel datasets documenting violence between Israelis and Palestinians.

## Overview

This application provides comprehensive visualization tools for analyzing patterns in Israeli-Palestinian conflict data through interactive maps and time-series charts. The application works with two complementary datasets:

- **Israeli violence against Palestinians**
- **Palestinian violence against Israelis**

These datasets share many common variables while maintaining distinct characteristics, allowing for both separate analysis and comparative visualization.

## Features

### Interactive Maps

#### 1. Event Mapping
- **Location-based visualization**: Plot all events using geocoded coordinates (X.1, Y.1)
- **Event density**: Circle size indicates number of events at each location
- **Filtering options**:
  - Time period (Year, Month)
  - Violence type (Primary.Violence)
  - Perpetrator identification (Perpetrator.1-.2, Perpetrator.Origin.1-.3)
  - Victim identification (Victim.1-.4)

#### 2. Casualty Mapping
- **Casualty visualization**: Map injuries, deaths, and total casualties by location
- **Same filtering capabilities** as event mapping for comprehensive analysis

#### 3. Detention/Arrest Mapping
- **Detention tracking**: Visualize detention and arrest data (Detained.Arrested) by location
- **Time-based filtering**: Filter by year and month

#### 4. Specialized Violence Mapping
- **Multiple violence types**: Rockets (Rocket.Number), incendiary balloons (Balloon.Number), riots, and riot subcategories
- **Riot analysis**: Detailed breakdown using Crimes and Riot.Sub.Category
- **Temporal filtering**: Year and month-based subsetting

### Time-Series Analysis

#### 5. Event Trends Over Time
- **Flexible time binning**: Weekly, monthly, quarterly, or annual aggregation
- **Custom time ranges**: Select specific years, months, or date ranges
- **Multi-dimensional filtering**:
  - Violence type (e.g., rockets, riots)
  - Perpetrator/victim identification
  - Geographic location (Region, City variables)

#### 6. Casualty Trends
- **Temporal casualty analysis** with same binning and filtering options as events
- **Location-based filtering**: 
  - Palestinian violence: Region, City.1-.8
  - Israeli violence: Area, Governorate, Town

#### 7. Detention Trends
- **Arrest/detention patterns** over time with temporal and geographic filtering

#### 8. Specialized Violence Trends
- **Rocket, balloon, and riot analysis** over time with comprehensive filtering options

### Contextual Data Integration

#### 9. Economic and Social Covariates
Add contextual line graphs to understand broader patterns:
- **Economic indicators**: CPI, unemployment rates, trade balances
- **Financial markets**: Stock indices (PASISI.PX_CLOSE, TA125.PX_CLOSE)
- **Exchange rates**: Currency fluctuation data
- **Border activity**: Gaza-Israel crossing, import/export data
- **Infrastructure**: Daily demolitions and displacement data
- **Environmental**: Temperature (TAVG) and rainfall (PRCP) data

#### 10. Political Context Shading
Highlight significant political periods:
- Major Israeli military operations
- Hamas-Fatah reconciliation talks
- US Secretary of State tenures
- US Presidential terms
- Israeli coalition composition (size, right-wing representation)

#### 11. Event Markers
Add vertical lines for specific events:
- International speeches
- Israeli elections
- UN Security Council/General Assembly votes
- High-level diplomatic visits (US officials, Israeli delegations)

### Data Access

#### 12. Data Download
- **Controlled access**: Email and institutional affiliation required
- **Full dataset export** capabilities for research purposes

## Dataset Compatibility

### Common Variables
Both datasets share key variables enabling comparative analysis:
- Case identification and validation (Case.ID, Coder.ID, Cross.Validator.ID)
- Temporal data (Year, Month, Day)
- Geographic information (Region, Area, City variations)
- Violence classification and outcomes
- Casualty data (Injured, Killed, Casualties)

### Dataset-Specific Variables
Each dataset contains unique variables reflecting different aspects of the conflict, with some variables having different option sets or ranges between datasets.

## Technical Requirements

### Prerequisites
- R (version 4.0 or higher)
- Required R packages:
  - `shiny`
  - `shinydashboard`
  - `shinyjs`
  - `tidyverse`
  - `lubridate`
  - `readr`
  - `dplyr`
  - `tibble`
  - Additional visualization and data manipulation packages

### Data Preparation

Before running the application, you need to build the processed datasets from the raw CSV files:

#### Building the Data
1. **Navigate to the data directory**:
   ```r
   setwd("data/")
   ```

2. **Run the data processing script**:
   ```r
   source("build-data.R")
   ```

#### What the build-data.R Script Does:
- **Loads raw datasets**: Reads `Palestinian.Violence.Covariates_new.csv` and `Israeli.Violence.Covariates_new.csv`
- **Geocode expansion**: Expands multiple geocodes (X.2-X.8, Y.2-Y.8) into separate observations for comprehensive mapping
- **Data standardization**: 
  - Converts factors to characters for consistent handling
  - Standardizes month names using abbreviated format
  - Creates unified longitude/latitude columns
- **Variable harmonization**:
  - Creates consistent perpetrator and victim type categories
  - Standardizes riot subcategories and violence types
  - Handles multiple perpetrators/victims scenarios
- **Temporal processing**: Adds Week, MonthNum, and Quarter variables using lubridate
- **Data filtering**: Filters to years 2010 and later, removes invalid coordinates
- **Combined dataset creation**: Creates a merged dataset (`cm`) with comparable variables
- **Output generation**: Saves processed data as `data.RData` in the app directory

#### Important Notes:
- The script includes an option to sample Israeli data (currently commented out) for development purposes
- Raw CSV files use Windows-1252 encoding for proper character handling
- The script cleans verbatim reports by removing problematic characters
- Final datasets are filtered to start from 2010 for consistency

### Installation
1. Clone this repository
2. Install required R packages:
   ```r
   install.packages(c("shiny", "shinydashboard", "shinyjs", 
                     "tidyverse", "lubridate", "readr", "dplyr", "tibble"))
   ```
3. **Build the data** (run `build-data.R` as described above)
4. Run the Shiny application from the app directory

### Data Files
- `data.RData`: Main dataset
- `data_subset.RData`: Subset for testing/development
- `Israeli.Violence.Covariates_new.csv`: Israeli violence contextual data
- `Palestinian.Violence.Covariates_new.csv`: Palestinian violence contextual data
- `Codebook.pdf`: Comprehensive variable documentation

## Usage

1. **Select Dataset**: Choose between Palestinian actions, Israeli actions, or both
2. **Choose Visualization**: Select maps or time-series charts
3. **Apply Filters**: Use temporal, geographic, and categorical filters
4. **Add Context**: Include covariate data and political context
5. **Export Data**: Download filtered datasets for further analysis

## Project Structure

```
prior-app-new/
├── app/
│   ├── ui.R              # User interface definition
│   ├── server.R          # Server logic
│   ├── global.R          # Global variables and functions
│   ├── data.RData        # Main dataset
│   ├── data_subset.RData # Development subset
│   ├── *.csv             # Covariate data files
│   ├── Codebook.pdf      # Variable documentation
│   └── www/              # Static assets (flags, images)
├── data/
│   └── build-data.R      # Data processing scripts
└── README.md
```

## Contributing

This research tool is designed for academic and policy analysis. For questions about data methodology or to report issues, please contact the research team.

## Data Citation

Please cite appropriately when using this data for research or publication purposes. Refer to the Codebook.pdf for detailed attribution requirements.