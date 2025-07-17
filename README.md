# Pa'Ilim App

This application allows users to explore the dataset created by Carly Wayne. 


## Project Setup

To **run this project locally**, you will need to install [RStudio](https://posit.co/download/rstudio-desktop/). 

You can use the script below to install the necessary R packages: 

```r
install.packages(c(
  "easyr", "qs", "qs2", "tidyverse", "lubridate", "readr", "dplyr",
  "tibble", "magrittr", "glue", "data.table", "tidyr", "shinyjs",
  "leaflet", "shinycssloaders", "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly",
  "scales", "stringr"
))
```

To **deploy an update to the app**, you will need [Node.js](https://nodejs.org/en/download) and the [gcloud utility](https://cloud.google.com/sdk/docs/install) installed. 

You can install the necessary node packages with command `npm install`.


## Project Structure

The main folders in this project are:

- `.github/`: Baseline instructions for GitHub Copilot.
- `app/`: Contains the main R Shiny application code, including UI, server logic, and supporting modules.
- `data/`: Scripts and files for data processing, raw and processed datasets, and cache.
- `admin/`: Deployment and administrative scripts for managing the app.
- `docs/`: Documentation and sample data. Used for giving context to GitHub Copilot. 
- `experiments/`: Exploratory R scripts for working with the data outside build processes.

These files are in the project root folder:

- `.gcloudignore`, `Dockerfile`: Files used to build the application on the cloud. 
- `.gitignore`: Indicates files that are not tracked by Git. 
- `package.json`: Node packages necessary for the deploy script.
- `README.md`: This file, provides important information about the project. 

The app is tracked by Git and backed up to https://github.com/superchordate/pailim.git. 

Raw data files are saved on [DropBox](https://www.dropbox.com/scl/fo/vvp5z8a8gchihkyxsw2cl/ADiFmSAH1GZPsEsSvt_e-vg?rlkey=5zqygjr86xb35lwx7s051b7nk&st=ky372ew8&dl=0). 

The website is hosted on [Google Cloud Platform](https://console.cloud.google.com/welcome?inv=1&invt=Ab281g&project=pailim-dataset&authuser=1). 


## Deploying the App

To deploy the app, run the Node command `npm run deploy`. This will run `admin/deploy.js` which builds the container, loads it to the artifact repository, updates the Cloud Run service, and deletes the artifact to avoid unnecessary storage fees. 


## Building the Data

Before using the app, you may need to process the raw data into data the app can use. 

* Make sure the raw data is placed at `data/raw-data`.
    - Covariates (2009-2023).csv
    - District Covariates (2009-2023).csv
    - Israeli Actions (2009-2023).csv
    - Palestinian Actions (2009-2023).csv

* Run `data/build-data.R` which will create the file `app/data.qs2`. 

These scripts will run in the scripts/ folder:

- `0-cache.R`: Sets up the cache for faster re-runs. 
- `1-read-data.R`: Reads Israeli Actions (2009-2023).csv and Palestinian Actions (2009-2023).csv.
- `2-simple-prep.R`: Performs simple prep steps for both il and pa. 
- `3-loop-prep-pa.R`, `4-loop-prep-il.R`: Performs prep that requires a loop, splitting multi-event rows into multiple rows.
- `5-postloop-prep.R`: Performs simple prep steps that can't be done before the loop. 
- `7-covariates.R`: Read Covariates (2009-2023).csv and District Covariates (2009-2023).csv and process the data for use in the app.
- `8-select-cols.R`: Reduce the data to the fields needed for the app, to reduce RAM usage and make loading times faster. 

You can now run the app by opening `app/global.R` and clicking "Run App" in RStudio. 


## App Files

The `app/` folder is organized as follows:

- `data.qs2`: The data the app uses, processed and compressed via `data/build-data.R`.
- `global.R`: Global settings. 
- `ui.R`: Partial code specifying organization of the user interface. This file is mostly to indicate the static portions of the UI, though there is some dynamic UI in there via conditionalPanel. The bulk of the dynamic UI is at `server/ui.R` and `app/server/_init/ui-critical.R`.
- `server.R`: File that reads in server files from the `server/` folder. 
- `server/`: Server files containing the code that runs on the server. Split into many .R files for focused code management. The` _init/` folder contains files that must run first. Descriptive file names are used to indicate the purpose of each file. 
- `www/`: Static and public files like images, JavaScript libraries, CSS. 


## Other App Notes

**Covariates** 

There are two types of covariates:

1. Time-varying: These come from `Covariates (2009-2023).csv` and are indexed by Date. They can be filtered by Month and Year and charts will show the daily average for a given period. 

2. District-varying: These come from `District Covariates (2009-2023).csv` and are indexed by Year. They can only be filtered by Year (not Month) and charts will show the average value across the selected years for each District. 

