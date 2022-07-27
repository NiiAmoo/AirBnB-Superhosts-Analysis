# Eurowings Digital Assessment

The data for this projeect is AirBnB data provided by Eurowings

## Requirements
- Overall Goal: 
Analyse the provided dataset and provide a dashboard for use within the management team.

- Sub Tasks:
Are there any differences between a normal host and a superhost in the provided dataset other than those due to the superhost classfication?

Is the superhost classification working correctly?

##  Output
- A PowerBI dashboard
- An Rmarkdown document (Superhost-Analysis-Report.html) in Superhost Analysis R Project


Tech stack:
- R 
- PowerBI

## Run Scipt on your local machine
--- 
- Ensure you have the [renv](https://rstudio.github.io/renv/articles/renv.html) package installed
- Open the main(project) directory and run `renv::restore()`
- Run the script `Superhost_analysis.R`

## App folder structure
---  

### **root directory**
There's a single folder (Superhost Analysis R Project) and multiple csv files, these are:
`calendar.csv`,`listing.csv`,`listing_details.csv`,`neighbourhood.csv`,`reviews.csv`,`review_details.csv`, `listing_out`.

There's also the Eurowings_Superhosts_Dashboard, a PowerBI Desktop folder, two image files used in the PowerBI file and a zip file containing the original data from Eurowings Digital.

### **Superhost Analysis R Project**
The project(root) directory for the analysis in R. Contains data,images,renv folders and the script `Superhost_analysis.R` as well as the report `Superhost Analysis Report.html`

### **data**
Contains the data file used for the analysis as well as saved outputs from the analysis.

### **images**
Contains saved plots and screenshots of the dashboard embedded in the report.

### **renv**
Here you can find the `renv` package configuration folder. This package manager is necessary to install all dependencies used in this analysis.
