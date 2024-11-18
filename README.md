# Harmonist Data Availability Browser (DAB)

## Description
The Harmonist Data Availability Browser is an interactive R/Shiny dashboard that allows users to view the number of patients in a consortium that have observations of a specific type (medication initiation, outcome, lab test) by time interval (e.g., Year), sex at birth (or other stratifying variable), and patient group (Region, cohort, site, etc).

## Adapting the DAB
After installing the code here, 
- edit the **consortiaDetails.R** file with your group's consortium name, x axis variable name ("timeVar", e.g., Year, Visit, etc), patient grouping variable name ("groupVar", e.g., Site, Region, Cohort)
- edit the **dabAgeGroups.json** file to reflect the patient age groups of interest for your consortium
- edit the **varChoices.json** file to describe the variables to include in the DAB
- **calculate aggregate counts** for the variables you chose to include in the DAB.
    - Save as a CSV file "alldata.csv" in the same directory as your DAB R code
    - Column headings should be Region (or cohort, etc; your patient grouping variable), Year (year of observation, or Visit number, etc), Sex (or other stratifying variable named in consortiaDetails.R), dabAge (age group at time of observation), Variable, Value (this will be "Count" if the variable is continuous; for categorical variables, the entry matches the category or code value), Patients (total number of patients matching this row's patient group, year, variable, and value entries), numcenters (number of sites within the patient group that contributed data), and percSites(optional; the percent of sites that numcenters represents)
    - Make sure that each variable has aggregate patient counts for **every combination** of Patient group (cohort or site), Year (or other time interval term), Sex, dabAge group, Variable, and Value.
     
