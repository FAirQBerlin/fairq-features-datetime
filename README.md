# fairq-features-datetime

This repo contains R code to create the following features and write them to a Clickhouse database:

- public holidays
- school holidays

Run `RScripts/main.R` to create the features.

Time period: '2015-01-01 00:00:00' - '2024-12-31 23:00:00'


## How to get started

- Create an .Renviron file in the project folder, see `.Renviron_template` for 
the structure
- Build the R package
- Create database as described in https://github.com/fairqBerlin/fairq-data/tree/public/inst/db (schema fairq_raw)


## Input and output

### Input

- schulferien.org

### Output

- Database, schema `fairq_features`
