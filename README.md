# NC camping at federally-owned facilities

by Daniel Moul

January 2022

<br>

Read the writeup: [Camping in NC at Federal Facilities 2009-2020](https://dmoul.github.io/nc-camping/nc-camping-writeup.html)


<br>

To recreate this analysis:

1. Create an RStudio project and the following directories in the project root directory:

```
./data
./data/raw
./data/raw/reservations
./data/processed
./figures/
./scripts
```

2. Download data from https://ridb.recreation.gov/download to ./data/raw, specifically:

    - RIDB Recreation Data: https://ridb.recreation.gov/downloads/RIDBFullExport_V1_JSON.zip which has data about the facilities and organizations offering them
    - RIDB/Recreation.gov Historical Reservation Data: https://ridb.recreation.gov/downloads/reservations2006.zip through https://ridb.recreation.gov/downloads/reservations2020.zip
    
    I downloaded this data on 2021-11-11.

3. Unzip the files you downloaded

    - Unzip reservationsYYYY.zip files and move these historical CSV files "YYYY.csv" to ./data/raw/reservations 

    - Unzip RIDBFullExport_V1_JSON.zip, using the filename as the directory ("RIDBFullExport_V1_JSON"), which your unzipping program probably does automatically

4. Download script files from this GitHub project ( https://github.com/dmoul/nc-camping )

    - ./_targets.R (pipeline functions and data)
    - ./scripts/functions.R (used by targets to prepare data frames)
    - ./scripts/my-setup.R (libraries, options, constants)
    - ./nc-camping.Rmd (main script)
    - ./README.md (this file)
    
5. Install necessary R packages. See ./scripts/my-setup.R for R packages needed.

6. Run targets::tar_make() to prepare data frames

7. Knit nc-camping-writeup.Rmd

8. See nc-camping-writeup.html
