# NC camping at federally-owned facilities

by Daniel Moul

January 2022

<br>

Read the writeup: [Camping in NC at Federal Facilities 2009-2020](https://dmoul.github.io/nc-camping/index.html)


<br>

To recreate this analysis:

1. Clone the github repo [dmoul/nc-camping](https://github.com/dmoul/nc-camping)

2. Create any of the following directories that don't exist in the root directory of your project:

```
./data
./data/raw
./data/raw/reservations
./data/processed
./figures/
./scripts
```

3. Download data from https://ridb.recreation.gov/download to ./data/raw, specifically:

    - RIDB Recreation Data: https://ridb.recreation.gov/downloads/RIDBFullExport_V1_JSON.zip which has data about the facilities and organizations offering them
    - RIDB/Recreation.gov Historical Reservation Data: https://ridb.recreation.gov/downloads/reservations2006.zip through https://ridb.recreation.gov/downloads/reservations2020.zip
    
    I downloaded this data on 2021-11-11.

4. Unzip the files you downloaded and move them under ./data/raw

    - Unzip reservationsYYYY.zip files and move these historical CSV files "YYYY.csv" to ./data/raw/reservations 

    - Unzip RIDBFullExport_V1_JSON.zip, using the filename as the directory ("RIDBFullExport_V1_JSON"), which your unzipping program probably does automatically

5. Make sure you have these files

    - ./_targets.R (pipeline functions and data)
    - ./_quarto.yml (quarto configuration)
    - ./scripts/functions.R (used by tar_make() to prepare data frames)
    - ./scripts/my-setup.R (libraries, options, constants)
    - ./*.qmd (main scripts)
    - ./README.md (this file)
    
6. Install necessary R packages. See ./scripts/my-setup.R for R packages needed.

7. Run targets::tar_make() to prepare data

8. In a terminal session in the root directory of your project, run: quarto render

9. Open nc-camping/index.html
