# NC camping at federally-owned facilities

by Daniel Moul

December 2021

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

3. Unzip reservationsYYYY.zip files and move these historical CSV files "YYYY.csv" to ./data/raw/reservations 

4. Unzip RIDBFullExport_V1_JSON.zip, using the filename as the directory ("RIDBFullExport_V1_JSON"), which your unzipping program probably does automatically

5. Install necessary R packages. See ./scripts/my-setup.R for R packages needed.

6. Knit nc-camping-writeup.Rmd
