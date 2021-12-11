# NC camping at federally-owned facilities

by Daniel Moul

December 2021



To recreate this analysis:

1. Create the following directories in the project root directory:

```
./data
./data/raw
./data/raw/reservations
./data/processed
./figures/
./scripts
./
```

2. Download data:

    - ***TODO: add here***
    - ***TODO: add here***

3. Unzip reservationsYYYY.zip files and move historical CSV files "YYYY.csv" to ./data/raw/reservations 

4. Unzip RIDBFullExport_V1_JSON.zip, using the filename as the directory ("RIDBFullExport_V1_JSON"), which your unzipping program probably does automatically

5. Install necessary R packages. See ./scripts/my-setup.R for R packages needed.

6. Knit nc-camping-writeup.Rmd
