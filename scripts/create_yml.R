library(dplyr)
all_states <- read.csv(here::here("data", "fips.csv")) |>
  filter(STATE != "DC", 
         STATEFP < 60)

states_to_write <- all_states$STATE

# Write header

header <- "on:
  push:
    branches: carbon

jobs:
"


# Write state jobs

state_text <- ""

for(i in 1:length(states_to_write)) {
  
  this_state_text <- "  CT:
    runs-on: ubuntu-latest
    steps:
      - name: Set up R
        uses: r-lib/actions/setup-r@v2
      - name: Run apt-get
        run: |
          sudo apt-get update
      - name: Install libcurl
        run: |
          sudo apt-get install libcurl4-openssl-dev
      - name: Install redland dep
        run: |
          sudo apt-get install librdf0-dev
      - name: Install sodium dep
        run: |
          sudo apt-get install libsodium-dev
      - name: Check out repository
        uses: actions/checkout@v4
      - name: Dependencies
        uses: r-lib/actions/setup-renv@v2
      - name: Set state
        run: Rscript -e 'state_to_use <- \"CT\"'
      - name: Generate state database
        run: Rscript -e 'source(\"scripts/01-state-by-state/CT-state-parquet.R\")'
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: CT
          path: data/parquet/*
" |>
    gsub(pattern = "CT", replacement = states_to_write[i])
  
  state_text <- paste0(state_text, this_state_text)
}


# Write stack job

stack_text <- "  stack:
    runs-on: ubuntu-latest
    needs: [CT]
    env:
      ZENODO_TOKEN: ${{ secrets.ZENODO_TOKEN }}
    steps:
      - name: Set up R
        uses: r-lib/actions/setup-r@v2
      - name: Run apt-get
        run: |
          sudo apt-get update
      - name: Install libcurl
        run: |
          sudo apt-get install libcurl4-openssl-dev
      - name: Install redland dep
        run: |
          sudo apt-get install librdf0-dev
      - name: Install sodium dep
        run: |
          sudo apt-get install libsodium-dev
      - name: Check out repository
        uses: actions/checkout@v4
      - name: Dependencies
        uses: r-lib/actions/setup-renv@v2
      - name: Download artifacts
        uses: actions/download-artifact@v4
        with:
          path: data/parquet
          merge-multiple: true
      - name: Stack files
        run: Rscript -e 'source(\"scripts/02-create_db_from_parquet.R\")'
      - name: Push to Zenodo
        run: Rscript -e 'source(\"scripts/03-upload_parquet_db_zenodo.R\")'
      - name: Check db
        run: Rscript -e 'source(\"scripts/04-check_db.R\")'
      - name: Check saplings
        run: Rscript -e 'source(\"scripts/05-check_saplings.R\")'
      - name: Check annualized
        run: Rscript -e 'source(\"scripts/06-check-annualized.R\")'" |>
  gsub(pattern = "CT", replacement = paste(states_to_write, collapse = ", "))

allText <- paste0(header,
                  state_text,
                  stack_text)

writeLines(allText, here::here(".github", "workflows", "create_db.yml"))
