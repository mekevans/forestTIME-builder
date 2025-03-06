# Adding interpolated TPA_UNADJ
Eric Scott

Experimenting with how we will add a `TPA_UNADJ` column to the
annualized tree table.

``` r
library(DBI)
library(duckdb)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)
library(readr)
library(here)
```

    here() starts at /Users/ericscott/Documents/GitHub/mekevans/forestTIME-builder

Setup connection and tables

``` r
con <- dbConnect(duckdb(here("data/db/foresttime-from-state-parquet.duckdb")))
```

``` r
(tables <- dbListTables(con))
```

     [1] "all_invyrs"                      "cond"                           
     [3] "nsvb_vars"                       "plot"                           
     [5] "qa_flags"                        "ref_species"                    
     [7] "ref_tree_carbon_ratio_dead"      "ref_tree_decay_prop"            
     [9] "sapling_transitions"             "tree"                           
    [11] "tree_annualized"                 "tree_carbon"                    
    [13] "tree_carbon_annualized_midpoint" "tree_carbon_annualized_mortyr"  
    [15] "tree_cns"                        "tree_info_composite_id"         

``` r
tree <- tbl(con, "tree")
plot <- tbl(con, "plot")
tree_carbon <- tbl(con, "tree_carbon_annualized_midpoint")
```

What design codes actually have trees in them?

``` r
left_join(tree, plot |> select(PLOT_COMPOSITE_ID, DESIGNCD)) |> 
  count(DESIGNCD) |> collect() |> knitr::kable()
```

    Joining with `by = join_by(PLOT_COMPOSITE_ID)`

| DESIGNCD |        n |
|---------:|---------:|
|        1 | 48480110 |
|      111 |   138628 |
|      112 |    95035 |
|      113 |    25986 |
|      115 |   388918 |
|      116 |   165594 |
|      117 |   500054 |
|      118 |    76576 |
|      312 |   990336 |
|      314 |   881121 |
|      316 |   113864 |
|      317 |   344128 |
|      319 |   925761 |
|      321 |   163790 |
|      323 |    30792 |
|      501 |  2045287 |
|      502 |  1356005 |
|      558 |     9095 |
|      559 |     4910 |
|      601 |     3853 |
|      602 |    34558 |
|      603 |       17 |

Get TPA for design codes 558 and 559 which don’t use the same TPA rules
based on DIA, but are invariant through time.

``` r
tree_designcd_tpa <- left_join(
  tree,
  plot |> select(PLOT_COMPOSITE_ID, DESIGNCD),
  by = join_by("PLOT_COMPOSITE_ID")
) |> select(TREE_COMPOSITE_ID, DESIGNCD, TPA_UNADJ) |> 
  #just keep one row per tree, ideally with a non-NA TPA_UNADJ
  group_by(TREE_COMPOSITE_ID, DESIGNCD) |> 
  summarize(TPA_UNADJ = max(TPA_UNADJ, na.rm = TRUE), .groups = "drop")

TPA_558_559 <- tree_designcd_tpa |> 
  filter(DESIGNCD %in% c(558, 559)) |> 
  select(TREE_COMPOSITE_ID, TPA_UNADJ)
```

Fill in TPA_UNDJ for design codes 558 and 559

``` r
tree_carbon_tpa1 <- left_join(tree_carbon, TPA_558_559)
```

    Joining with `by = join_by(TREE_COMPOSITE_ID)`

``` r
#checking that it worked
tree_carbon_tpa1 |> 
  filter(TREE_COMPOSITE_ID == "53_5_29_11_2_4871760") |> 
  select(TPA_UNADJ,)
```

    # Source:   SQL [?? x 1]
    # Database: DuckDB v1.1.3 [root@Darwin 24.3.0:R 4.4.1//Users/ericscott/Documents/GitHub/mekevans/forestTIME-builder/data/db/foresttime-from-state-parquet.duckdb]
      TPA_UNADJ
          <dbl>
    1      32.9

Try out rules translated from Green Book by Dani in DESIGNCD_TPA.csv.
This uses a [rolling
join](https://www.tidyverse.org/blog/2023/01/dplyr-1-1-0-joins/#rolling-joins).

``` r
rules <- read_csv(here("data/DESIGNCD_TPA.csv")) |> 
  select(-...1)
```

    New names:
    Rows: 62 Columns: 5
    ── Column specification
    ──────────────────────────────────────────────────────── Delimiter: "," dbl
    (5): ...1, DESIGNCD, min_DIA, max_DIA, TPA_2
    ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    • `` -> `...1`

``` r
#the rules table needs to be added to the database in order for joins to work. This can just be temporary though
copy_to(con, rules, "rules", temporary = TRUE, overwrite = TRUE)

tree_designcd <- tree_designcd_tpa |> select(-TPA_UNADJ)

#add the design code column
tree_carbon_tpa <- left_join(
  tree_carbon_tpa1,
  tree_designcd,
  by = join_by(TREE_COMPOSITE_ID)
) |> 
  #rolling join to add the rules
  left_join(tbl(con, "rules"), by = join_by(DESIGNCD, between(DIA, min_DIA, max_DIA))) |> 
  #fill in the TPA_UNADJ column if there isn't anything there
  mutate(TPA_UNADJ = if_else(is.na(TPA_UNADJ), TPA_2, TPA_UNADJ)) |> 
  select(-TPA_2)
  
#check that it worked (this took a lot of RAM with the arrange())
tree_carbon_tpa |> 
  select(TREE_COMPOSITE_ID, YEAR, TPA_UNADJ, everything()) #|> 
```

    # Source:   SQL [?? x 145]
    # Database: DuckDB v1.1.3 [root@Darwin 24.3.0:R 4.4.1//Users/ericscott/Documents/GitHub/mekevans/forestTIME-builder/data/db/foresttime-from-state-parquet.duckdb]
       TREE_COMPOSITE_ID  YEAR TPA_UNADJ      ID PROVINCE SFTWD_HRDWD STATECD TRE_CN
       <chr>             <int>     <dbl>   <int> <chr>    <chr>         <int> <chr> 
     1 1_3_5_5_1_8        2014      75.0 1310724 232      H                 1 25719…
     2 1_3_109_13_1_17    2007      75.0 1310726 232      H                 1 20925…
     3 1_3_109_13_1_19    2007      75.0 1310727 232      H                 1 20925…
     4 1_3_109_13_1_20    2007      75.0 1310728 232      H                 1 20925…
     5 1_3_109_13_1_21    2007      75.0 1310729 232      H                 1 20925…
     6 1_3_109_13_1_22    2007      75.0 1310730 232      H                 1 20925…
     7 1_3_109_13_3_12    2007      75.0 1310731 232      H                 1 20925…
     8 1_3_5_5_3_4        2014      75.0 1310734 232      H                 1 25719…
     9 1_3_5_28_1_11      2014      75.0 1310742 232      H                 1 25742…
    10 1_3_101_1_3_12     2007      75.0 1310748 232      H                 1 20925…
    # ℹ more rows
    # ℹ 137 more variables: midpoint_dead_year <dbl>, HT <dbl>, DIA <dbl>,
    #   ACTUALHT <dbl>, PLT_CN <chr>, STATUSCD <dbl>, SPCD <dbl>, TREECLCD <int>,
    #   CULL <dbl>, VOLCFGRS <dbl>, DRYBIO_AG <dbl>, CARBON_AG <dbl>,
    #   STANDING_DEAD_CD <dbl>, DECAYCD <dbl>, CR <dbl>, ACTUAL_HT <dbl>,
    #   ECOSUBCD <chr>, STDORGCD <int>, JENKINS_SPGRPCD <dbl>, WDSG <dbl>,
    #   CULL_DECAY_RATIO <dbl>, DECAY_WD <dbl>, DECAY_BK <dbl>, DECAY_BR <dbl>, …

``` r
  # arrange(TREE_COMPOSITE_ID, YEAR)
```

It would probably be good to test this with a smaller dataset with known
outcome.
