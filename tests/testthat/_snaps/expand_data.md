# expand_data() works

    Code
      df_expanded
    Output
      # A tibble: 12 x 6
         tree_ID plot_ID  YEAR   DIA    HT  SPCD
         <chr>   <chr>   <dbl> <dbl> <dbl> <dbl>
       1 A       p1       2000    24    12   123
       2 A       p1       2001    NA    NA   123
       3 A       p1       2002    NA    NA   123
       4 A       p1       2003    NA    NA   123
       5 A       p1       2004    NA    NA   123
       6 A       p1       2005    26    18   123
       7 B       p1       2000    10     5   222
       8 B       p1       2001    NA    NA   222
       9 B       p1       2002    NA    NA   222
      10 B       p1       2003    NA    NA   222
      11 B       p1       2004    NA    NA   222
      12 B       p1       2005    11     7   222

