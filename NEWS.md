# forestTIME-builder (development version)

- Fixed a bug in `expand_data()` that was caused `STANDING_DEAD_CD` and `DECAYCD` to not be interpolated correctly, resulting in extrapolated measurments for fallen dead trees (#101 reported by @dnsteinberg)
- `prep_data()` now converts `PLT_CN` from numeric to character for better readability in the output.
- Empty plots are no longer dropped silently by `prep_data()` and should be handled correctly by the rest of the workflow through `interpolate_data()`.
- `expand_data()` now adds a column, `interpolated`, that marks whether an observation was interpolated (`TRUE`) or in the original data (`FALSE`).
- Trees that have always been fallen and have no measurements are now removed by `prep_data()`
- Trees that change species (more than one `SPCD` value) are assumed to have always been their last recorded species.  `prep_data()` now overwrites `SPCD` with the last recorded `SPCD` for each tree.
- `prep_data()` now removes intensfication plots (`INTENSITY != 1`)
- Additional columns `PLT_CN`, `COND_STATUS_CD` are kept for the interpolated data.
- Added a vignette (WIP) on how to use outputs of `forestTIME.builder` to get population level estimates.
- `forestTIME.builder` is now an R package
- Added functions `add_composite_ids()` and `split_composite_ids()` to deal with the composite ID columns `tree_ID` and `plot_ID`.  This should make it easier to join to other FIA tables.

# forestTIME-builder v1.0.0

- Refactored to not use databases and instead to produce a single table of interpolated data
- Added new "main" functions `get_fia_tables()`, `read_fia()`, `prep_data()`, `expand_data()`, `interpolate_data()`, `adjust_mortality()`, `prep_carbon()`, and `estimate_carbon()`
- Outlined the process of creating annualized data in `docs/pop_scaling.qmd`
- Added "null and length 0 coalescing operator" `%|||%` to `R/utils.R` which is used to suppress warnings that come from adjusting for mortality when a tree hasn't yet died.
- fixed bug where TPA_UNADJ wasn't getting joined for trees with DIA between 4.9 and 5 (#68)
- fixed a bug where categorical vars weren't interpolated correctly when switching from `NA`s (#72)
- trees that have had RECONCILECD 7 ("Cruiser error") or 8 ("Procedural change") at any inventory are now removed in `prep_data()` (#59)
- interpolates trees with STATUSCD 0 and RECONCILECD 5, 6 or 9 at t2 to midpoint between t1 and t2 and then removes them from sample (#59)

# forestTIME-builder v0.1.0

- Initial release with "pre-carbon" code to annualize the tree table, but not estimate carbon or biomass
