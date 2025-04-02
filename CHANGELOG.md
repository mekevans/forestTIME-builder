# forestTIME-builder (development version)

- Refactored to not use databases and instead to produce a single table of interpolated data
- Added new "main" functions `get_fia_tables()`, `read_fia()`, `prep_data()`, `expand_data()`, `interpolate_data()`, `adjust_mortality()`, `prep_carbon()`, and `estimate_carbon()`
- Outlined the process of creating annualized data in `docs/pop_scaling.qmd`

# forestTIME-builder v0.1.0

- Initial release with "pre-carbon" code to annualize the tree table, but not estimate carbon or biomass
