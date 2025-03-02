# sfislands 1.1.1

# sfislands 1.1.0

## New Features

- The functions `st_manual_cut_nb` and `st_manual_join_nb` have been **deprecated** and replaced by the more intuitively named `st_force_cut_nb` and `st_force_join_nb`.

    - The new `st_force_cut_nb()` and `st_force_join_nb()` functions work similarly to their predecessors, but now include a new argument `xy_df`. This allows multiple cuts or joins to be applied in a **single function call**, improving both performance and usability.
    - The `xy_df` argument should be a data frame containing two columns, `x` and `y`, representing pairs of units to be cut or joined.

- In `st_bridges()`, the argument `geom_col_name` has been **deprecated** in favour of the clearer `row_identifier`.  
    - This is purely a naming change — the functionality remains the same.

## Bug Fixes

- Several spurious warnings and messages that appeared in version 1.0 have been resolved.

## Other Improvements

- Error messages have been reviewed and improved for better clarity and consistency.

# sfislands 1.0.0

- Initial CRAN release.

# sfislands 0.1.0

- Initial development release.

