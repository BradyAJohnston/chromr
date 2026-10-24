# Changelog

## chromr 0.3.0

### Breaking changes

- Every reader gives the same columns: `time`, `volume`, `fraction`,
  absorbance as `a280` etc. (or `uv` without a wavelength), then
  `conductivity`, `conductivity_percent`, `percent_b`, `pressure`,
  `flow` and `temperature`.

  - [`chrom_read_quadtech()`](https://bradyajohnston.github.io/chromr/reference/chrom_read_quadtech.md)
    names conductivity, pressure and the UV channel `conductivity`,
    `pressure` and `uv` instead of `m_s_cm`, `psi` and `au`, and puts
    the standard columns first. The values are unchanged.
  - [`chrom_read_ngc()`](https://bradyajohnston.github.io/chromr/reference/chrom_read_ngc.md)
    gives one row per volume with a column per trace, instead of a long,
    grouped data frame with a volume for each trace.

- Points not collected into a fraction have a `fraction` of `NA`, where
  QuadTec files record 0.

- [`.find_common_start()`](https://bradyajohnston.github.io/chromr/reference/dot-find_common_start.md),
  [`check_column_exist()`](https://bradyajohnston.github.io/chromr/reference/check_column_exist.md),
  `rename_columns()` and `interpolate_column()` are no longer exported.

### New features

- `fraction` columns are a new fraction number type, created with
  [`fraction()`](https://bradyajohnston.github.io/chromr/reference/fraction.md).
  Fractions compare, sort and do arithmetic as numbers, and adding a
  number continues the numbering.

- Absorbance columns are a new type, created with
  [`absorbance()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md),
  that records the wavelength and the unit, AU or mAU. Combining
  absorbance in different units converts it to mAU, dividing absorbance
  gives a plain ratio such as A260/A280, and plots label the axis with
  the unit.
  [`absorbance_convert()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md),
  [`absorbance_wavelength()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md)
  and
  [`absorbance_unit()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md)
  work with it.

- The other columns are a new type of numbers with a unit, created with
  [`quantity()`](https://bradyajohnston.github.io/chromr/reference/quantity.md),
  using the unit recorded in the file: `time` in s, `volume` in mL,
  `pressure` in psi or MPa, and so on. They compare and do arithmetic
  with plain numbers, so `volume > 10` works as before. Combining runs
  recorded in different units of the same kind, such as psi and MPa,
  converts them to the unit of the first, and plots label the volume
  axis with its unit.
  [`quantity_convert()`](https://bradyajohnston.github.io/chromr/reference/quantity.md)
  and
  [`quantity_unit()`](https://bradyajohnston.github.io/chromr/reference/quantity.md)
  work with them.

- The readers return a chromatogram, a tibble that records the
  instrument, run, date, method and file, and prints them.
  [`chrom_meta()`](https://bradyajohnston.github.io/chromr/reference/chrom_meta.md)
  gives them.

- [`chrom_fractions()`](https://bradyajohnston.github.io/chromr/reference/chrom_fractions.md)
  gives the start and end volume of each fraction.

- [`chrom_add_volume()`](https://bradyajohnston.github.io/chromr/reference/chrom_add_volume.md)
  uses the unit of the `time` column.

- [`fraction_to_well()`](https://bradyajohnston.github.io/chromr/reference/fraction_to_well.md)
  gives the well of the collection plate each fraction went into, as
  wellr well IDs.

### Bug fixes

- [`read_akta_old()`](https://bradyajohnston.github.io/chromr/reference/read_akta_old.md)
  aligns the traces by volume. They were matched by row, but each trace
  is recorded at its own volumes, so later in a run the values came from
  different volumes, up to 6 mL apart.

- [`read_akta_old()`](https://bradyajohnston.github.io/chromr/reference/read_akta_old.md)
  ends the last fraction at a “Waste” mark, instead of extending it and
  warning “NAs introduced by coercion”, and reads files without
  injection or logbook columns. `fraction_size` is deprecated, as it was
  never used.

- [`read_akta_old()`](https://bradyajohnston.github.io/chromr/reference/read_akta_old.md)
  reads UNICORN exports whose traces are named without the run, such as
  “UV” and “Gradient Concentration”, and fraction marks that name the
  tube, such as “T12”. It drops the run log, numbers repeated traces (a
  second UV trace is `uv_2`), warns about fraction marks it can’t read,
  and errors for UNICORN exports that aren’t curves, such as methods and
  peak tables.

- [`chrom_read_quadtech()`](https://bradyajohnston.github.io/chromr/reference/chrom_read_quadtech.md)
  reads files whose header has been removed, and errors when the column
  names don’t match the data instead of giving meaningless columns.

- [`chrom_append_run()`](https://bradyajohnston.github.io/chromr/reference/chrom_append_run.md)
  continues each run from the end of all the runs before it, in the
  units of the first run, so a third run no longer overlaps the second.
  `adjust_fractions = TRUE` only renumbers runs whose fractions start
  again, and no longer warns.

- [`chrom_plot_fractions()`](https://bradyajohnston.github.io/chromr/reference/chrom_plot_fractions.md)
  draws the fraction labels, which were never drawn, and shades the
  fractions in cycles of `frac_labelling` instead of 5.

- [`chrom_plot()`](https://bradyajohnston.github.io/chromr/reference/chrom_plot.md)
  and
  [`chrom_plot_fractions()`](https://bradyajohnston.github.io/chromr/reference/chrom_plot_fractions.md)
  label the axis with the absorbance unit instead of always “AU”.

- The fraction geoms no longer warn about points not collected into a
  fraction.

### Minor improvements

- chromr now requires R 4.1.0 and ggplot2 3.4.0, and no longer depends
  on magrittr. It suggests wellr, for
  [`fraction_to_well()`](https://bradyajohnston.github.io/chromr/reference/fraction_to_well.md).

- Errors for invalid arguments name the argument and say what’s wrong.
