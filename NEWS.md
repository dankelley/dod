# dod 0.1.7

* `dod.ctd.itp()` added.

# dod 0.1.6

* `dod.tideGauge()` can now handle both CHS and NOAA downloads.
* All links to the oce package are removed, to avoid circular dependencies.

# dod 0.1.5

* `dod.tideGauge()` added.

# dod 0.1.4

* move project to github.com/dankelley/dod
* `dod.amsr()` improvements.

# dod 0.1.3

* `dod.amsr()` is now local, as opposed to a call to `oce::download.amsr()`.
  It is also changed in major ways, because the data provider has altered
  both the directory structure and the data format.  For more, see
  https://github.com/dankelley/oce/issues/2124.

# dod 0.1.2

* add `age` argument to functions that download.

# dod 0.1.1

* add `dod.met.sounding()`

# dod 0.1.0

* Initial version

