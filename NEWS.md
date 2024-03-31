# dod 0.1.10

* Change `dod.buoy()` to handle several buoys of the `smartatlantic` program.

# dod 0.1.9

* Change `dod.met()` to default station to 43405, since on 2024-03-16 it was
  noticed that the older default no longer held data.

# dod 0.1.8

* Change `dod.tideGauge()` to accept a number for `start`.

# dod 0.1.7

* Add `dod.ctd.itp()`.

# dod 0.1.6

* Change `dod.tideGauge()` to handle both CHS and NOAA downloads.
* All links to the oce package are removed, to avoid circular dependencies.

# dod 0.1.5

* `dod.tideGauge()` added.

# dod 0.1.4

* Move project to github.com/dankelley/dod
* `dod.amsr()` improvements.

# dod 0.1.3

* `dod.amsr()` is now local, as opposed to a call to `oce::download.amsr()`.
  It is also changed in major ways, because the data provider has altered
  both the directory structure and the data format.  For more, see
  https://github.com/dankelley/oce/issues/2124.

# dod 0.1.2

* Add `age` argument to functions that download.

# dod 0.1.1

* Add `dod.met.sounding()`.

# dod 0.1.0

* Initial version

