# dod 0.1.13

* Change `dod.tideGauge()` to handle present-day DFO format.

# dod 0.1.12

* Change `dod.amsr()` to default to 4 days into the past, because in early
  morning, the 3-day default would fail because files were not yet ready on the
  server.  Also, change the first parameter so it can be a Date or time object,
  or a string that can be converted to the former.
* Switch from `download.file()` to `RCurl::download_file()`, because the former
  was seen to cause problems on Windows machines.
* Rename `silent` argument to `quiet`, to match the name used in
  `RCurl::download_file()`.

# dod 0.1.11

* Add `dod.met.index()`.

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
  <https://github.com/dankelley/oce/issues/2124>.

# dod 0.1.2

* Add `age` argument to functions that download.

# dod 0.1.1

* Add `dod.met.sounding()`.

# dod 0.1.0

* Initial version
