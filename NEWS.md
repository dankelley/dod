# dod 0.1.17

* `dod.tideGauge()` accepts `resolution` for NOAA data (issue #22)

# dod 0.1.16

* Add `dod.ctd.bbmp.index()` (issue #16).

# dod 0.1.15

* Add `dod.river.index()` and `dod.river()` (issue #16).

# dod 0.1.14

* Update documentation to show the familial relationships between all
  downloading functions.
* Change `dod.coastline()` to handle the new server location noticed in
  December 2024.
* Change `dod.tideGauge()` to obey `age` parameter.
* Prevent documentation examples from running in non-interactive mode. This
  seems to be the only way to prevent a `pkgdown::build_site()` error that
  comes up with downloaded files.  The error is actually a warning, but it
  stops the processing, so we cannot tolerate it.  The message is from
  `gzfile()` stating that it cannot open a compressed file.  I've seen errors
  like this in discussion boards going back many years, related to many things.
  I have no idea what the problem is, despite quite a lot of searching and
  testing.  Would I like it if the website showed plots, etc?  Sure.  Do I want
  to spend a day aimlessly trying things and searching the web? Not so much.

# dod 0.1.13

* Change `dod.tideGauge()` to handle present-day DFO format (issue #12).
* Change `dod.ctd.bats()` to simply report an error about the BATS website
  (issue #15).
* Change all examples to no longer download since this causes problems with
  `pkgdown::build_site()`. The problem was first noticed on 2024-12-17.

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

* Move project to <https://www.github.com/dankelley/dod> for future development.
* Improve `dod.amsr()`.

# dod 0.1.3

* Make `dod.amsr()` be local, as opposed to a call to `oce::download.amsr()`.
  It is also changed in major ways, because the data provider has altered
  both the directory structure and the data format.  For more, see
  <https://github.com/dankelley/oce/issues/2124>.

# dod 0.1.2

* Add `age` argument to functions that download.

# dod 0.1.1

* Add `dod.met.sounding()`.

# dod 0.1.0

* Initial version
