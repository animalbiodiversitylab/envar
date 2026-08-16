# Supplementary Technical Note: Software Quality, Testing and Failure Behaviour of the `envar` R Package

*Accompanies:* Simoncini et al., "'envar': an R package to retrieve and process environmental variables for macroecology and biogeography"

*Package version described:* `envar` 0.1.1 (CRAN)
*Source repository:* <https://github.com/animalbiodiversitylab/envar>
*Documentation:* <https://animalbiodiversitylab.github.io/envar/>
*Archived release:* <https://doi.org/10.5281/zenodo.21915323>

---

## S1. Scope and summary

This note documents the quality-assurance procedures applied to `envar`, the
extent of its automated testing, and — in most detail — its specified behaviour
when the remote services it depends on fail. Because `envar` is a client for
more than twenty independently operated open-data web services, the majority of
its failure modes originate outside the package. Its design therefore
prioritises *defensive handling of external failure* over internal algorithmic
complexity, and this note reports on both.

The package comprises 14,301 lines of R across 43 source files, exposing 28
exported user-facing functions (plus a re-exported `%>%` pipe) documented on 33
help pages.

We report the current state of quality assurance accurately, including where it
is incomplete. A summary is given in Section S7.

---

## S2. Automated unit tests and code coverage

**Version 0.1.1 does not include an automated unit-test suite.** The package
contains no `tests/` directory, and consequently:

- **Functions covered by automated unit tests: 0 of 28 exported functions.**
- **Code coverage has not been formally evaluated.** No coverage instrumentation
  (e.g. `covr`) has been run, and no coverage figure is reported.

We state this explicitly rather than describing partial or informal testing as
though it were a test suite.

Verification of `envar` to date has instead rested on four mechanisms, described
in Sections S3–S4: multi-platform `R CMD check`, execution of the package's
documented examples by the CRAN check farm, execution of every vignette against
bundled offline data, and manual exercise of each remote source during
development.

The principal limitation of this approach is that it validates *whole
workflows* rather than *individual functions*. It detects failures that occur
along the documented paths — and it has done so in practice (Section S4) — but
it provides no regression protection for the internal argument-validation,
coordinate-transformation and raster-alignment helpers, which are the components
most likely to change silently between releases. Section S6 sets out how we
intend to address this.

---

## S3. Continuous integration and multi-platform checking

### S3.1 Continuous integration

`envar` uses GitHub Actions. The repository currently defines **one** workflow,
`.github/workflows/pkgdown.yaml`, which runs on every push and pull request to
`main`, on every published release, and on manual dispatch. It installs the
package and all declared dependencies from source on `ubuntu-latest` and builds
the complete documentation website.

This workflow therefore provides continuous verification that the package
**installs cleanly from a bare checkout with its dependency tree resolved from
scratch**, and that every help page and vignette renders. It is run with
`examples = FALSE`, because executing the documented examples requires
downloading multi-gigabyte rasters from third-party hosts (a full run with
examples enabled takes approximately 2.5 hours and places substantial load on
those services).

**No `R CMD check` workflow is currently configured in continuous
integration.** Checking is performed at release time against the environments
listed below rather than on every commit.

### S3.2 Release-time check environments

Version 0.1.1 was checked with `R CMD check --as-cran` on:

| Platform | R version | Result |
|---|---|---|
| Ubuntu 22.04 (local) | 4.4.1 | 0 ERRORs, 0 WARNINGs, 3 NOTEs |
| Windows Server 2022 (win-builder) | 4.6.1 (R-release) | 0 ERRORs, 0 WARNINGs, 2 NOTEs |
| macOS (mac.R-project.org builder) | R-devel | **OK** (no NOTEs) |

In addition, as a CRAN-hosted package `envar` is checked continuously by the
CRAN check farm across its full flavour matrix (r-devel, r-release and r-oldrel
on Linux, Windows and macOS, including the `noLD`, `clang-ASAN` and `valgrind`
flavours), with results published at
<https://cran.r-project.org/web/checks/check_results_envar.html>.

The three NOTEs on the local check concerned installed size (8.0 Mb, from
`xz`-compressed bundled datasets and pre-computed vignette figures), a GBIF URL
that returns HTTP 403 to automated checkers but resolves normally in a browser,
and the interval since the previous submission. None indicates a defect.

---

## S4. Execution of example workflows on a clean installation

Of 33 help pages, 30 carry executable examples. **28 of these wrap the
network-dependent portion of the example in `\donttest{}`**, because they
require live network access and large downloads from third-party hosts;
executing them during a routine `R CMD check` would be both unreliable and
discourteous to the data providers.

This does **not** mean those examples go unexecuted. CRAN runs a dedicated
`donttest` check flavour, so every one of the 28 examples is executed
periodically on CRAN infrastructure against a clean installation and live
servers. This mechanism has demonstrated its value: the `donttest` flavour of
the 0.1.0 check identified a genuine defect in which the `chelsa()` example used
`vars = "bio"`, a value that prompts the user to choose interactively among the
19 bioclimatic variables and therefore could not complete in a non-interactive
session. Version 0.1.1 both removed the offending example and hardened the
function, which now tests `interactive()` before prompting and, in a
non-interactive session, fails immediately with a message directing the user to
name variables explicitly (`vars = c("bio1", "bio12")`).

The examples that run unconditionally under every check — including
`corr_check()` and `extr_check()` — operate entirely offline on the small raster
bundled with the package (24 × 55 cells, 4 layers).

### S4.1 Vignettes

The package ships four vignettes (`intro`, `package_overview`, `variables`,
`sdm`). All are pre-computed using the `.Rmd.orig` pattern, so that `R CMD
build` and `R CMD check` require no network access and trigger no large
downloads. Each vignette nonetheless contains at least one chunk that executes
at build time, exercising core functionality offline against bundled data.
Vignette rendering is verified on every push by the continuous-integration
workflow of Section S3.1.

---

## S5. Behaviour under expected failure conditions

Because `envar` depends on many independently maintained remote services,
failure of those services is treated as an expected operating condition rather
than an exceptional one. Failure handling is implemented throughout the
codebase: across the 43 source files there are **208 structured abort calls**
(`cli::cli_abort()`), **114 structured warnings**, and **44 `tryCatch()`
blocks**. All user-facing diagnostics are emitted through the `cli` package,
which yields consistently formatted messages that name the offending argument
and, wherever possible, state the corrective action.

The package distinguishes two response classes:

- **Abort** — the requested operation cannot be completed correctly, and
  continuing would produce a misleading result (invalid CRS, incompatible
  resolution, non-intersecting extents, invalid argument values).
- **Warn and continue** — one component of a multi-layer request failed, but the
  remainder is still valid (an individual layer or source that could not be
  downloaded). The affected layer is reported and skipped rather than aborting
  a long-running pipeline that may already have retrieved many gigabytes.

### S5.1 Failure-mode reference

| Failure mode | Detection | Behaviour |
|---|---|---|
| **Server unavailable / HTTP error** | HTTP status code checked explicitly; only `200` is accepted | Automatic retry with exponential backoff (`2^i` seconds); on exhaustion, a warning naming the file and source URL, and the layer is skipped |
| **Connection stalls or hangs** | `curl` transfer limits: 500 s connection timeout; abort if throughput stays at zero for 500 s. No limit on total transfer time, so legitimate multi-gigabyte downloads are never truncated | Attempt is abandoned and retried, then reported as a failed download |
| **Network interruption mid-download** | Downloads are written to a temporary `.part` file and moved into final position **only** after a successful `200` response | A file present at the destination is therefore always a complete download; partial files are deleted on failure. A truncated raster can never be read, and an interrupted run can be safely resumed |
| **Failed download within a multi-layer request** | Return value of the internal downloader | Warning identifying the layer; that layer is skipped and the remaining layers proceed |
| **Unsupported or malformed CRS** | `sf::st_crs()` inside `tryCatch()`, plus an `is.na()` test on the parsed object | Abort: *"`crs` <value> is not a valid coordinate reference system"*, with the hint *"Use an EPSG or ESRI code (e.g. 4326, 3035, 54009), or a PROJ4/WKT string"* |
| **Coordinates inconsistent with target CRS** | Coordinate ranges tested against ±180/±90 while the target CRS is projected | Warning that the points will be labelled without reprojection and may be mislocated, recommending `crs = 4326` if the data are WGS84. Behaviour is not silently altered |
| **Missing CRS on a user-supplied shape** | `sf::st_crs()` returns `NA` | The target CRS is assigned and the action is reported to the user |
| **Source raster coarser than requested resolution** | Native resolution compared against the requested value (20% tolerance), converted to km for both geographic and projected sources | Abort, because resampling coarse data onto a fine grid manufactures detail that does not exist. The message states the native resolution and the minimum admissible `res` |
| **Datasets with non-overlapping extents** | Extent intersection tested before layers are combined | Abort: *"No overlapping extent between layers. Cannot combine datasets with non-intersecting extents"* |
| **Mismatched geometry between stacked layers** | `terra::compareGeom(stopOnError = FALSE)` | Automatic resampling onto the existing grid, reported to the user |
| **Categorical layers resampled** | `terra::is.factor()` | Nearest-neighbour resampling and modal aggregation selected automatically, preventing the invention of intermediate class codes. Overridable via `options(envar.resample_method=)` |
| **Invalid categorical argument** | Value matched against the permitted set | Abort with fuzzy "Did you mean…?" suggestions (substring match, then approximate match via `agrep()`), plus either the full list of valid values or a count and a pointer to the reference list |
| **Malformed `pointsdf`** | Presence of `X` and `Y` columns | Abort naming both the requirement and the columns actually found |
| **Optional dependency absent** | `requireNamespace()` guards on every `rnaturalearthdata` code path | Informative message rather than an unhandled error |
| **Interactive prompt in a non-interactive session** | `interactive()` tested before prompting | Immediate, informative failure instructing the user to specify variables explicitly, rather than an indefinite hang |
| **Cache writes without consent** | `interactive()` tested before caching | In non-interactive use the answer is always "no" and a session temporary directory is used, so nothing is written outside `tempdir()` |

### S5.2 Download retry policy

The internal downloader performs up to `max_retries` attempts (default 2),
pausing `2^i` seconds between attempt *i* and the next, and re-issuing the
request with an explicit browser user-agent string. Progress is reported to the
console throughout. On final failure the partial file is removed, a failure
notice naming the file is emitted, and control returns to the calling source
function, which decides whether to skip the layer or abort.

### S5.3 Reproducibility and resumption

When persistent caching is enabled (`par_set(cache = TRUE)`), a complete file
already present at the destination is reused rather than re-downloaded, so an
interrupted multi-source pipeline resumes without repeating completed work.
Because of the `.part` mechanism described above, reuse is safe: cached files
are complete by construction. Caching is opt-in — the default (`cache = NULL`)
asks once per session interactively and always declines in non-interactive
sessions — so neither `R CMD check` nor a scripted run writes outside the
session temporary directory.

---

## S6. Known limitations and planned work

We identify the following gaps, in order of priority:

1. **No automated unit tests.** The most significant gap. We intend to add a
   `testthat` suite targeting the components that can be tested offline and
   deterministically: argument validation and the "did you mean" matcher, CRS
   parsing and rejection, resolution and extent compatibility checks, resampling
   method selection for categorical versus continuous layers, and the collinearity
   and extrapolation helpers (`corr_check()`, `extr_check()`) against the bundled
   raster. Network-dependent code paths will be tested against mocked HTTP
   responses, allowing the failure behaviour tabulated in Section S5.1 to be
   asserted directly and without contacting live servers.
2. **No coverage measurement.** Coverage will be instrumented with `covr` and
   reported once the suite of item 1 exists.
3. **No `R CMD check` in continuous integration.** A standard multi-platform
   `R-CMD-check` workflow (Linux, Windows, macOS × r-devel/release/oldrel) will
   be added alongside the existing documentation workflow.
4. **No integrity verification of retrieved files.** The `.part` mechanism
   guarantees that a download was *completed*, but the package does not verify
   file *content*: no checksum is compared, and raster files are opened without
   a guarded read. A file that is corrupt at source, or that has been damaged in
   the cache after a successful download, will therefore surface as an
   unhandled `terra` error rather than an `envar` diagnostic. Adding a guarded
   read that reports the offending file and suggests clearing the cache
   (`clear_cache()`) is a straightforward improvement.
5. **Upstream availability is outside the package's control.** `envar`
   can report a service outage clearly and retry, but cannot compensate for a
   dataset being permanently relocated or withdrawn by its provider. Users
   should treat the reported source URLs and access dates as part of the
   provenance of any downloaded product.

---

## S7. Summary

| Question | Status in `envar` 0.1.1 |
|---|---|
| Automated unit tests included? | No |
| Exported functions covered by unit tests | 0 of 28 |
| Code coverage evaluated? | No |
| Continuous integration used? | Yes, for installation from source and full documentation build on every push; **not** for `R CMD check` |
| Example workflows verified on a clean installation? | Yes — via `R CMD check --as-cran` on Linux, Windows and macOS at release, and via the CRAN `donttest` flavour, which executes all 28 network-dependent examples against live servers and detected a real defect in 0.1.0 |
| Documented behaviour under expected failures? | Yes — 208 structured aborts, 114 warnings and 44 exception handlers; retry with backoff, atomic `.part` downloads, explicit CRS/resolution/extent validation, and graceful degradation of multi-layer requests (Section S5) |

The package's principal quality-assurance strength is its systematic, tested-in-
production handling of remote-service failure; its principal weakness is the
absence of a unit-test suite and coverage measurement. Section S6 sets out our
plan to close the latter.
