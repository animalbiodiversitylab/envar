# Clear the envar download cache

Removes all files stored in the persistent download cache used when
\`cache = TRUE\` in \[par_set()\]. This is useful to free disk space or
to force a fresh download of every variable.

## Usage

``` r
clear_cache()
```

## Value

Invisibly, the path of the cache directory that was cleared.
