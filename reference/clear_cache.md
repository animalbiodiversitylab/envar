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

## Details

The cache is stored in the per-user cache directory returned by
\[tools::R_user_dir()\] and is only ever written after the user has
agreed to it (see the \`cache\` argument of \[par_set()\]).

## Examples

``` r
# \donttest{
# Empty the persistent download cache, if the user enabled one
clear_cache()
# }
```
