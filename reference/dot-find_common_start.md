# Find common start of multiple strings

Walks along all strings until there is a character that is not shared
between all of them, then returns the string up until there.

## Usage

``` r
.find_common_start(strings)
```

## Arguments

- strings:

  Vector of strings to be processed

## Value

Single character that is common to the start of all the strings. Returns
empty string if nothing in common.
