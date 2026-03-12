# Calculate the hash of a raw vector or string

This performs a hash of the raw bytes - not of the serialized
representation.

## Usage

``` r
xxhash_raw(vec, algo = "xxh128", as_raw = FALSE)
```

## Arguments

- vec:

  raw vector or single character string

- algo:

  Select the specific xxhash algorithm. Default: 'xxh128'. (the latest
  algorithm in the xxhash family) Valid values: 'xxh32', 'xxh64',
  'xxh128', 'xxh3'

- as_raw:

  Return the hash as a raw vector of bytes instead of string? Default:
  FALSE. If TRUE, then the raw bytes are returned in big-endian order -
  which is what `xxHash` considers the *canonical* form.

## Value

String representation of hash. If `as_raw = TRUE` then a raw vector is
returned instead.
