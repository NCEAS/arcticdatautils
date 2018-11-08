# arcticdatautils

[![Travis build status](https://travis-ci.org/NCEAS/arcticdatautils.svg?branch=master)](https://travis-ci.org/NCEAS/arcticdatautils)

The `arcticdatautils` package contains code for doing lots of useful stuff that's too specific for the [dataone](https://github.com/DataONEorg/rdataone) package:

- Inserting large numbers of files into a Metacat Member Node
- High-level [dataone](https://github.com/DataONEorg/rdataone) wrappers for working with Objects and Data Packages that streamline Arctic Data Center operations

Note: The package is intended to be used by NCEAS staff and may not make much sense to others.

## Installing

We recommend installing from the latest [release](https://github.com/NCEAS/arcticdatautils/releases) (aka tag) instead of from `master`. Install the latest release with the [`remotes`](https://github.com/r-lib/remotes) package:

```r
remotes::install_github("nceas/arcticdatautils@*release")
```

If you're feeling adventurous, you can install from the bleeding edge:

```r
remotes::install_github("nceas/arcticdatautils")
```
