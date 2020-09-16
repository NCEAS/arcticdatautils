# arcticdatautils

[![Travis build status](https://travis-ci.org/NCEAS/arcticdatautils.svg?branch=master)](https://travis-ci.org/NCEAS/arcticdatautils)

The `arcticdatautils` package contains code for doing lots of useful stuff that's too specific for the [dataone](https://github.com/DataONEorg/rdataone) package:

- Inserting large numbers of files into a Metacat Member Node
- High-level [dataone](https://github.com/DataONEorg/rdataone) wrappers for working with Objects and Data Packages that streamline Arctic Data Center operations

Note: The package is intended to be used by NCEAS staff and may not make much sense to others.

## Installing

To install the latest version of this package we recommend installing from Github:

```r

remotes::install_github("nceas/arcticdatautils")

```

## Contributing

- Please submit suggestions or bugs as [Issues](https://github.com/NCEAS/arcticdatautils/issues).
- Pull Requestss (PR) should target the `master` branch
- Before submitting a PR, please:
  - Re-document and commit any `*.Rd` file changes
    > `devtools::document()`
  - R CMD CHECK and fix any issues related to your changes
    > `devtools::check()`
  - Run the tests and make sure they all pass
    > `devtools::test()`

## Support

- Explore the pkgdown site for documentation: https://nceas.github.io/arcticdatautils/
- Please submit bugs or other comments as [Issues](https://github.com/NCEAS/arcticdatautils/issues)
- Maintainers of the package are @jeanetteclark and @jagoldstein

## Testing

Note: The test suite contains a set of tests that call out to a remote server and whether or not these tests are run depends on whether `is_token_set()` returns true which just checks whether the `dataone_test_token` option is set.

If you don't want to run integration tests:

```r
devtools::test()
```

If you *do* want to run integration tests

1. Visit [https://test.arcticdata.io](https://test.arcticdata.io)
2. Log in
3. Navigate to My Profile > Settings > Authentication Token
4. Click the "Token for DataONE R" tab
5. Copy the code snippet
6. Modify the first line in the snippet below:

```r
options(dataone_test_token = "{YOUR_TOKEN_HERE}") # <- Modify this line
devtools::test()
```

<div>
<p align="center"> <a href="https://www.nceas.ucsb.edu/"><img  src="https://www.nceas.ucsb.edu/files/logos/NCEAS/NCEAS-full%20logo-4C.jpg" width="250px" align="center" /></a> <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png"  /> </p>
</div>
