# arcticadatautils

The `articadatautils` R package contains code for:

- Inserting large numbers of files into Metacat
- High-level [rdataone](https://github.com/DataONEorg/rdataone) wrappers for
editing objects and Data Packages

Note: The package is intended to be used by NCEAS staff and may not make much sense to others.


## Installing

I recommend installing from the latest [release](https://github.com/NCEAS/arcticdatautils/releases) (aka tag) instead of from `master`. Install from release with:

```
devtools::install_github("NCEAS/arcticdatautils", ref = "{TAG_NAME_HERE}")
```

If you're feeling adventurous, you can install from the bleeding edge:

```
devtools::install_github("NCEAS/arcticdatautils")
```


## Contributing

Please submit suggestions or bugs as [Issues](https://github.com/NCEAS/arcticdatautils/issues).


## Testing

Note: The test suite contains a set of tests that call out to a remote server and whether or not these tests are run depends on whether `is_token_set()` returns true which just checks whether the `dataone_test_token` option is set.

If you don't want to run integration tests:

```r
devtools::test() 
```

If you *do* want to run integration tests

1. Visit https://test.arcticdata.io 
2. Log in
3. Navigate to My Profile > Settings > Authentication Token
4. Click the "Token for DataONE R" tab
5. Copy the code snippet
6. Modify the first line in the snippet below:

```r
options(dataone_test_token = "{YOUR_TOKEN_HERE}") # <- Modify this line
devtools::test()
```
