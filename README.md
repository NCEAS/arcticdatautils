# arcticadatautils

The `articadatautils` R package contains code for:

- Inserting large numbers of files into Metacat
- High-level [rdataone](https://github.com/DataONEorg/rdataone) wrappers for
editing objects and Data Packages

Note: The package is intended to be used by NCEAS staff and may not make much sense to others.

## Installing

```
devtools::install_github("NCEAS/arcticdatautils")
```

## Contributing

Please submit suggestions or bugs as [Issues](https://github.com/NCEAS/arcticdatautils/issues).

## Testing

Some tests are dependent on an authentication token being set and be skipped if one is not set.

```
# Skips tests that depend on a Metacat instance:
devtools::test()

# Set a token to run skipped tests:
options(dataone_test_token = "...")
devtools::test()
```
