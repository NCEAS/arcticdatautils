# arcticadatautils

The `articadatautils` R package contains code for:

- Inserting large numbers of files into Metacat
- High-level [rdataone](https://github.com/DataONEorg/rdataone) wrappers for
editing objects and Data Packages

Note: The package is intended to be used by NCEAS staff and may not make much sense to others.

## Abbreviated API overview:

- `publish_update`:
  - Mint a DOI for a package
  - Replace the metadata for a package, from a local file
  - Add/remove data in a package
- `publish_object`: Use before `publish_update` if you're adding new data to a package.
- `update_resource_map`: Edit the set of child packages for a package
- `create_resource_map`: Useful for creating a new package from scratch. For both project-level metadata packages or dataset-level packages.
- `get_related_pids`: Run this before `set_rights_and_access` to get all of the PIDs for a package
- `set_rights_and_access`: Use this to give a user edit rights to a package

Note: The package does way more than this but the above are the most common tasks.

## Usage scenarios

For a lot of editing tasks, we'll first want to get some variables set up. For
the following use cases, we're going to be doing *something* to a package,
which has a metadata file with the PID 'X' in it. Here's how we set that up:

```{r}
# Set up your environment first
options(dataone_test_token = "...") # Set your token here
env <- env_load("production")

# Set up some variables for later
my_eml_file <- "/path/to/the/file/on/disk/eml.xml"

metadata_pid <- "X" # PID of the metadata object in the package you're editing
pids <- get_related_pids(env$mn, metadata_pid)
data_pids <- ... # Filter the items of `pids` to just the data object PIDs
resmap_pid <- ... # Filter the items of `pids` to just the resource map's PID
```

At this point, we can do a number of things.

### Use: I want to update the metadata in a package with an edited EML file I have on my computer

```{r}
publish_update(env$mn,
               metadata_old_pid = metadata_pid,
               resmap_old_pid = resmap_pid,
               data_old_pids = data_pids,
               metadata_file_path = my_eml_file)
```

### Use: Mint a DOI for the package

```{r}
publish_update(env$mn,
               metadata_old_pid = metadata_pid,
               resmap_old_pid = resmap_pid,
               data_old_pids = data_pids,
               use_doi = TRUE)
```

### Use: Add a new CSV data file to a package

```{r}
new_object_path <- "/path/to/the/new/file.csv"
new_data_object_pid <- publish_object(filepath = new_object_path,
                                      format_id = "text/csv")

publish_update(env$mn,
               metadata_old_pid = metadata_pid,
               resmap_old_pid = resmap_pid,
               data_old_pids = c(data_pids, newnew_data_object_pid),
               use_doi = TRUE)
```

## Installing

I recommend installing from the latest [release](https://github.com/NCEAS/arcticdatautils/releases) which should be v0.4.1:

```
devtools::install_github("NCEAS/arcticdatautils", ref = "v0.4.1")
```

If you're feeling adventurous, you can install from the bleeding edge:

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
