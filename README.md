# arcticadata R Package

The `articadata` R package contains code relevant to working with and inserting
datasets from ACADIS to a Metacat instance.

Files from ACADIS are held on the KNB which does not have R installed so, in
order to work with the files, we run three shell commands to produce listings
of the filenames, their sizes (in bytes), and their SHA256 checksums. In order
to work with the files you'll need to have either generated these files
yourself or obtained them another way.

Files are kept in what I'm calling the Inventory, which is just an R
`data.frame` which can be created from scratch or saved to a CSV and read in
at a later time.

The following code snippet shows the process of loading the `articadata`
package, creating the Inventory and populating it with filenames, sizes,
checksums, and a number of other useful bits of information relevant to
inserting files into a Metacat instance.

```{r}
library(articadata)

inv <- inv_init()
inv <- inv_load_files("/path/to/files.txt", inv) # Takes some time
inv <- inv_load_sizes("/path/to/sizes.txt", inv)  # Takes some time
inv <- inv_load_checksums("/path/to/checksums.txt", inv) # Takes some time
inv <- inv_add_extra_columns(inv) # Takes a lot of time
```

Once you have an Inventory object, you can run `insert_package` or
`show_random_dataset` on its contents. See the help for those functions.
