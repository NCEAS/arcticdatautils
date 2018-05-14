# Maintenance

TODO: Add reasons for these things!

This document should serve as a guide for new maintainers of this package.
It's a work in progress so expect it to change and, hopefully, get better and more complete over time.

## Releases

### Why release

So users can use new features. Ideally, no one is installing from source, i.e., `remotes::install_github("nceas/arcticdatautils")`.

### When to release

Whenever, really.
Since this package isn't on CRAN, you can release it as much as you like or need to.
You might want to release either when:

- You accrue enough changes to write an interest release announcement ("Hey, look at his cool new release that fixes annoying bug X!")
- You accrue at least one change and users of the package need the fix immediately

### How to release

There are a few steps in releasing a new version of the package:

1. Increment the `Version` tag in the `DESCRIPTION` file to the appropriate next version.

  What this is set to specifies what the user sees from R when they run `sessionInfo()` or `devtools::session_info()` and tell you what version they have installed.

  This package tries to use [Semantic Versioning](https://semver.org/) (semver) which can be summarized in three bullets:

  > Given a version number MAJOR.MINOR.PATCH, increment the:
  >
  > - MAJOR version when you make incompatible API changes,
  > - MINOR version when you add functionality in a backwards-compatible manner, and
  > - PATCH version when you make backwards-compatible bug fixes.

  Note: A common mistake people make is thinking that the next version after 0.9 is 1.0, but it could be 0.10, then 0.11, and so on.

  `git` and GitHub helps us a lot with determining _what_ has changed so we can determine what the next release version number should be. We can compare a previous release to `master` to get a list of all commits what were made between that release and now:

  > https://github.com/NCEAS/arcticdatautils/compare/v0.6.3...master


- Make and push a commit with just that diff,

  ```sh
  git add DESCRIPTION
  git commit -m "vx.y.z"
  git push
  ```

  [Example here](https://github.com/NCEAS/arcticdatautils/commit/87f91179f4820ecdb283672e2179984d4f6cd334).

2. Go to [the releases tab](https://github.com/NCEAS/arcticdatautils/releases) and click "Draft a new release"

  - Tag version and Release title should match v{MAJOR}.{MINOR}.{PATCH}, e.g., v6.4.5
  - The release description should include:
    - A brief, 1-2 sentence description of what's changed since the last release
    - Sections for ADDED/FIXED/REMOVED (omit section if not applicable), each with a bulleted list of changes in human-readable prose
    
      Example: https://github.com/NCEAS/arcticdatautils/releases/tag/v0.6.2

  - Make liberal use of GitHub's Compare feature: [Example](https://github.com/NCEAS/arcticdatautils/compare/v0.6.3...master) comparing `v0.6.3` to `master`.

You're done, now go tell people to upgrade!

```r
remotes::install_github("nceas/arcticdatautils@*release")
```

Note: `@*release` specifies that the latest release should be installed.

## Pull Requests

- Code style
- Commit style

TODO
