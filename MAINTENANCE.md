# Maintenance

TODO: Add reasons for these things!

This document should serve as a guide for new maintainers of this package.
It's a work in progress so expect it to change and, hopefully, get better and more complete over time.

## Releases

### When to release?

Whenever, really.
Since this package isn't on CRAN, we can release it as much as we like and we're only bothering people we know and those people can tell us how displeased they are with how often we're releasing this package in person.

### How to release?

- Increment the `Version` tag in the `DESCRIPTION` file to the appropriate next version.

  This package tries to use [Semantic Versioning](https://semver.org/) (semver) which can be summarized in three bullets:
  
  > Given a version number MAJOR.MINOR.PATCH, increment the:
  >
  > - MAJOR version when you make incompatible API changes,
  > - MINOR version when you add functionality in a backwards-compatible manner, and
  > - PATCH version when you make backwards-compatible bug fixes.
  
  Note: A common mistake people make is thinking that the next version after 0.9 is 1.0, but it could be 0.10, then 0.11, and so on.

- Make and push a commit with just that diff.

  Example here: https://github.com/NCEAS/arcticdatautils/commit/87f91179f4820ecdb283672e2179984d4f6cd334

- Go to [the releases tab](https://github.com/NCEAS/arcticdatautils/releases) and click "Draft a new release"

  - Tag version and Release title should match v{MAJOR}.{MINOR}.{PATCH}, e.g., v6.4.5
  - The release description should include:
    - A brief, 1-2 sentence description of what's changed since the last release
    - Sections for ADDED/FIXED/REMOVED (omit section if not applicable), each with a bulleted list of changes in human-readable prose
    
      Example: https://github.com/NCEAS/arcticdatautils/releases/tag/v0.6.2

- You're done, now go tell people to upgrade!

## Pull Requests

- Code style
- Commit style

TODO
