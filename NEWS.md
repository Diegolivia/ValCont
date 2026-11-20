# ValCont News

## ValCont 0.1.1

### Added

- Added `RobinsonA()` for generalized Robinson agreement coefficients, bootstrap confidence intervals, and leave-one-out diagnostics for judges and items.
- Added Cohen's h, with optional confidence intervals, to `CID()` for standardized comparisons of content-validity coefficients.
- Added grouped analyses and pairwise MOVER comparisons to `LuAgree()`, together with percentile, normal, and BCa bootstrap confidence intervals.
- Added project citation metadata in `CITATION.cff` and `llms.txt`.

### Changed

- Expanded `CVC()` with global and Hernandez overall estimates, configurable Wilson or MER confidence intervals, and support for missing-value handling.
- Improved `Haiken()` bootstrap inference with percentile, basic, normal, BCa, and automatic interval selection, plus clear guidance for small panels.
- Updated `MDScontent()` aggregation and plotting behavior, documentation, and examples.
- Clarified interpretation of negative SVC values in `SVALmult()`.
- Updated `LuAgree()`, `Vaiken()`, and related function documentation and missing-value handling.
- Excluded `CITATION.cff` and `llms.txt` from source-package builds.

### Removed

- Removed the deprecated `HTmult()` implementation and its exported namespace entry and documentation.

### Fixed

- Corrected the `MDScontent()` example so CRAN checks can parse and run it.
- Improved missing-value validation and complete-case handling in CVC, CID, Haiken, LuAgree, MDS, and substantive-validity functions.

## ValCont 0.1.0

### Added

- `MDScontent()` for multidimensional scaling maps of item-trait correspondence.
- `LuAgree()` for estimating Lu's agreement coefficient.
- `ColquittHT()` for Colquitt's content validity approach.
- `Haiken()` for Aiken's coefficient of homogeneity with bootstrap confidence intervals.
- `HTmult()` for Hinkin-Tracey indices.
- `minimumCV()` for Wilson-based minimum sample-size or critical-value calculations.
- Additional public and supporting functions for content validity analyses, including `CVIpub()` and `Vaikenpub()`.
- GitHub Actions workflows for cross-platform package validation and source-package release archives.
- Content validity coefficients such as CVC, CVI, CVIR, CVR, MER, and Aiken's V.
- Asymmetric confidence intervals for bounded content validity coefficients.
- Functions for comparing independent content validity coefficients.
- MOVER-R confidence intervals for ratios of independent coefficients.
- Functions for evaluating rating homogeneity.
- Basic plotting functions for validity-coefficient results.

### Changed

- Added explicit validation for missing data across the package functions.
- Refactored confidence-interval calculations for MER, Aiken's V, CVI, CVR, and related coefficients.
- Expanded and improved content validity calculations across CVC, CIR, CVI, CVIR, CVR, and SVAL functions.
- Improved plotting support using `ggplot2` and added MDS plotting functionality.
- Expanded and regenerated package documentation.
- Updated package metadata, namespace exports, dependencies, README, and repository documentation.

### Fixed

- Corrected documentation, namespace, and package-structure issues identified during CRAN-style validation.
- Improved handling of package examples and test execution.
- Added package build exclusions and validation checks for clean source-package archives.
