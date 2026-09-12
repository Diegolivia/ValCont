# ValCont News

## ValCont 0.2.0

### Added

- `MDScontent()` for multidimensional scaling maps of item-trait correspondence.
- `LuAgree()` for estimating Lu's agreement coefficient.
- `ColquittHT()` for Colquitt's content validity approach.
- `Haiken()` for Aiken's coefficient of homogeneity with bootstrap confidence intervals.
- `HTmult()` for Hinkin-Tracey indices.
- `minimumCV()` for Wilson-based minimum sample-size or critical-value calculations.
- Additional public and supporting functions for content validity analyses, including `CVIpub()` and `Vaikenpub()`.
- GitHub Actions workflows for cross-platform package validation and source-package release archives.

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

## ValCont 0.1.0

Initial public version of `ValCont`, including:

- Content validity coefficients such as CVC, CVI, CVIR, CVR, MER, and Aiken's V.
- Asymmetric confidence intervals for bounded content validity coefficients.
- Functions for comparing independent content validity coefficients.
- MOVER-R confidence intervals for ratios of independent coefficients.
- Functions for evaluating rating homogeneity.
- Basic plotting functions for validity-coefficient results.
