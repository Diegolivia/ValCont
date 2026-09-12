# ValCont

R package for content validity analysis.

## Package Information

- **Title:** Content Validation Using Vaiken and Other Functions
- **Version:** 0.1.0
- **Date:** 2025-07-28
- **License:** GPL (>= 3)
- **URL:** <https://github.com/Diegolivia/ValCont>
- **Bug reports:** <https://github.com/Diegolivia/ValCont/issues>

## Authors

- **Cesar Merino-Soto** (aut, ctb, rev) - [email](mailto:sikayax@yahoo.com.ar) - [ORCID](https://orcid.org/0000-0001-8049-7069)
- **Jose Livia-Segovia** (aut, ctb) - [email](mailto:joselivia@gmail.com) - [ORCID](https://orcid.org/0000-0003-2226-3349)
- **Diego Livia-Ortiz** (aut, cre, ctb, rev) - [email](mailto:diegolivia@hotmail.com) - [ORCID](https://orcid.org/0000-0002-2107-3140)

## Description

`ValCont` is an R package for computing coefficients used in content validity
studies based on ratings from expert or experiential judges. The package
implements:

- CVC (Hernandez-Nieto, 2002)
- CVI (Martuza, 1977; Lynn, 1986)
- CVIR (Polit et al., 2007)
- CVR (Lawshe, 1975)
- Psv (proportion of substantive agreement) and Csv (coefficient of substantive validity; Anderson & Gerbing, 1991)
- V (Aiken, 1980, 1985)
- MER (mean of expert ratings; Penfield & Miller, 2004)

Because these coefficients are proportions bounded between 0 and 1, `ValCont`
also implements asymmetric confidence intervals suitable for small samples and
non-normal data (Penfield & Giacobbi, 2004; Wilson, 1927).

Additional functions support:

- Differences between two independent content validity coefficients, including confidence intervals and standardized differences (Merino-Soto, 2018, 2023).
- Ratios of two independent content validity coefficients and their confidence intervals using the MOVER-R approach (Zou, Donner, & Qiu, 2025).
- Homogeneity of ratings using the H coefficient (Aiken, 1980, 1985).
- Basic plots for visualizing results.

## Installation

You can install the development version of `ValCont` from GitHub with:

```r
if (!"devtools" %in% rownames(installed.packages())) {
  install.packages("devtools")
}

devtools::install_github("Diegolivia/ValCont")
```

## References

-   Aiken, L. R. (1980). Content validity and reliability of single items or questionnaires. Educational and Psychological Measurement, 40, 955-959. <https://doi.org/10.1177/001316448004000419>
-   Aiken, L. R. (1985). Three coefficients for analyzing the reliability and validity of ratings. Educational and Psychological Measurement, 45, 131-142. <https://doi.org/10.1177/0013164485451012>
-   Anderson, J. C., & Gerbing, D. W. (1991). Predicting the performance of measures in a confirmatory factor analysis with a pretest assessment of their substantive validities. Journal of Applied Psychology, 76(5), 732–740. <https://doi.org/10.1037/0021-9010.76.5.732>
-   Hernandez-Nieto, R. A. (2002). Contributions to Statistical Analysis. Merida, Venezuela: Universidad de Los Andes.
-   Lawshe, C. H. (1975). A quantitative approach to content validity. Personnel Psychology, 28,563-575
-   Lynn, M.R. (1986). Determination and quantification of content validity. Nursing Research, 35, 382–385.
-   Martuza, V.R. (1977). Applying norm-referenced and criterion-referenced measurement in education. Boston: Allyn & Bacon
-   Merino-Soto, C. (2023). Aiken’s V Coefficient: Differences in Content Validity Judgments. MHSalud: Revista En Ciencias Del Movimiento Humano Y Salud, 20(1), 1-10. <https://doi.org/10.15359/mhs.20-1.3>
-   Merino-Soto, C. (2018). Confidence interval for difference between coefficients of content validity (Aiken's V): a SPSS syntax. Anales de Psicología, 34(3), 587-590. <https://dx.doi.org/10.6018/analesps.34.3.283481>
-   Penfield, R. D. & Giacobbi, P. R., Jr. (2004) Applying a score confidence interval to Aiken’s item content-relevance index. Measurement in Physical Education and Exercise Science, 8(4), 213-225. <https://doi.org/10.1207/s15327841mpee0804_3>
-   Penfield, R. D., & Miller, J. M. (2004). Improving Content Validation Studies Using an Asymmetric Confidence Interval for the Mean of Expert Ratings. Applied Measurement in Education, 17(4), 359–370. <https://doi.org/10.1207/s15324818ame1704_2>
-   Polit, D.F., Beck, C.T. and Owen, S.V. (2007), Is the CVI an acceptable indicator of content validity? Appraisal and recommendations. Res. Nurs. Health, 30: 459-467. <https://doi.org/10.1002/nur.20199>
-   Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22, 209-212. <https://doi.org/10.2307/2276774>
-   Zou, G., Donner, A. and Qiu, S. (2025). MOVER-R for Confidence Intervals of Ratios. In Wiley StatsRef: Statistics Reference Online (eds N. Balakrishnan, T. Colton, B. Everitt, W. Piegorsch, F. Ruggeri and J.L. Teugels). <https://doi.org/10.1002/9781118445112.stat08085>

## Package Requirements

- **R:** >= 2.10
- **Imports:** boot, ggplot2, ggrepel, MASS, rlang, stats, utils
- **Encoding:** UTF-8
- **LazyData:** true
