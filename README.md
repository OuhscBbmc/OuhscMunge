<!-- rmarkdown v1 -->

| [GitHub](https://github.com/OuhscBbmc/OuhscMunge) | [Travis-CI](https://travis-ci.org/OuhscBbmc/OuhscMunge/builds) | [AppVeyor](https://ci.appveyor.com/project/wibeasley/ouhscmunge/history) | [Coveralls](https://coveralls.io/r/OuhscBbmc/OuhscMunge) |
| :----- | :---------------------------: | :-----------------------------: | :-------: |
| [Master](https://github.com/OuhscBbmc/OuhscMunge/tree/master) | [![Build Status](https://travis-ci.org/OuhscBbmc/OuhscMunge.svg?branch=master)](https://travis-ci.org/OuhscBbmc/OuhscMunge) | [![Build status](https://ci.appveyor.com/api/projects/status/j74ml0yqlmve7oaw/branch/master?svg=true)](https://ci.appveyor.com/project/wibeasley/ouhscmunge/branch/master) | [![Coverage Status](https://coveralls.io/repos/OuhscBbmc/OuhscMunge/badge.svg?branch=master)](https://coveralls.io/r/OuhscBbmc/OuhscMunge?branch=master) |
| [Dev](https://github.com/OuhscBbmc/OuhscMunge/tree/dev) | [![Build Status](https://travis-ci.org/OuhscBbmc/OuhscMunge.svg?branch=dev)](https://travis-ci.org/OuhscBbmc/OuhscMunge) | [![Build status](https://ci.appveyor.com/api/projects/status/j74ml0yqlmve7oaw/branch/dev?svg=true)](https://ci.appveyor.com/project/wibeasley/ouhscmunge/branch/dev) | [![Coverage Status](https://coveralls.io/repos/OuhscBbmc/OuhscMunge/badge.svg?branch=dev)](https://coveralls.io/r/OuhscBbmc/OuhscMunge?branch=dev) | -- |
| | *Ubuntu 12.04 LTS* | *Windows Server 2012* | *Test Coverage* | *Independently-hosted Archive* |

OuhscMunge
==========
Data manipulation operations commonly used by the [Biomedical and Behavioral Methodology Core](http://www.ouhsc.edu/bbmc/) within the [Department of Pediatrics](http://www.oumedicine.com/pediatrics) of the [University of Oklahoma Health Sciences Center](http://ouhsc.edu/).

### Download and Installation Instructions
<!--
The *release* version of IalsaSynthesis can be installed from [CRAN](http://cran.r-project.org/web/packages/OuhscMunge/).
```r
install.packages("OuhscMunge")
```
-->

The *development* version of `OuhscMunge` can be installed from [GitHub](https://github.com/OuhscBbmc/OuhscMunge) after installing the `devtools` package.  (The *release* version will be available on [CRAN](http://cran.rstudio.com/) later.)
```r
install.packages("devtools")
devtools::install_github(repo="OuhscBbmc/OuhscMunge")
```

### Collaborative Development
We encourage input and collaboration from the overall community.  If you're familar with GitHub and R packages, feel free to submit a [pull request](https://github.com/OuhscBbmc/OuhscMunge/pulls).  If you'd like to report a bug or make a suggestion, please create a GitHub [issue](https://github.com/OuhscBbmc/OuhscMunge/issues); issues are a usually a good place to ask public questions too.  However, feel free to email Will (<wibeasley@hotmail.com>).  Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Thanks to Funders
*OUHSC CCAN Independent Evaluation of the State of Oklahoma Competitive Maternal, Infant, and Early Childhood Home Visiting ([MIECHV](http://mchb.hrsa.gov/programs/homevisiting/)) Project.*: Evaluates MIECHV expansion and enhancement of Evidence-based Home Visitation programs in four Oklahoma counties. [HRSA/ACF D89MC23154](https://perf-data.hrsa.gov/mchb/DGISReports/Abstract/AbstractDetails.aspx?Source=TVIS&GrantNo=D89MC23154&FY=2012).  

(So far) the primary developers of OuhscMunge are the external evaluators for [Oklahoma's MIECHV](http://www.ok.gov/health/Child_and_Family_Health/Family_Support_and_Prevention_Service/MIECHV_Program_-_Federal_Home_Visiting_Grant/MIECHV_Program_Resources/index.html) program.