# textablr

## LaTeX Regression Table Output from R

- This package draws inspiration from `estout` for Stata and `stargazer` for R.
- Tables produced are `tex` fragments meant to work with the `threeparttable` method described here http://www.jwe.cc/2012/03/stata-latex-tables-estout/

### Installation

To install the latest version from github:

```r
install.packages("devtools")
devtools::install_github("jamesfeigenbaum/textablr")
```

### Regression Package Support

- `lm`
- `glm`
- `lfe` (`felm` function)

### To Do

- `rdrobust`

### Resources

- J&ouml;rg Weber's Three Part Explanation of `estout` and `LaTeX` tables
    1. [Automated Table generation in Stata and integration into LaTeX ](http://www.jwe.cc/2012/03/stata-latex-tables-estout/)
    2. [LaTeX and Stata integration (2): Solving some problems](http://www.jwe.cc/2012/08/latex-and-stata-integration-solving-some-problems/)
    3. [LaTeX and Stata integration (3): Improving the Design](http://www.jwe.cc/2012/08/latex-stata-design/)
