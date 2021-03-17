
<!-- README.md is generated from README.Rmd. Please edit that file -->

# britpol

`{britpol}` is an `R` package that makes analysing British political
data quick and simple. It contains two pre-formatted datasets, plus a
host of useful functions. The first dataset, `pollbase`, is a
long-format version of Mark Pack’s dataset of historic British public
opinion polls combined with more recent data from Wikipedia. The second
dataset, `pollbasepro`, provides 24,038 daily estimates of voting
intention figures for each of Britain’s three largest parties between 26
May 1955 and 17 March 2021. [Stata and SPSS versions of the data are
also available
too](https://github.com/jackobailey/britpol/tree/master/download).

To install the latest version of `{britpol}`, run the following code in
`R`:

``` r
devtools::install_github("jackobailey/britpol")
```

## Latest Polling Estimates from PollBasePro

<center>
<img src="https://raw.githubusercontent.com/jackobailey/britpol/master/documentation/_assets/timeplot_gh.png">
</center>

As of 17 March 2021, **the Conservative Party has the largest base of
support** at around 41% (95% CI: 38% to 44%) of the electorate. **They
hold a lead over the Labour Party** of 5% (95% CI: 1% to 8%). **This
puts the Labour Party in second place** on 37% (95% CI: 34% to 40%) of
the vote. **The Liberal Democrats are in third place** with the support
of 9% (95% CI: 7% to 10%) of voters.

## Notes, Usage, and Attribution

{britpol}, `pollbase`, and `pollbasepro` will change over time as
elections come and go. Users should use only the most recent version of
the package when conducting their analyses. Like any project, some minor
mistakes might have crept into the code. If you think that you have
found an error or would like to make a recommendation for a future
update, please [raise an
issue](https://github.com/jackobailey/britpol/issues).

You may also use the `{britpol}` codebase for your own purposes in line
with our license. But you must do so *with attribution*. That is, you
may reproduce, reuse, and adapt the code as you see fit, but must state
in each case that you used `{britpol}` to produce your work. The
relevant citations are as follows:

### britpol

-   **Documentation:** Bailey, J. (2021) britpol v0.1.0: User Guide and
    Data Codebook. Retrieved from
    <https://doi.org/10.17605/OSF.IO/2M9GB>.

### PollBasePro

-   **Data:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021 v.0.1.0 \[computer file\], March 2021. Retrieved from
    <https://doi.org/10.7910/DVN/3POIQW>.

-   **Paper:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021. Retrieved from doi.
