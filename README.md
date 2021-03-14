
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BritPol

BritPol is a data package for the `R` programming language. It includes
two datasets: `pollbase` and `pollbasepro`. The first provides a
long-format and ready-to-analyse version of Mark Pack’s dataset of
historic British public opinion polls combined with more recent polling
data from Wikipedia. The second provides 24,035 daily estimates of
voting intention figures for each of Britain’s three largest parties
between 26 May 1955 and 14 March 2021. A user guide and codebook is
available for `pollbasepro`
[here](https://github.com/jackobailey/BritPol/blob/master/download/userguide.pdf)
and an accompanying paper is available
[here](https://github.com/jackobailey/BritPol/blob/master/download/paper.pdf).

To install the latest version of BritPol, run the following code in `R`:

``` r
devtools::install_github("jackobailey/BritPol")
```

We also provide `pollbase` and `pollbasepro` as .dta and .sav files for
Stata and SPSS users. These files include all necessary value and
variable labels and should work seamlessly with both software packages.
[Click here to download
them](https://github.com/jackobailey/BritPol/tree/master/download).

## Latest Polling Estimates

<center>
<img src="https://raw.githubusercontent.com/jackobailey/BritPol/master/documentation/_assets/timeplot_gh.png">
</center>

As of 14 March 2021, we estimate that **the Conservative Party has the
largest base of support** with 42% (95% CI: 39% to 45%) of the
electorate intending to vote for them at the next election. **They hold
a lead over the Labour Party** of 5% (95% CI: 1% to 9%). **This puts the
Labour Party in second place** on 37% (95% CI: 34% to 39%) of the vote.
**In third place is the Liberal Democrats**, with support from 9% (95%
CI: 8% to 10%) of voters.

## Notes, Usage, and Attribution

Both `pollbase` and `pollbasepro` are living datasets. As such, users
should try to use only the most recent version of the data when
conducting their analyses. Like any project of this size, some minor
mistakes might have crept into the data processing pipeline. If you
think that you have found an error in the code or wish to make a
recommendation for a future update, please [raise an
issue](https://github.com/jackobailey/BritPol/issues).

Your may use any BritPol code for your own purposes, but you must do so
*with attribution*. That is, you may reproduce, reuse, and change the
code here as you see fit, but must state in each case that you used
BritPol to produce your work. The citations for each item in BritPol are
as follows:

### BritPol

-   **Documentation:** Bailey, J. (2021) BritPol v0.1.0: User Guide and
    Data Codebook. Retrieved from doi.

### PollBasePro

-   **Data:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021 v.0.1.0 \[computer file\], March 2021. Retrieved from
    <https://doi.org/10.7910/DVN/3POIQW>.

-   **Paper:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021. Retrieved from doi.
