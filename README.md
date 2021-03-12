
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PollBasePro

PollBasePro is a data package for the `R` programming language. It
includes two datasets: `pollbase` and `pollbasepro`. The first provides
a long-format and ready-to-analyse version of Mark Pack’s dataset of
historic British public opinion polls combined with more recent polling
data from Wikipedia. The second provides 24,033 daily estimates of
voting intention figures for each of Britain’s three largest parties
between 26 May 1955 and 12 March 2021. The user guide and codebook is
available
[here](https://github.com/jackobailey/PollBasePro/blob/master/download/userguide.pdf)
and the accompanying paper is available
[here](https://github.com/jackobailey/PollBasePro/blob/master/download/paper.pdf).

To install the latest version of PollBasePro, run the following code in
`R`:

``` r
devtools::install_github("jackobailey/PollBasePro")
```

We also provide `pollbase` and `pollbasepro` as .dta and .sav files for
Stata and SPSS users. These files include all necessary value and
variable labels and should work seamlessly with both software packages.
[Click here to download
them](https://github.com/jackobailey/PollBasePro/tree/master/download).

## Latest Polling Estimates

<center>
<img src="https://raw.githubusercontent.com/jackobailey/PollBasePro/master/documentation/_assets/timeplot_gh.png?token=AI2VF2PMPYJQ277S7LBU4ILAJOPVI">
</center>

As of 12 March 2021, we estimate that **the Conservative Party has the
largest base of support** with 41% (95% CI: 37% to 44%) of the
electorate intending to vote for them at the next election. **They hold
a lead over the Labour Party** of 3% (95% CI: -3% to 8%). **This puts
the Labour Party in second place** on 38% (95% CI: 35% to 41%) of the
vote. **In third place is the Liberal Democrats**, with support from 9%
(95% CI: 8% to 11%) of voters.

## Notes, Usage, and Attribution

Both `pollbase` and `pollbasepro` are living datasets. As such, users
should try to use only the most recent version of the data when
conducting their analyses. Like any project of this size, we cannot rule
out the possibility that minor mistakes might have crept into our data
processing pipeline. If you think that you have found an error in our
code or wish to make a recommendation for a future update, please [raise
an issue](https://github.com/jackobailey/PollBasePro/issues) or [contact
us directly](mailto:jack.bailey@manchester.ac.uk).

We are happy for others to use our code for their own purposes, but ask
that they do so *with attribution*. That is, you may reproduce, reuse,
and change the code here as you see fit, but must state in each case
that you based your analysis on our work. The citations for each item in
PollBasePro are as follows:

-   **Data:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021 v.0.1.0 \[computer file\], March 2021. Retrieved from
    doi.

-   **Documentation:** Bailey, J., M. Pack, and L. Mansillo (2021)
    PollBasePro v0.1.0: User Guide and Codebook. Retrieved from doi.

-   **Paper:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021. Retrieved from doi.
