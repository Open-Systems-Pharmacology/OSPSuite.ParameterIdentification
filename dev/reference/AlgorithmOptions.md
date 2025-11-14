# Algorithm Options for Optimization Algorithms

Default options for various optimization algorithms used within the
package. These algorithms are configured via the `PIConfiguration`
class.

## Usage

``` r
AlgorithmOptions_HJKB

AlgorithmOptions_BOBYQA

AlgorithmOptions_DEoptim
```

## Format

An object of class `list` of length 5.

An object of class `list` of length 6.

An object of class `list` of length 21.

## AlgorithmOptions_HJKB

Default options for the `HJKB` algorithm. See
[`dfoptim::hjkb()`](https://rdrr.io/pkg/dfoptim/man/hookejeeves.html)
for more details on the options.

## AlgorithmOptions_BOBYQA

Default options for the `BOBYQA` algorithm. See
[`nloptr::nl.opts()`](https://astamm.github.io/nloptr/reference/nl.opts.html)
for more details on the options.

## AlgorithmOptions_DEoptim

Default options for the `DEoptim` algorithm. See
[`DEoptim::DEoptim.control()`](https://rdrr.io/pkg/DEoptim/man/DEoptim.control.html)
for more details on the options.
