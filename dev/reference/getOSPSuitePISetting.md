# Get the value of a global ospsuite.parameteridentification setting.

Get the value of a global ospsuite.parameteridentification setting.

## Usage

``` r
getOSPSuitePISetting(settingName)
```

## Arguments

- settingName:

  String name of the setting

## Value

Value of the setting stored in `piEnv`. If the setting does not exist,
an error is thrown.

## Examples

``` r
getOSPSuitePISetting("packageVersion")
#>      version 
#> "2.1.0.9005" 
```
