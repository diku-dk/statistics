# Statistics Library for Futhark

This library provides a number of statistics functions for Futhark.

## Status

[![Build Status](https://travis-ci.org/diku-dk/statistics.svg?branch=master)](https://travis-ci.org/diku-dk/statistics)

## Installation

```
$ futhark-pkg add github.com/diku-dk/statistics
$ futhark-pkg sync
```

## Usage example

```
$ futharki
> import "lib/github.com/diku-dk/sobol/statistics"
> module s = Statistics f64
> s.mean [3.0,4.5,6.0,6.5]
5.0f64
```
