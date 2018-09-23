# Statistics Library for Futhark [![Build Status](https://travis-ci.org/diku-dk/statistics.svg?branch=master)](https://travis-ci.org/diku-dk/statistics)

This library provides a number of statistics functions for Futhark.

Part of this library is based on work by Emily Erica Hueg and Marcus
Færch Stensler.

## Installation

```
$ futhark-pkg add github.com/diku-dk/statistics
$ futhark-pkg sync
```

## Usage example

```
$ futharki
[0]> import "lib/github.com/diku-dk/statistics/statistics"
[1]> module s = mk_statistics f64
[2]> s.mean [3.0f64,4.5f64,6.0f64,6.5f64]
5.0f64
```
