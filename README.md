[![codecov](https://codecov.io/gh/SkyeC0re/rust-cavalieri-integration/branch/main/graph/badge.svg?token=163Y6KZPIJ)](https://codecov.io/gh/SkyeC0re/rust-cavalieri-integration)

# Rust Cavalieri Integration

A Rust native library for generating Cavalieri integral visualizations in $\mathbb{R}^2$ and $\mathbb{R}^3$ as well as representations for the Riemann-Stieltjes integral.

## Modules

The library is partitioned into several functional modules, which can be accessed directly:

- `cav2d` - Cavalieri visualizations in $\mathbb{R}^2$ and Riemann-Stieltjes representations
- `cav3d` - Cavalieri visualization in $\mathbb{R}^3$
- `core`
    - `differentiable` - Forward mode Automatic Differentiation
    - `integrate` - 10-21 adaptive Gauss-Kronrod integration over intervals in $\mathbb{R}$ and triangles in $\mathbb{R}^2$
    - `triangulation` - Triangulation procedure for nested polygon sets
    - `parsing` - A parsing module for generating tree like representations of functions from text input.
- `pyo3_wrappers` - pyO3 wrapped functions for use with Python

## Installation

`cavint` is available on [crates.io](https://crates.io/) and can be included in any Cargo enabled Rust project via:

```toml
[dependencies]
cavint = "1.0.4"
```

## Creating Python Wheels:

The simplest way to generate Python Wheel archives from this project for use in Python is to make use of the [Maturin](https://github.com/PyO3/maturin) module which can be installed with pip:

```
pip install maturin
```

Wheel archives can then be built using:
```
maturin build --release
```
