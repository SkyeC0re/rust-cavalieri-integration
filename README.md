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
