# Image Rotation Angle Estimation Using Cross-Variograms

## Overview
This repository contains the code used to reproduce the results of the article

**“Image rotation angle estimation using cross-variograms”**

The proposed methodology estimates the rotation angle and the center of rotation
of images using angular variograms, cross-angular variograms, and pseudo
cross-angular variograms. The approach is designed to handle both regular and
irregular spatial grids and is motivated by applications in high-resolution
images and spatial data analysis.

The repository is intended to support **reproducibility of the numerical
experiments and applications presented in the paper**.

---

## Repository Structure

The main scripts and functions are organized as follows:

- `functions.R`  
  Core functions for computing angular variograms, cross-angular variograms,
  pseudo cross-angular variograms, and for estimating the rotation angle and
  center of rotation.

- `create_simulated_images.R`  
  Script for generating simulated images under different covariance models and
  rotation settings.

- `simulated_images.R`  
  Analysis of simulated images, including estimation accuracy and performance
  measures.

- `real_images.R`  
  Application of the proposed methodology to real image data.

---

## Dependencies

All code is written in **R**. The following R packages are required:

- `fields`
- `MASS`
- `imager`
- `writexl`
- `progress`
- `tidyverse`
- `rstudioapi`

The code was developed and tested using a recent version of R (≥ 4.0).

---

## Reproducibility

The scripts are designed to be run sequentially depending on the experiment of
interest:

1. **Simulation setup**  
   Run `create_simulated_images.R` to generate simulated image data.

2. **Simulation analysis**  
   Run `simulated_images.R` to reproduce the simulation results reported in the
   paper.

3. **Real data analysis**  
   Run `real_images.R` to reproduce the application to real images.

Random seeds are set within the scripts to ensure reproducibility of the results.

---

## Detailed File Documentation

### `functions.R`
This file contains the main functions used throughout the project, including:
- computation of angular and cross-angular variograms,
- construction of pseudo cross-angular variograms,
- estimation of the rotation angle and center of rotation,
- auxiliary utilities for visualization and numerical optimization.

### `create_simulated_images.R`
This script generates simulated images under predefined covariance models,
rotation angles, and centers of rotation. The output is used as input for the
simulation study.

### `simulated_images.R`
This script applies the proposed estimation methodology to simulated images and
computes performance measures such as estimation error and variability across
replications.

### `real_images.R`
This script applies the methodology to real image data, illustrating its
practical performance and robustness.

---

## License

This project is released under the **MIT License**.  
See the `LICENSE` file for details.

---

## Code Availability

The version of the code corresponding to the results reported in the manuscript
is archived in this repository and will be tagged upon submission/acceptance of
the article.
