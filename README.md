# Haskell Spaceflight Workshop

[![Build Status](https://travis-ci.org/lancelet/space-workshop.svg?branch=master)](https://travis-ci.org/lancelet/space-workshop)

<img src="./logo.svg" width="150px" height="150px" alt="Logo"/>
This workshop is still WIP.

The current notes for the workshop (VERY WIP) are available from
[GitHub pages](https://lancelet.github.io/space-workshop), built using Travis CI.

## Progress so far

### Apollo Lunar Ascent Guidance

For an early example, take a look at [`AGC.hs`](https://github.com/lancelet/space-workshop/blob/485a047ab5f16d3f07330fa10cd58bfea87c5c9f/src/LunarAscent2/AGC.hs#L36), which is a transcription into Haskell of the Apollo Lunar Ascent guidance program, complete with type-safe units.

The figure below shows the output from the Lunar Ascent simulation code. The ascent and coasting trajectories are both fully computed by the existing code. The text annotations were added manually afterwards:
![lunar-ascent](https://raw.githubusercontent.com/lancelet/space-workshop/master/lunar-ascent.png)

This shows the "quick, early takeoff" trajectory that was programmed by default into the Apollo Guidance Computer. Normally the astronaut would customize the parameters for appropriate CSM rendezvous. The equations and general reference for this guidance algorithm were obtained from:
  - [Levine et al. (1971) Apollo Guidance Navigation and Control. Guidance systems operations plan for manned LM earth orbital and lunar missions using program luminary 1E. Section 5: Guidance Equations (Rev. 11)](https://www.ibiblio.org/apollo/Documents/j2-80-R-567-SEC5-REV11_text.pdf), and
  - [Bennett, FV. (1970) Apollo Lunar Descent and Ascent Trajectories. NASA TM X-58040. AIAA 8th Aerospace Sciences Meeting, New York, January 19-21, 1970.](https://www.hq.nasa.gov/alsj/nasa58040.pdf).
  
The radial axis in the figure is scaled by a factor of 10 above the lunar surface to make the trajectory more distinguishable from the moon itself. Better visualizations are in-progress.

### Some Comparisons of ODE Integration

Early in the workshop, we have participants write integrators to solve the initial value problem of a system of ordinary differential equations (ODEs). As part of this, we compare the results from Euler integration against those from a 4th-order Runge Kutta method (RK4). The plot below shows the culmination of this work, demonstrating a comparison of RK4 vs Euler for the same number of function evaluations. The RK4 method is much closer to the known analytical solution than the Euler method:

![simple-harmonic-motion](https://raw.githubusercontent.com/lancelet/space-workshop/master/shm.png)

This plot gives an idea of the regular graphical feedback that participants will get through the individual stages of some worked problems.

### Drag Coefficient Curves

In simulations of atmospheric flight from late in the workshop, we plan to include realistic simulation of drag and the variation of rocket nozzle efficiency with ambient pressure. Simulating drag in particular has posed numerous problems, as it's infeasible to address realistic CFD modeling within the workshop, and little reliable, referreed, or accessible data is available for historical or contemporary rocket designs. Consequently, we have evaluated several different phenomenological models. In the end, the figure below shows a plot of what we have settled on for the workshop:

![drag-curves](https://raw.githubusercontent.com/lancelet/space-workshop/master/drag-coeff.png)

These are plots of the "standardised" drag coefficients from:
  - [Kephart, DC (1971) Boost: On-Line Computer Program for Estimating Powered-Rocket Performance. A Report prepared for United States Air Force Project Rand. R-670-PR.](https://www.rand.org/pubs/reports/R0670.html)
  
These drag coefficient models provide a reasonable match against the other phenomenological drag models we evaluated (MC-DRAG and Digital DATCOM), while being simpler to evaluate.
