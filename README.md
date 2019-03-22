# space-workshop

This workshop is still WIP.

## Progress so far

### Apollo Lunar Ascent Guidance

For an early example, take a look at [`AGC.hs`](https://github.com/lancelet/space-workshop/blob/485a047ab5f16d3f07330fa10cd58bfea87c5c9f/src/LunarAscent2/AGC.hs#L36), which is a transcription into Haskell of the Apollo Lunar Ascent guidance program, complete with type-safe units.

The figure below shows the output from the Lunar Ascent simulation code (with text annotations added manually afterwards):
![lunar-ascent](https://raw.githubusercontent.com/lancelet/space-workshop/master/lunar-ascent.png)

This shows the "quick, early takeoff" trajectory that was programmed by default into the Apollo Guidance Computer. Normally the astronaut would customize the parameters for appropriate CSM rendezvous. The equations and general reference for this guidance algorithm were obtained from:
  - [Levine et al. (1971) Apollo Guidance Navigation and Control. Guidance systems operations plan for manned LM earth orbital and lunar missions using program luminary 1E. Section 5: Guidance Equations (Rev. 11)](https://www.ibiblio.org/apollo/Documents/j2-80-R-567-SEC5-REV11_text.pdf), and
  - [Bennett, FV. (1970) Apollo Lunar Descent and Ascent Trajectories. NASA TM X-58040. AIAA 8th Aerospace Sciences Meeting, New York, January 19-21, 1970.](https://www.hq.nasa.gov/alsj/nasa58040.pdf).
  
The radial axis in the figure is scaled by a factor of 10 above the lunar surface to make the trajectory more distinguishable from the moon itself. Better visualizations are in-progress.

### Some Comparisons of ODE Integration

Early in the workshop, we have participants write integrators to solve the initial value problem of a system of ordinary differential equations (ODEs). As part of this, we compare the results from Euler integration against those from a 4th-order Runge Kutta method (RK4). The plot below shows the culmination of this work, demonstrating a comparison of RK4 vs Euler for the same number of function evaluations. The RK4 method is much closer to the known analytical solution than the Euler method:
![simple-harmonic-motion](https://raw.githubusercontent.com/lancelet/space-workshop/master/shm.png)

This plot gives an idea of the regular graphical feedback that participants will get through the individual stages of some worked problems.
