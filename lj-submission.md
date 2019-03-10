# Spaceflight Workshop

## Abstract

In the 50th Anniversary year of the Apollo 11 moon landing, this
workshop is a spaceflight-themed exploration of basic numerical
methods in Haskell. Attendees will solve a set of problems that
involve simulation of several different spaceflight manoevres,
learning a bit about Haskell libraries along the way.

On the Haskell side, you'll explore basic numerical algorithms for the
solution of ordinary differential equations (ODEs), optimisation, and
the simulation of dynamical systems. You'll see how Haskell ecosystem
libraries such as `vector-spaces`, `vector` and `dimensional` can help
to provide a type-safe scaffolding for numerical work. The focus isn't
on optimality of the algorithms, which would require much longer than
90 minutes, but rather on providing an understandable, accurate
engineering solution to the problems posed in the available time.

On the spaceflight side, you'll see your code used as the core of a
simulation of lunar ascent, using a guidance algorithm based on the
P12 program of the Apollo Guidance Computer. You'll investigate the
reasons that rockets have stages. You'll see how elliptical transfer
orbits work and how they can be used to position spacecraft for
rendezvous. Finally, you'll look at the challenges unique to 
atmospheric rocket flight, including the effects of aerodynamic drag
and the variation of rocket engine efficiency with ambient pressure.

While it is intended to be relatively challenging, the workshop also
takes a "fun for everyone" approach, so that even those without a
prior interest in numerical algorithms or spaceflight will be able to
participate in an immersive way. Most of the setup of the various
scenarios are pre-baked and referenced against the published
literature. All of the problems posed have fallback solutions, so that
it's possible for attendees to skip any parts that are too far outside
their comfort zone, while still seeing the approach used and the
outcomes.

## Theme / Topic / Category

Case Study

## Level

Intermediate

## Session Type

Workshop

## Duration

90 mins

## Co-Presenters

Luke Clifton?

## Target Audience

Everyone! Even if you're not that interested in numerical methods,
come along to gain an insight into spaceflight mechanics.

## Session Prerequisite

A MacOS or Linux machine (not tested on Windows - recommend to use a
Linux VM hosted by VirtualBox in this case). Ideally download the
workshop git repository in advance and make sure you have all the
dependencies installed.

## Outline / Structure

Participants will be presented with a series of problem in a Haskell
project. Notes for the workshop will be provided in three forms:
  1. A higher-level roadmap.
  2. Inline instructions in the code.
  3. A "theory manual" containing derivations that are too involved or
     time-consuming to cover during the workshop itself, but can form
	 pre- or post-reading.
 
Most of the scenarios covered in the workshop are "pre-baked", based
either on broad descriptions in the published academic spaceflight
literature or on specific research papers. Miscellaneous functionality 
such as plotting results is already set up.

Chronological list of topics / scenarios (this is an outline, NOT the 
workshop instructions):
  - Integration of ODEs:
    - Euler's method specialized to Double,
    - Plot Euler's method vs analytic for radioactive decay,
	- Intro to `vector-spaces` and `dimensional` using simple harmonic 
	  motion model,
    - Generalize Euler's method to an affine space, using `vector-spaces`,
    - Plot Euler's method vs analytic for SHM,
    - Implement RK4 integration.
	- (Easter egg: double pendulum.)
  - Using the participant's RK4, simulate lunar ascent trajectory.
  - Implement triggers in ODE driver using bisection.
  - Investigate rocket staging.
  - Orbital mechanics:
    - Simulate Hohmann transfer with short, large magnitude burns.
	- Simulate double Hohmann transfer for orbital phase adjustment
	  (rendezvous with another spacecraft).
  - Implement gradient descent optimizer.
  - Simulate atmospheric orbital insertion by optimizing initial pitch 
	angle for a simple gravity turn.
  - Implement particle swarm optimizer.
  - Simulate orbital insertion with a piecewise-linear guidance law and
    objective function that deals with multiple parameters:
	  - Maximise payload mass and remaining second-stage fuel,
	  - Penalize dynamic pressure beyond a threshold value,
	  - Penalize down-range displacement prior to stage separation
        (useful for landing rocket stages).

## Learning Outcomes

- Introduction to some numerical methods.
- Familiarity with type classes from `vector-spaces`, how they can
  improve type safety and generalize algorithms.
- Introduction to `dimensional` and `vector`.
- Appreciation that naive Haskell is easily fast enough to tackle the
  problems in the workshop, but also that different parts could be
  farmed out to `ST`, Fortran or C libraries.
- Appreciation of the under-developed nature of Haskell for numerical
  applications; lack of good libraries providing
  Matlab/SciPy/Julia-like capabilities.
- Introduction to spaceflight. Appreciation of some of the basics of
  mission and manoeuvre planning.

## Slides

N/A

## Video

N/A

## Links

In-progress workshop: https://www.github.com/lancelet/haskell-spaceflight-workshop

## Session Requirements

None.

## Labels

Numerical programming, Haskell, Spaceflight.
