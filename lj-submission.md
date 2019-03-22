# Spaceflight Workshop

## Abstract

In celebration of the 50th Anniversary year of the Apollo 11 moon
landing, we present a spaceflight-themed exploration of numerical
methods in Haskell!

This workshop focuses on both the joys and pain-points of
intentionally using Haskell's abstractions for numerical work. We
consider what approaches are available to make the time-domain
simulation of dynamical systems safer. To do this, we combine
`vector-spaces`, `units`, `linear`, and several other libraries, to
solve practical problems. The results are not always ergonomic
(warning: may contain some horrible type errors), but we feel they
help to demonstrate what is currently possible and motivate further
development. Unlike many more theoretical presentations of this topic,
our focus is very much on solving *real* problems, selected from the
published spaceflight literature.

Participants will use abstractions from the `vector-spaces` library,
applied to numerical integration of ordinary differential equations
(ODEs) and simple optimization algorithms. They will see how these
concepts allow the algorithms to be expressed more generally, even
allowing types with statically-checked units to appear in them.

On the spaceflight side, participants will implement some basic
spacecraft manoevures, and there are some pre-baked simulations that
interact with participants' code. Among the scenarios examined is a
simulation of the lunar ascent phase of the Apollo missions, including
a faithful transcription into Haskell of the actual guidance algorithm
used during the lunar ascent (our Haskell version has
statically-checked, auto-converted units - which turned out to be
useful when mixing feet, metres, and nautical miles!).

We encourage all forms of participation; from people who want to
follow the pre-baked set of problems, to those who may want to
re-implement our examples in other languages, or just deep-dive into
the spaceflight theory. All the problems have fallback solutions that
can be examined or called directly.

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

A MacOS or Linux machine (not tested on Windows). Ideally download the
workshop git repository in advance and make sure you have all the
dependencies installed.

## Outline / Structure

Participants will be presented with a series of problems in a Haskell
project.
 
The more complicated scenarios covered in the workshop are
"pre-baked", based either on broad descriptions in the published
academic spaceflight literature or on specific research
papers. Miscellaneous functionality such as plotting results is
generally already set up.

Chronological list of topics / scenarios (this is an outline, NOT the 
workshop instructions):
  - Integration of ODEs. Solving the initial value problem using
    Euler's method and RK4, implemented with both concrete types and
    affine spaces. Introduction to derivatives as linear maps, with
    the motivation of enabling type-safe units.
  - Use the participant's RK4 implementation for the lunar ascent
    simulation.
  - Implement triggers in ODE driver using bisection.
  - Investigate rocket staging and support mass budget. Simple setup
    of ODEs; comparison with closed-form results.
  - Orbital mechanics:
    - Simulated Hohmann transfers.
	- Rendezvous manoeuvres with multiple transfers.
  - Optimization methods. Implement gradient descent.
    - Optimizing "pitchover" angle for a gravity-turn trajectory in a
      vacuum flight.
    - Optimizing multiple objectives for an atmospheric flight.

## Learning Outcomes

- Introduction to practical examples of affine spaces, vector spaces,
  derivatives as linear maps, and the interactions of these with other
  numeric types, such as when type-checked units are introduced.
- Appreciation of the benefits of writing more generic numerical
  algorithms, abstracted over more than just a single scalar type
  (ie. `Num a` is definitely not the end of useful abstraction).
- Appreciation of cost-benefit, and some of the more difficult, thorny
  points that arise: some difficulties in making libraries work
  together, necessary orphans, the horrible type errors that can
  appear when you have a unit type wrapping a vector type wrapping
  some underlying scalar, dependent type checking failures, etc. (NB:
  we have made sure that these are all "solved problems" in the
  workshop, but we mention them as appropriate.)
- Introduction to time-domain simulation of dynamical systems.

## Slides

N/A

## Video

N/A

## Links

The workshop is very much in-progress, but we already have a working
version of the Apollo guidance algorithm mentioned above. The
WIP workshop is here: 
https://www.github.com/lancelet/space-workshop

## Session Requirements

None.

## Labels

Numerical methods, Haskell, Spaceflight.
