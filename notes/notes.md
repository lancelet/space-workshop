---
title: |
    | The Haskell Space Program
    | Planning an Orbital Launch
author:
- Jonathan Merritt 
- Luke Clifton
bibliography: references.bib
csl: american-physics-society.csl
link-citations: true
---

\tableofcontents

# Introduction

Todo.

# Rocket Equations of Motion \label{ch:eom}

The equations of motion of the rocket are a set of coupled,
first-order, ordinary differential equations (ODEs). They describe the
relationships between the rates of change of various quantities and
how the rocket moves. These ODEs provide a basis for all the
simulation we will do later.

We will use a point mass model for the rocket. This means that we
model its linear motion, but we won't model its angular motion 
(orientation, rate of rotation and rotational inertia). For a more
complete consideration of these equations, consult 
[@gantmacher1950].

## Governing Equations

Newton's second law (Eq \ref{eq:newton2}) relates the force applied to
the rocket and its acceleration:
\begin{align}
  \bvec{F} &= m\dot{\bvec{v}} \label{eq:newton2}
\end{align}
The symbols in the equation are defined as follows:

$\defmath{\bvec{F}}$
  ~ Total force applied to the rocket, including thrust from ejected
    reaction mass ($\mathrm{N}$).

$\defmath{m}$
  ~ Instantaneous mass of the rocket, bearing in mind this varies 
    over time ($\mathrm{kg}$).

$\defmath{\bvec{v}}$
  ~ Velocity of the rocket ($\mathrm{m}/\mathrm{s}$).

The rocket burns fuel, causing its mass to decrease over time. The
burning fuel creates an ejection of reaction mass from the rocket
nozzle, which results in a thrust force. Thus, the mass flow rate is
related to the magnitude of thrust force exerted on the
rocket. Modeling this relationship in full is a complicated
hydrodynamics problem, but we can approximate it as a linear
relationship using a quantity known as specific impulse: 
\begin{align}
F_T &= g_0 I_{sp} \dot{m} \label{eq:thrust-mag}
\end{align}

$\defmath{F_T}$
  ~ Magnitude of the thrust force ($\mathrm{N}$).

$\defmath{g_0}$
  ~ Standard gravity (constant) ($9.80665\,\mathrm{m}/\mathrm{s}^2$).

$\defmath{I_{sp}}$
  ~ Specific impulse ($\mathrm{s}$). This is a constant determined by
    the propellant used.

## Coupled ODEs

To control the rocket, we will specify the following input control
signals (both vary with time):

$\defmath{c_{\dot{m}}}$
  ~ Control signal for the mass flow rate ($\mathrm{kg}/\mathrm{s}$).
  
$\defmath{c_\theta}$
  ~ Control signal for the angle of the rocket thrust 
    ($\mathrm{radians}$).

Using these inputs, we can write the following set of coupled ODEs,
based directly on the equations above:
\begin{align}
  \dot{m} &= c_{\dot{m}} \\
  \dot{\bvec{x}} &= \bvec{v} \\
  \dot{\bvec{v}} &= \frac{\bvec{F}}{m} \label{eq:vdot}
\end{align}
Where the force is composed of terms describing thrust, gravity and
drag:
\begin{align}
  \bvec{F} = \bvec{F}_G(\bvec{x}) 
           + \bvec{F}_T(c_{\dot{m}}, c_{\theta}) 
		   + \bvec{F}_D(\ldots)
\end{align}

$\defmath{\bvec{x}}$
  ~ Position of the rocket ($\mathrm{m}$).

$\defmath{\bvec{F}_G}$
  ~ Gravitational force acting on the rocket ($\mathrm{N}$).
  
$\defmath{\bvec{F}_T}$
  ~ Thrust force acting on the rocket ($\mathrm{N}$).

$\defmath{\bvec{F}_D}$
  ~ Aerodynamic drag force acting on the rocket ($\mathrm{N}$).

The remaining details will be filled in during the following
chapters.

# Tsiolkovsky Rocket Equation and Delta-V

Most discussions of rocket capabilities start with a quantity known as
"Delta-V".[^1] It comes from the Tsiolkovsky Rocket Equation, which
can be derived as a closed-form integration of the rocket equations of
motion from Chapter \ref{ch:eom}.

[^1]: It's more fun if you drawl it with same accent you'd use to 
      pronounce the phrase "silicone diode". :-)

## Derivation of the Tsiolkovsky Rocket Equation

Consider a rocket burning fuel and moving in a consistent direction in a
vacuum, with no gravity or aerodynamic drag forces. If we assume that
the mass flow rate of propellant is constant, then we can write the
instantaneous mass of the rocket as: 
\begin{align}
  m(t) = m_0 - \dot{m}\,t
\end{align}

$\defmath{m_0}$
  ~ Mass of the rocket at time $t=0$ ($\mathrm{kg}$).

$\defmath{t}$
  ~ Time ($\mathrm{s}$).

The rocket acceleration during this constant burn is given by the
equation of motion for the acceleration, Eq \ref{eq:vdot} (using
scalars now):
\begin{align}
  \dot{v} &= \frac{F_T}{m} \\ &= \frac{g_0 I_{sp} \dot{m}}{m_0 -
          \dot{m}\,t} \end{align} The definite integral of this
          acceleration over time gives us a final velocity for the
          rocket after it has burned some fuel, between times $t=0$
          and $t=t_f$: \begin{align} v_f &= v_0 + \int_{0}^{t_f}
          \frac{g_0 I_{sp} \dot{m}}{m_0 - \dot{m}\,t} \, dt \\ &=
          v_0 + \left[ \frac{g_0 I_{sp} \dot{m}}{-\dot{m}} \ln (m_0
          -\dot{m}\,t) \right]_{t=0}^{t=t_f} \\ &= v_0 + (-1) g_0
          I_{sp} \left( \ln (m_0 - \dot{m}\,t_f) - \ln (m_0) \right)
          \\ &= v_0 + g_0 I_{sp} \left( \ln (m_0) - \ln (m_0 -
          \dot{m}\,t_f) \right) \\ &= v_0 + g_0 I_{sp} \ln \left(
          \frac{m_0}{m_0 - \dot{m}\,t_f} \right) \end{align}

$\defmath{t_f}$ 
  ~ Time at the end the rocket burn ($\mathrm{s}$).

$\defmath{v_0}$ 
  ~ Starting velocity of the rocket, at time $t=0$
    before the burn ($\mathrm{m}/\mathrm{s}$).

$\defmath{v_f}$ 
  ~ Final rocket velocity, at time $t=t_f$
    ($\mathrm{m}/\mathrm{s}$).
  
In this derivation we have used the integral identity: \begin{align}
\int \frac{c}{ax + b}\,dx = \frac{c}{a} \ln \left| ax+b \right| + C
\end{align}

If we identify the change in velocity and the final mass, the equation
for change in velocity is: \begin{align} \Delta v = v_f - v_0 &= g_0
I_{sp} \ln \left( \frac{m_0}{m_f} \right) \\ \Delta v &= v_e \ln
\left( \frac{m_0}{m_f} \right) \label{eq:tsiolkovsky} \end{align}

$\defmath{\Delta v}$ 
  ~ Change in velocity caused by the burn "Delta-V"
    ($\mathrm{m}/\mathrm{s}$).

$\defmath{m_f}$ 
  ~ Final mass of the rocket 
    ($m_f = m_0 - \dot{m}\,t_f\;\mathrm{kg}$).

$\defmath{v_e}$ 
  ~ Velocity of the rocket exhaust 
    ($v_e = g_0 I_{sp}\;\mathrm{m}/\mathrm{s}$).

Equation \ref{eq:tsiolkovsky} is referred to as "The Tsiolkovsky
Rocket Equation".

## Delta-V and Mission Planning

Equation \ref{eq:tsiolkovsky} gives us a way to quantify the *impulse*
of a rocket system. Although it is written as a change in velocity, it
does not necessarily correspond to the *real* change in velocity of
the rocket in all situations. This is because it does not account for
aerodynamic drag or the effects of gravity, and only describes the
true change in velocity for the gravity-free vacuum burn described.
However, it provides a good way to roughly compare different rocket
systems and to perform rough assessments of capability.

The equations of motion of the rocket have this nice closed-form
solution for a constant burn in a vacuum. However, it's worth pointing
out that a closed-form solution won't generally be possible once we
introduce atmouspheric drag. This is the whole point of the rest of the
workshop.

The approximate Delta-V values for various missions can be calculated
and tabulated in advance, so that rocket systems which might be
suitable for them can be discussed. For example, NASA described the
Delta-V of various stages of the Apollo Lunar Landing missions in the
following figure:

![NASA summary of energy requirements for a Lunar Landing Mission,
illustrating the use of Delta-V for mission planning. 
From [@maynard1966].](
images/ApolloEnergyRequirementsMSC1966.png)

## Rocket Structure and Propellant Mass Fraction

A rocket must support the fuel it contains as well as carry a payload.
The more fuel that it carries, the more mass will be required in the
structure of the rocket to support that fuel (eg. in the form of fuel
tanks, etc.).



# Integrating ODEs

# Fuel

# Serial Staging: Why do Rockets Have Stages?

# Atmospheric Drag

# Stable Circular Orbit

# Particle Swarm Optimisation of Orbital Insertion Guidance

# Extension Material

# References
::: {#refs}
:::
