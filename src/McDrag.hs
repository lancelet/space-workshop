{-|
Module      : McDrag
Description : Estimating drag coefficients of projectiles
Maintainer  : Jonathan Merritt <j.s.merritt@gmail.com>

Estimate drag coefficients of projectiles. This module implements equations
that approximate drag for projectiles published in:

  McCoy, Robert L (1981) "MC DRAG" - A Computer Program for Estimating
    the Drag Coefficients of Projectiles. Technical Report
    ARBRL-TR-02293, US Army Armament Research and Development Command,
    Ballistic Research Laboratory, Aberdeen Proving Ground, Maryland.

=== Note on Valid Ranges of Inputs

The original paper notes that it is based on equations founded on
theory, with fitting of data across some experimental region. The
coefficients in the paper are known to be valid over a Mach range of
0.5 to 5 and a projectile diameter range of 4mm to 400mm. However, the
paper also applies the calculations to "a scale model of a Minuteman
re-entry stage vehicle". Although only a scale model, presumably
calculations were performed on this model to evaluate the behavior of
a full-sized Minuteman re-entry stage, whose size lies outside the
known valid range.
-}

module McDrag where


-- | Free stream Mach number.
newtype Mach a = Mach a


-- | Length of the conical head of the projectile (calibers).
newtype HeadLength a = HeadLength a


-- | Headshape parameter.
--
--   This is the ratio of the tangent radius to the actual ogive radius. Thus
--   (HeadShape 1.0) isa cone, and (HeadShape 0.0) is a pure ogive nose, and
--   values between describe the family of ogive nosecones.
newtype HeadShape a = HeadShape a


newtype HeadMeplatDiam a = HeadMeplatDiam a


-- | Drag term due to the conical head of the projectile.
newtype HeadDrag a = HeadDrag a


-- | Drag on the head of a projectile at supersonic speeds.
--
--   Equation (4) from McCoy (1981).
headDragSupersonic
  :: (Floating a)
  => Mach a
  -> HeadLength a
  -> HeadShape a
  -> HeadMeplatDiam a
  -> HeadDrag a
headDragSupersonic mach headLength headShape headMeplatDiam =
  let
    Mach m = mach
    HeadLength ln = headLength
    HeadShape rr = headShape
    HeadMeplatDiam dm = headMeplatDiam

    z = m**2 - 1.0
    tau = (1.0 - dm) / ln
    c1 = 0.7156 - 0.5313 * rr + 0.5950 * rr**2
    c2 = 0.0796 + 0.0779 * rr
    c3 = 1.587 + 0.049 * rr
    c4 = 0.1122 + 0.1658 * rr
    k = 0.75
  in
    HeadDrag
    $ ((c1 - c2*tau**2)/z) * (tau * sqrt z)**(c3 + c4 * tau)
      + (pi / 4.0) * k * dm**2


-- | Drag on the head of a projectile at transonic speeds.
--
--   Equation (8) from McCoy (1981).
headDragTransonic
  :: (Floating a)
  => Mach a
  -> HeadLength a
  -> HeadMeplatDiam a
  -> HeadDrag a
headDragTransonic (Mach m) (HeadLength ln) (HeadMeplatDiam dm) =
  let
    z = m**2 - 1.0
    tau = (1.0 - dm) / ln
    gamma = 1.4
  in
    HeadDrag
    $ 0.368 * tau**(9.0/5.0)
      + 1.6 * tau * z / ((gamma + 1.0) * m**2)
