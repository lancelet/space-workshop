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


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

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


-- | Head meplat diameter (calibers).
newtype HeadMeplatDiam a = HeadMeplatDiam a


-- | Drag term due to the conical head of the projectile.
newtype HeadDrag a = HeadDrag { unHeadDrag :: a }


-------------------------------------------------------------------------------
-- Head Drag
-------------------------------------------------------------------------------

-- | Head drag.
--
--   To compute overall head drag, we blend between the subsonic, transonic
--   and supersonic regimes.
headDrag
  :: (Floating a, Ord a)
  => Mach a
  -> HeadLength a
  -> HeadShape a
  -> HeadMeplatDiam a
  -> HeadDrag a
headDrag mach headLength headShape headMeplatDiam =
  let
    Mach m = mach
    HeadLength ln = headLength
    HeadMeplatDiam dm = headMeplatDiam

    tau = (1.0 - dm) / ln

    -- mcrit is the critical Mach number, below with the transonic regime
    --  produces zero drag and can be ignored
    mcrit = (1.0 + 0.552 * tau**(4.0/5.0))**(-0.5)
    ss = unHeadDrag
       $ headDragSupersonic mach headLength headShape headMeplatDiam
    ts = unHeadDrag
       $ headDragTransonic mach headLength headMeplatDiam

    -- critw is a linear blending width from subsonic to transonic;
    --  it's expressed as a fraction of the width from the critical Mach
    --  number to Mach 1
    critw = (1.0 / 5.0) * (1.0 - mcrit)
    -- tsw is a linear blending width from transonic to supersonic;
    --  it's an absolute Mach number value
    tsw = 0.2
 
    hd
      | m < (mcrit + critw) = lerp (mcrit, 0) (mcrit + critw, ts) m
      | otherwise           = lerp (1.0, ts) (1.0 + tsw, ss) m
  in
    HeadDrag hd

{-# SPECIALIZE headDrag
  :: Mach Float
  -> HeadLength Float
  -> HeadShape Float
  -> HeadMeplatDiam Float
  -> HeadDrag Float
  #-}


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

{-# SPECIALIZE headDragSupersonic
  :: Mach Float
  -> HeadLength Float
  -> HeadShape Float
  -> HeadMeplatDiam Float
  -> HeadDrag Float
  #-}


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

{-# SPECIALIZE headDragTransonic
  :: Mach Float
  -> HeadLength Float
  -> HeadMeplatDiam Float
  -> HeadDrag Float
  #-}


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Linear interpolation with clamping.
lerp
  :: (Floating a, Ord a)
  => (a, a)  -- Initial (x,y) value.
  -> (a, a)  -- Final (x,y) value.
  -> a       -- Input x value.
  -> a       -- Output y value.
lerp (x1, y1) (x2, y2) x
  | x <= x1           = y1
  | x > x1 && x < x2  = let q = (x-x1)/(x2-x1) in q * y2 + (1.0-q) * y1
  | otherwise         = y2
{-# INLINE lerp #-}
