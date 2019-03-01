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
{-# LANGUAGE ScopedTypeVariables #-}

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
  :: forall a. (RealFloat a, Ord a, Show a)
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

    -- mcrit is the critical Mach number from McCoy (1981), below which the
    --  transonic regime produces zero drag and we thus produce zero drag
    --  overall
    mcrit = (1.0 + 0.552 * tau**(4.0/5.0))**(-0.5)
    ss = unHeadDrag
       $ headDragSupersonic mach headLength headShape headMeplatDiam
    ts = unHeadDrag
       $ headDragTransonic mach headLength headMeplatDiam

    -- critw is a linear blending width from subsonic to transonic;
    --  it's expressed as a fraction of the width from the critical Mach
    --  number to Mach 1
    critw = 0.2 * (1.0 - mcrit)
    -- tsw is a linear blending width from transonic to supersonic;
    --  it's an absolute Mach number value
    tsw = 0.15
 
    hd
      | m < (mcrit + critw) = smoothmix (mcrit, mcrit + critw)  0 ts m
      | otherwise           = smoothmix (1.001,     1.0 + tsw) ts ss m
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

    z = m * m - 1.0
    tau = (1.0 - dm) / ln
    c1 = 0.7156 - 0.5313 * rr + 0.5950 * rr * rr
    c2 = 0.0796 + 0.0779 * rr
    c3 = 1.587 + 0.049 * rr
    c4 = 0.1122 + 0.1658 * rr
    k = 0.75
  in
    HeadDrag
    $ ((c1 - c2*tau**2)/z) * (tau * sqrt z)**(c3 + c4 * tau)
      + (pi / 4.0) * k * dm * dm

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
    z = m * m - 1.0
    tau = (1.0 - dm) / ln
    gamma = 1.4
  in
    HeadDrag
    $ 0.368 * tau**(9.0/5.0)
      + (1.6 * tau * z) / ((gamma + 1.0) * m * m)

{-# SPECIALIZE headDragTransonic
  :: Mach Float
  -> HeadLength Float
  -> HeadMeplatDiam Float
  -> HeadDrag Float #-}


-------------------------------------------------------------------------------
-- BoatTail Drag
-------------------------------------------------------------------------------

-- | BoatTail angle (degrees).
newtype BoatTailAngle a = BoatTailAngle a

-- | BoatTail length (calibers).
newtype BoatTailLength a = BoatTailLength a

-- | Length of the cylindrical body (calibers).
newtype BodyCylinderLength a = BodyCylinderLength a

-- | Drag on the boattail region.
newtype BoatTailDrag a = BoatTailDrag { unBoatTailDrag :: a }


boatTailDrag
  :: (Floating a, Ord a)
  => Mach a
  -> HeadLength a
  -> HeadShape a
  -> HeadMeplatDiam a
  -> BodyCylinderLength a
  -> BoatTailAngle a
  -> BoatTailLength a
  -> BoatTailDrag a
boatTailDrag mach
             headLength
             headShape
             headMeplatDiam
             bodyCylinderLength
             boatTailAngle
             boatTailLength =
  let
    Mach m = mach
    
    ss = unBoatTailDrag
       $ boatTailDragSupersonic mach
                                headLength
                                headShape
                                headMeplatDiam
                                bodyCylinderLength
                                boatTailAngle
                                boatTailLength

    btd = if ss < 0
          then 0
          else ss
  
  in
    BoatTailDrag btd
      

boatTailDragSupersonic
  :: (Floating a)
  => Mach a
  -> HeadLength a
  -> HeadShape a
  -> HeadMeplatDiam a
  -> BodyCylinderLength a
  -> BoatTailAngle a
  -> BoatTailLength a
  -> BoatTailDrag a
boatTailDragSupersonic mach
                       headLength
                       headShape
                       headMeplatDiam
                       bodyCylinderLength
                       boatTailAngle
                       boatTailLength =
  let
    Mach m = mach
    HeadLength ln = headLength
    HeadShape rr = headShape
    HeadMeplatDiam dm = headMeplatDiam
    BodyCylinderLength lcyl = bodyCylinderLength
    BoatTailAngle beta = boatTailAngle
    BoatTailLength lbt = boatTailLength

    gamma = 1.4
    tau = (1.0 - dm) / ln
    z = m * m - 1.0
    k = 0.85 / (sqrt z)
    a1 = (1 - 3*rr/(5*m))*
         (5*tau/(6*(sqrt z))*(tau/2)**2 - 0.7435/(m**2)*(tau*m)**1.6)
    a = a1*exp((-1) * sqrt(2/(gamma*m**2)) * lcyl)
        + 2*(tan beta)/(sqrt z)
        - (((gamma + 1)*m**4 - 4*(m**2 - 1)) * (tan beta)**2) / (2*z**2)

    el = exp ((-1)*k*lbt)

    simil = (4*a*(tan beta)/k) *
            (1 - el +
             2*(tan beta)*
             (el * (lbt + (1/k)) - (1/k)))
  
  in
    BoatTailDrag (0.9 * simil)



-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Smooth polynomial mix of functions with clamping.
smoothmix
  :: ( Fractional a, Ord a )
  => (a, a)  -- ^ Blending region.
  -> a       -- ^ Value from function before blend region.
  -> a       -- ^ Value from function after blend region.
  -> a       -- ^ Point at which to evaluate.
  -> a       -- ^ Mixed value.
smoothmix (x0, x1) f g x
  -- must case-split because f and g may not be defined over the whole
  -- domain
  | x <= x0 = f
  | x <= x1 =
      let
        c = (x - x0) / (x1 - x0)
        cg = smootherstep c
        cf = 1.0 - cg
      in
        (cf * f) + (cg * g)
  | otherwise = g

{-# INLINE smoothmix #-}
{-# SPECIALIZE smoothmix
  :: (Float, Float)
  -> Float
  -> Float
  -> Float
  -> Float #-}


-- | Ken Perlin's smootherstep function.
--
--   Ref: https://en.wikipedia.org/wiki/Smoothstep
smootherstep
  :: (Fractional a, Ord a)
  => a  -- ^ Input value.
  -> a  -- ^ Output value.
smootherstep x
  | x <= 0.0  = 0.0
  | x <= 1.0  = x * x * x * (x * (x * 6.0 - 15.0) + 10.0)
  | otherwise = 1.0

{-# INLINE smootherstep #-}
{-# SPECIALIZE smootherstep :: Float -> Float #-}
