{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module LunarAscent.AGC where

import           Control.Lens      ((^.))
import           Data.Basis        (Basis, HasBasis)
import           Data.VectorSpace  (InnerSpace, Scalar, VectorSpace, zeroV)

import           LunarAscent.Types (AGCCommand (AGCCommand),
                                    AGCState (AGCState), AscentTarget,
                                    Constants, EngineShutoff (EngineShutoff),
                                    LVCS, MFCS, P2, Sim,
                                    ThrustAngle (ThrustAngle), V2, accel,
                                    agcState, apsExhaustVelocity,
                                    apsMassFlowRate, bThreshold, dynamics, mass,
                                    moonSGP, pos, prevThrustAngle, qcsAssignV2,
                                    qv2, rDotFLVPEnd, t2_HoldAll,
                                    t3_PositionControl, tEngineThreshold,
                                    targetRadius, targetVel, tgo, time, vel)
import           Units             (type (@+), Qu, qMagnitude, qNegate,
                                    qNormalized, qSq, si, ( # ), (%), (%.),
                                    (*|), (|*^|), (|*|), (|+|), (|-|), (|.+^|),
                                    (|.-.|), (|.|), (|/), (|/|), (|^*|))
import qualified Units             as U


-- | Ascent guidance program P12.
p12
  :: forall a.
     ( InnerSpace a, a ~ Scalar a, Floating a, Ord a, Basis a ~ (), HasBasis a, Show a )
  => Constants a
  -> AscentTarget a
  -> Sim a
  -> (AGCCommand a, AGCState a)
p12 constants target sim =
  let
    -- convert position and velocity from moon-fixed coordinates
    -- (MFCS) to local vertical coordinates (LVCS)
    p_l :: U.Length (P2 LVCS a)
    v_l :: U.Velocity (V2 LVCS a)
    a_l :: U.Acceleration (V2 LVCS a)
    (p_l, v_l, a_l) = mfcsToLocal (sim^.dynamics.pos)
                                  (sim^.dynamics.vel)
                                  (sim^.accel)
    p_lv :: U.Length (V2 LVCS a)
    p_lv = p_l |.-.| (zeroV %. [si| m |])  -- LVCS position as a vector

    -- unit vector in the radial direction of the local coordinate system
    u_R = qv2 @LVCS 1 0

    -- radial position and velocity
    r    = qMagnitude p_lv
    rDot = v_l |.| u_R

    -- target radial position and velocity
    r_D    = target^.targetRadius
    rDot_D = target^.targetVel |.| u_R

    -- v_G' - the velocity to be gained, which is the target velocity
    -- minus current LVCS velocity
    v_G'  = target^.targetVel |-| v_l

    -- compensate v_G' for g_eff
    g_N = averageG (constants^.moonSGP) p_l v_l a_l (2 % [si| s |])
    g_eff = qSq (p_lv `qCross2D` v_l) |/| (r |*| r |*| r) |-| qMagnitude g_N
    v_G = v_G' |-| (0.5 *| sim^.agcState.tgo |*| g_eff |*^| u_R)

    -- update time-to-go estimate
    tau = sim^.dynamics.mass |/| constants^.apsMassFlowRate
    ve = constants^.apsExhaustVelocity
    v_G_mag = qMagnitude v_G
    tgo' = tau |*| v_G_mag |/| ve |*| (1 |-| 0.5 *| v_G_mag |/| ve)  -- Apollo original
    -- tgo' = tau |*| (1 |-| exp (-1 *| v_G_mag |/| ve))  -- more accurate Tsiolkovsky solution

    -- compute control rate signals
    l = log (1 |-| tgo' |/| tau)  -- ~= -1 *| v_G_mag |/| ve
    d12 = tau |+| tgo' |/| l
    d21 = tgo' |-| d12
    e = (tgo' |/ 2) |-| d21
    b' = (d21 |*| (rDot_D |-| rDot) |-| (r_D |-| r |-| rDot |^*| tgo'))
         |/| (tgo' |*| e)
    -- "B" control parameter
    b
      -- in "Injection Position Control" mode, the B parameter is set to zero
      | tgo' < constants^.t3_PositionControl = 0 % [si| m/s^2 |]
      -- B parameter must be negative; it's clamped at zero
      | b' > 0 % [si| m/s^2 |]               = 0 % [si| m/s^2 |]
      -- the B parameter has a low threshold that it can't drop below
      | b' < constants^.bThreshold |*| tau   = constants^.bThreshold |*| tau
      -- in between the lower and upper thresholds; use the computed value
      | otherwise                            = b'
    -- "A" control parameter.
    a = (-1) *| b |*| d12 |-| (rDot_D |-| rDot) |/| l

    -- compute thrust angle
    aTR = (a |+| (1 % [si| s |]) |*| b) |/| tau |-| g_eff   -- desired rad accel
    -- In the real ACG, the maximum acceleration is computed from
    -- filtered measurements of thrust; that's not necessary
    -- here... just using the nominal value based on the exhaust
    -- velocity.
    aMax = constants^.apsExhaustVelocity |/| tau  -- maximum acceleration
    angle = if aTR > aMax
            -- if we request more radial thrust than the maximum
            -- possible acceleration, then just command the thrust
            -- straight down
            then ThrustAngle 0
            -- otherwise, compute the thrust angle by comparing the
            -- required radial thrust to the total available thrust
            else ThrustAngle $ acos (aTR |/| aMax) # [si| |]

    -- near the end of the burn, we schedule the engine to cut-off;
    -- this is done once the time-to-go has passed a threshold point
    shutoff = if tgo' < constants^.tEngineThreshold
              then Just (EngineShutoff (sim^.time |+| tgo'))
              else Nothing

    -- compute the new commanded thrust angle
    thrustAngle
      -- during the vertical ascent we use a fixed zero thrust
      -- angle. vertical ascent is any time prior to achieving a
      -- vertical velocity of (nominally) 40feet-per-second
      | rDot < constants^.rDotFLVPEnd = ThrustAngle 0
      -- during the final control-hold stage (nominally 2 sec), we use
      -- the previously-commanded thrust angle
      | tgo' < constants^.t2_HoldAll = sim^.agcState.prevThrustAngle
      -- all other times; use the computed control thrust angle
      | otherwise = angle
  in
    (AGCCommand thrustAngle shutoff, AGCState tgo' thrustAngle)


-- | Convert position and velocity in moon-fixed coordinates ('MFCS')
-- to local vertical coordinates ('LVCS').
mfcsToLocal
  :: forall a.
     ( InnerSpace a, Floating (Scalar a), Num a )
  => U.Length (P2 MFCS a)        -- ^ Position in moon-fixed coords.
  -> U.Velocity (V2 MFCS a)      -- ^ Velocity in moon-fixed coords.
  -> U.Acceleration (V2 MFCS a)  -- ^ Acceleration in moon-fixed coords.
  -> ( U.Length (P2 LVCS a)
     , U.Velocity (V2 LVCS a)
     , U.Acceleration (V2 LVCS a) )
mfcsToLocal p v a =
  let
    pv = qcsAssignV2 $ p |.-.| (zeroV %. [si| m |])

    u_R = qNormalized $ pv
    u_Z = qRotatedCW u_R

    r_LVCS = qv2 @LVCS 1 0
    z_LVCS = qv2 @LVCS 0 1

    p' = (zeroV %. [si| m |])
         |.+^| ((pv |.| u_R) |*^| r_LVCS)
         |.+^| ((pv |.| u_Z) |*^| z_LVCS)
    v' = ((v |.| u_R) |*^| r_LVCS)
         |+| ((v |.| u_Z) |*^| z_LVCS)
    a' = ((a |.| u_R) |*^| r_LVCS)
         |+| ((a |.| u_Z) |*^| z_LVCS)
  in
    (p', v', a')


-- | Rotate vector 90 degrees clockwise.
qRotatedCW
  :: forall d l c a.
     ( VectorSpace a, Fractional (Scalar a), Num a, InnerSpace a
     , U.Normalize (U.Normalize d) ~ U.Normalize d )
  => U.Qu d l (V2 c a) -> U.Qu (U.Normalize d) l (V2 c a)
qRotatedCW v =
  let
    i = qv2 1 0
    j = qv2 0 1
    x = v |.| i
    y = v |.| j
    x' = y
    y' = qNegate x
  in
    (x' |*^| i) |+| (y' |*^| j)


-- | Instantaneous acceleration due to gravity.
gravAccel
  :: forall a c.
     ( InnerSpace a, Floating a, a ~ Scalar a )
  => U.SGPUnit a
  -> U.Length (P2 c a)
  -> U.Acceleration (V2 c a)
gravAccel mu r =
  let
    vr = r |.-.| (zeroV %. [si| m |])
    rhat = U.qNormalized vr
    r2 = U.qMagnitudeSq vr
  in
    -1 *| mu |/| r2 |*^| rhat


-- | Average gravity during a time period.
averageG
  :: ( InnerSpace a, a ~ Scalar a, Floating a )
  => U.SGPUnit a                 -- Specific gravitational parameter.
  -> U.Length (P2 LVCS a)        -- Position of the vehicle.
  -> U.Velocity (V2 LVCS a)      -- Velocity of the vehicle.
  -> U.Acceleration (V2 LVCS a)  -- Acceleration of the vehicle.
  -> U.Time a                    -- Time increment.
  -> U.Acceleration (V2 LVCS a)  -- Average velocity during the time increment.
averageG mu r v a dt =
  let
    r' = r |.+^| ((1/8) *| a |^*| (dt |*| dt)) |.+^| ((1/2) *| v |^*| dt)
  in
    gravAccel mu r'


-- | Magnitude of a cross-product between 2D vectors as though they were 3D.
qCross2D
  :: ( Scalar a ~ a, InnerSpace a, Num a
     , U.Normalize (U.Normalize d1 @+ U.Normalize d2) ~ U.Normalize (d1 @+ d2) )
  => Qu d1 l (V2 c a)
  -> Qu d2 l (V2 c a)
  -> Qu (U.Normalize (d1 @+ d2)) l a
qCross2D va vb =
  let
    i = qv2 1 0
    j = qv2 0 1

    vax = va |.| i
    vay = va |.| j
    vbx = vb |.| i
    vby = vb |.| j
  in
    (vax |*| vby) |-| (vay |*| vbx)
