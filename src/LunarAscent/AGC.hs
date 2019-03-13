{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module LunarAscent.AGC where

import           Control.Lens           ((&), (.~), (^.))
import           Data.Metrology         (type (%*), type (%/), (:*) ((:*)),
                                         (:/) ((:/)))
import           Data.Metrology.SI.Poly (Meter (Meter), SI, Second (Second))
import           Data.Metrology.Vector  (type (@+), Normalize, Number (Number),
                                         Qu, qMagnitude, qMagnitudeSq,
                                         qNormalized, qSq, ( # ), (%), (*|),
                                         (|*^|), (|*|), (|+|), (|-|), (|.|),
                                         (|/), (|/|), (|^*|))
import qualified Linear

import           LunarAscent.Types      (AGCState, AccelMag, Acceleration, AscentStage (Coasting, InjectionPositionControl, MainBurn, VerticalRise),
                                         AscentTarget, GravAccel (GravAccel),
                                         Length, LunarModuleSim, Position, R,
                                         ThrustAngle (ThrustAngle), Time, V2,
                                         VelMag, Velocity, agcState, dDynState,
                                         dynState, gPrev, mass, massDot, pos,
                                         stage, targetAltitude, targetVelocity,
                                         tgo, time, unGravAccel, unTGo, vel,
                                         velDot)
import           Orphans                ()



-- | Simplified version of the ascent guidance algorithm.
p12 :: AscentTarget -> LunarModuleSim -> (ThrustAngle, AGCState)
p12 target sim
  = case sim^.agcState^.stage of
      VerticalRise -> verticalRise sim
      _            -> controlledAscent target sim


-- | Vertical rise phase of lunar ascent.
--
-- During the model of the vertical rise phase, we simply apply thrust
-- straight down. The actual ascent stage would rise along its axis
-- for 2s first before straightening (due to the control delay), but
-- it's the same if the lander was parked level.
verticalRise :: LunarModuleSim -> (ThrustAngle, AGCState)
verticalRise sim =
  let
    -- output state of the guidance computer
    state :: AGCState
    state = sim^.agcState
            -- if we have been running for > 10 seconds, then we enter the main
            -- burn stage
          & (stage .~ if sim^.time < 10%Second then VerticalRise else MainBurn)
            -- run the "averageG" routine
          & (gPrev .~ averageG (sim^.agcState^.gPrev)
                               (sim^.dynState^.pos)
                               (sim^.dynState^.vel)
                               (sim^.dDynState^.velDot)
                               (2%Second))
  in
    ( ThrustAngle 0
    , state )


controlledAscent
  :: AscentTarget
  -> LunarModuleSim
  -> (ThrustAngle, AGCState)
controlledAscent target sim =
  let
    r = sim^.dynState^.pos            -- vehicle position
    v = sim^.dynState^.vel            -- vehicle velocity
    a = sim^.dDynState^.velDot        -- vehicle acceleration
    m = sim^.dynState^.mass           -- vehicle mass
    mDot = sim^.dDynState^.massDot    -- mass flow rate
    g_prev = sim^.agcState^.gPrev     -- previous average g value
    t_go = unTGo (sim^.agcState^.tgo) -- previous time-to-go estimate
    v_D = target^.targetVelocity      -- target velocity
    r_D = target^.targetAltitude      -- target altitude

    r_mag = qMagnitude r
    u_R = qNormalized r

    v_E_mag :: VelMag
    v_E_mag = undefined

    -- Mass to mass-flow-rate ratio
    tau :: Time
    tau = m |/| mDot

    -- calculate Average-G and g_eff
    g_N = unGravAccel $ averageG g_prev r v a (2%Second)
    g_eff = qSq (r `qCross2DMag` v) |/| (r_mag |*| r_mag |*| r_mag) |-| qMagnitude g_N  -- TODO: why (r_mag |^ 3) does not work?

    -- compute velocity-to-be-gained and compensate for g_eff
    v_G' = v |-| v_D
    v_G = v_G' |-| (t_go |*| g_eff |/ 2) |*^| u_R
    v_G_mag = qMagnitude v_G

    -- update time-to-go estimate
    t_go' :: Time
    t_go' = tau |*| v_G_mag |/| v_E_mag |*| (1 |-| v_G_mag |/| v_E_mag |/ 2)

    -- control parameter calculation
    l :: Qu '[] SI Double
    l = log (1 |-| t_go' |/| tau)

    d_12 :: Time
    d_12 = tau |+| t_go' |/| l

    d_21 :: Time
    d_21 = t_go' |-| d_12

    eE :: Time
    eE = t_go' |/ 2 |-| d_21

    bB'' :: AccelMag
    bB'' = (d_21 |*| (v_G' |.| u_R) |-| (r_D |-| r_mag |-| (v |.| u_R)|*|t_go'))|/|(t_go' |*| eE)

    bBLower :: AccelMag
    bBLower = (-0.3048 % Meter :/ Second :/ Second :/ Second) |*| tau

    bB' :: AccelMag
    bB'
      | bB'' > (0 % Meter :/ Second :/ Second) = 0 % Meter :/ Second :/ Second
      | bB'' < bBLower = bBLower
      | otherwise = bB''

    bB :: AccelMag
    bB = case sim^.agcState^.stage of
           VerticalRise             -> error "Should not be in vertical rise phase"
           MainBurn                 -> bB'
           InjectionPositionControl -> 0 % Meter :/ Second :/ Second
           Coasting                 -> error "Should not be coasting"

    aA :: VelMag
    aA = -1 *| bB |*| d_12 |-| (v_G' |.| u_R) |/| l

    -- acceleration vector
    a_max :: AccelMag
    a_max = v_E_mag |/| tau

    a_TR :: AccelMag
    a_TR = (aA |+| (1 % Second) |*| bB) |/| tau |-| g_eff

    angle = if a_TR > a_max
            then 0
            else asin (a_TR |/| a_max)
  in
    ( ThrustAngle (angle # Number)
    , undefined
    )


-- | Average G routine.
--
-- From section 5.3.2 POWERED FLIGHT NAVIGATION - AVERAGE-G ROUTINE.
-- Only the lunar portion is implemented here (the Earth calculations are not
-- used for lunar ascent).
averageG
  :: GravAccel     -- ^ Previous average acceleration due to gravity.
  -> Position      -- ^ Position of the vehicle.
  -> Velocity      -- ^ Velocity of the vehicle.
  -> Acceleration  -- ^ Acceleration of the vehicle.
  -> Time          -- ^ Time increment (nominally 2s).
  -> GravAccel     -- ^ Calculated average acceleration due to gravity.
averageG (GravAccel gp) r v a dt =
  let
    r' = r |+| dt |*^| (v |+| (gp |+| a) |^*| dt |/ 2)
    u_R' = qNormalized r'
    r2' = qMagnitudeSq r'
  in
    GravAccel (-1 *| sgpMoon p12Const |/| r2' |*^| u_R')


-- | Magnitude of cross-product between 2D vectors as though they were 3D.
qCross2DMag
  :: forall d1 d2 l.
     ( Normalize (Normalize d1 @+ Normalize d2) ~ Normalize (d1 @+ d2) )
  => Qu d1 l V2
  -> Qu d2 l V2
  -> Qu (Normalize (d1 @+ d2)) l R
qCross2DMag v1 v2 =
  let
    i = Linear.V2 1 0 % Number
    j = Linear.V2 0 1 % Number

    v1x = v1 |.| i
    v1y = v1 |.| j
    v2x = v2 |.| i
    v2y = v2 |.| j
  in
    v1x |*| v2y |-| v1y |*| v2x


data P12Const
  = P12Const
    { -- | Standard gravitational parameter.
      sgpMoon         :: Length %* Length %* Length %/ Time %/ Time
      -- | APS exhaust velocity.
    , exhaustVelocity :: VelMag
    }

p12Const :: P12Const
p12Const
  = P12Const
    { sgpMoon = 0.4902778e13 % Meter :* Meter :* Meter :/ Second :/ Second
    , exhaustVelocity = 3030.0 % Meter :/ Second
    }
