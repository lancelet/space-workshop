{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NegativeLiterals   #-}
{-# LANGUAGE TypeOperators      #-}
module LunarAscent.AGC where

import           Control.Lens           ((&), (.~), (^.))
import           Data.Metrology         (type (%*), type (%/), (:*) ((:*)),
                                         (:/) ((:/)), Qu)
import           Data.Metrology.SI.Poly (Meter (Meter), SI, Second (Second))
import           Data.Metrology.Vector  (qMagnitude, qMagnitudeSq, qNormalized,
                                         (%), (*|), (|*^|), (|*|), (|+|), (|-|),
                                         (|/), (|/|), (|^*|))

import           LunarAscent.Types      (AGCState, Acceleration, AscentStage (Coasting, InjectionPositionControl, MainBurn, VerticalRise),
                                         AscentTarget, DimensionlessV2,
                                         GravAccel (GravAccel), Length,
                                         LunarModuleSim, Position, TGo (TGo),
                                         ThrustAngle (ThrustAngle), Time, V2,
                                         Velocity, agcState, dynState, gPrev,
                                         pos, stage, targetVelocity, time, vel)
import           Orphans                ()


p12 :: AscentTarget -> LunarModuleSim -> (ThrustAngle, AGCState)
p12 target sim
  = case sim^.agcState^.stage of
      VerticalRise -> verticalRise sim
      _            -> controlledAscent target sim


-- | Vertical rise phase of lunar ascent.
--
--   During the model of the vertical rise phase, we simply apply thrust
--   straight down.
verticalRise :: LunarModuleSim -> (ThrustAngle, AGCState)
verticalRise sim =
  let
    -- radial distance of the vehicle from the moon's center
    r :: Length
    r = qMagnitude (sim^.dynState^.pos)

    -- unit vector along the radial direction
    u_R :: DimensionlessV2
    u_R = qNormalized (sim^.dynState^.pos)

    state :: AGCState
    state = sim^.agcState
            -- if we have been running for > 10 seconds, then we enter the main
            -- burn stage
          & (stage .~ if sim^.time < 10%Second then VerticalRise else MainBurn)
            -- keep initializing the "previous gravity" value
          & (gPrev .~ GravAccel
             (-1 *| (sgpMoon p12Const) |/| (r |*| r) |*^| u_R))
  in
    ( ThrustAngle 0
    , state )


controlledAscent
  :: AscentTarget
  -> LunarModuleSim
  -> (ThrustAngle, AGCState)
controlledAscent target sim =
  let
    -- radial distance of the vehicle from the moon's center
    r :: Length
    r = qMagnitude (sim^.dynState^.pos)

    -- Unit vector in the radial direction
    u_R :: DimensionlessV2
    u_R = qNormalized (sim^.dynState^.pos)

    -- Acceleration due to gravity
    --
    -- This is an approximation; the original AGC computes an average gravity
    -- over the entire time step.
    g_N :: Acceleration
    g_N = -1 *| (sgpMoon p12Const) |/| (r |*| r) |*^| u_R

    --
    g_eff = undefined

    -- Velocity to be gained
    v_G :: Velocity
    v_G = target^.targetVelocity |-| sim^.dynState^.vel

    -- Compensate for g_eff
  {-
    v_G' :: Velocity
    v_G' = v_G ^-^ sim^.tgo*g_eff*
  -}

  in
    undefined


-- | Average G routine.
--
-- From section 5.3.2 POWERED FLIGHT NAVIGATION - AVERAGE-G ROUTINE
-- Only the lunar portion is implemented here (the Earth calculations are not
-- used for lunar ascent).
averageG
  :: GravAccel     -- ^ Previous avg acceleration due to gravity.
  -> Position      -- ^ Position of the vehicle.
  -> Velocity      -- ^ Velocity of the vehicle.
  -> Acceleration  -- ^ Acceleration of the vehicle.
  -> Time          -- ^ Time increment (nominally 2s).
  -> GravAccel     -- ^ Calculated avg acceleration due to gravity.
averageG (GravAccel gp) r v a dt =
  let
    r' = r |+| dt |*^| (v |+| (gp |+| a) |^*| dt |/ 2)
    u_R' = qNormalized r'
    r2' = qMagnitudeSq r'
  in
    GravAccel (-1 *| sgpMoon p12Const |/| r2' |*^| u_R')


data P12Const
  = P12Const
    { -- | Standard gravitational parameter.
      sgpMoon :: Length %* Length %* Length %/ Time %/ Time
    }

p12Const :: P12Const
p12Const
  = P12Const
    { sgpMoon = 0.4902778e13 % Meter :* Meter :* Meter :/ Second :/ Second
    }
