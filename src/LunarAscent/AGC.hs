{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NegativeLiterals   #-}
{-# LANGUAGE TypeOperators      #-}
module LunarAscent.AGC where

import           Control.Lens           ((&), (.~), (^.))
import           Data.AdditiveGroup     ((^-^))
import           Data.Metrology         (type (%*), type (%/), (:*) ((:*)),
                                         (:/) ((:/)), Qu)
import           Data.Metrology.SI.Poly (Meter (Meter), SI, Second (Second))
import           Data.Metrology.Vector  (qMagnitude, qNormalized, (%), (*|),
                                         (|*^|), (|*|), (|/|))

import           LunarAscent.Types      (Acceleration, AscentPhase (ControlledAscent, VerticalRise),
                                         AscentTarget, DimensionlessV2, Length,
                                         LunarModuleSim,
                                         ThrustAngle (ThrustAngle), Time, V2,
                                         Velocity, ascentPhase, dynState, flags,
                                         pos, targetVelocity, time, vel)
import           Orphans                ()


p12 :: AscentTarget -> LunarModuleSim -> (ThrustAngle, LunarModuleSim)
p12 target sim = case sim^.flags^.ascentPhase of
            VerticalRise     -> verticalRise sim
            ControlledAscent -> controlledAscent target sim


verticalRise :: LunarModuleSim -> (ThrustAngle, LunarModuleSim)
verticalRise sim
  = ( ThrustAngle 0,
      if sim^.time < 10%Second
      then sim
      else sim & flags.ascentPhase .~ ControlledAscent )



controlledAscent
  :: AscentTarget
  -> LunarModuleSim
  -> (ThrustAngle, LunarModuleSim)
controlledAscent target sim =
  let
    r :: Length
    r = qMagnitude (sim^.dynState^.pos)

    -- Unit vector in the radial direction
    u_R :: DimensionlessV2
    u_R = qNormalized (sim^.dynState^.pos)

    -- Acceleration due to gravity
    g_N :: Acceleration
    g_N = -1 *| (sgpMoon p12Constants) |/| (r |*| r) |*^| u_R


    g_eff = undefined

    -- Velocity to be gained
    v_G :: Velocity
    v_G = target^.targetVelocity ^-^ sim^.dynState^.vel

    -- Compensate for g_eff
  {-
    v_G' :: Velocity
    v_G' = v_G ^-^ sim^.tgo*g_eff*
  -}

  in
    undefined


data P12Constants
  = P12Constants
    { -- | Standard gravitational parameter.
      sgpMoon :: Length %* Length %* Length %/ Time %/ Time
    }

p12Constants
  = P12Constants
    { sgpMoon = 0.4902778e13 % Meter :* Meter :* Meter :/ Second :/ Second
    }
