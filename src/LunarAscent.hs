{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE QuasiQuotes      #-}

module LunarAscent where

import           Control.Lens          ((^.))
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Metrology.Vector (( # ), (%), (*|), (|*^|), (|*|), (|+|),
                                        (|/|))
import           Data.Units.SI.Parser  (si)
import qualified Diagrams.Prelude      as D
import qualified Linear

import           LunarAscent.AGC       (exhaustVelocity, gravAccel, p12,
                                        p12Const)
import           LunarAscent.Types     (AscentStage (VerticalRise),
                                        AscentTarget (AscentTarget), DDynState,
                                        DynState, LunarModuleSim,
                                        TCutoff (TCutoff), TGo (TGo),
                                        ThrustAngle (ThrustAngle), agcState,
                                        dDynState, dynState, engineCutoffTime,
                                        mass, massDot, mkAGCState, mkDynState,
                                        mkDynStateGradient, mkLunarModuleSim,
                                        pos, time, vel, unGravAccel)
import qualified ODE
import qualified Plot


{-
plotLunarAscent :: Plot.Output -> IO ()
plotLunarAscent output = do
  let
    v2toTuple (Linear.V2 x y) = (x, y)

    ascentTarget = AscentTarget (5.9436 % [si| m/s |]) (1679.2956 % [si| m/s |]) ((1731.1 % [si| km |]) |+| (18288 % [si| m |]))
    sims = burnBabyBurn ascentTarget
    coasts = coastBabyCoast ((NonEmpty.last sims)^.dynState)
    rBurn = v2toTuple <$> (\sim -> sim^.dynState^.pos # [si| km |]) <$> sims
    rCoast = v2toTuple <$> (\state -> state^.pos # [si| km |]) <$> coasts

  {-
  putStrLn . show $ sims
  putStrLn . show $ rs
  -}
  putStrLn $ show ascentTarget
  putStrLn $ show (NonEmpty.last sims)

  Plot.plotOrbitSystem output
    (Plot.OrbitSystem
     [ Plot.Trajectory (NonEmpty.toList rBurn) D.red
     , Plot.Trajectory (NonEmpty.toList rCoast) D.grey
     , Plot.Planet 1731.1 D.grey
     ])


coastBabyCoast :: DynState -> NonEmpty DynState
coastBabyCoast state0 = snd <$> ODE.integrate ODE.rk4Step state0 (NonEmpty.fromList [0, 10 .. 10000]) gradFn
  where
    gradFn :: ((Double, DynState) -> DDynState)
    gradFn (_, state) =
      let
        dpos = state^.vel
        dvel = unGravAccel (gravAccel (state^.pos))
        dmass = 0 % [si| kg/s |]
      in
        mkDynStateGradient dpos dvel dmass
      
  

burnBabyBurn :: AscentTarget -> NonEmpty LunarModuleSim
burnBabyBurn target =
  let
    agcState0 = mkAGCState VerticalRise (TGo (370 % [si| s |])) Nothing (gravAccel r0)

    r0 = Linear.V2 0 1731.1 % [si| km |]
    v0 = Linear.V2 0 0 % [si| m/s |]
    mass0 = (2445.0 + 2252.0) % [si| kg |]
    dynState0 = mkDynState r0 v0 mass0

    dr0 = Linear.V2 0 0 % [si| m/s |]
    dv0 = Linear.V2 0 0 % [si| m/s^2 |]
    dmass0 = -1 *| mass0 |/| (912.02 % [si| s |])  -- from tau0
    ddynState0 = mkDynStateGradient dr0 dv0 dmass0

    sim0 = mkLunarModuleSim (0 % [si| s |]) agcState0 dynState0 ddynState0

    f :: LunarModuleSim -> (LunarModuleSim, Maybe LunarModuleSim)
    f x = (x, burnStep target x)
  in
    NonEmpty.unfoldr f sim0


burnStep :: AscentTarget -> LunarModuleSim -> Maybe LunarModuleSim
burnStep target sim =
  let
    ti = sim^.time # [si| s |]
    tf = ti + 2

    -- first we check in with the guidance computer for our thrust angle and
    -- computer state update
    (ThrustAngle theta, agcState') = p12 target sim

    -- thrust = exhaustVelocity p12Const |*| dmass0
    thrust = 15600.0 % [si| N |]

    -- specify the gradient function (the equations of motion)
    gradFn :: ((Double, DynState) -> DDynState)
    gradFn (_, state) =
      let
        dpos = state^.vel
        phi = (pi/2) - theta
        dvelHat = Linear.V2 (cos phi) (sin phi) % [si| |]
        dvel = thrust |/| state^.mass |*^| dvelHat |+| unGravAccel (gravAccel (state^.pos))
        dmass = sim^.dDynState^.massDot
      in
        mkDynStateGradient dpos dvel dmass

    -- next integrate forward for 2s
    steps = ODE.integrate ODE.rk4Step (sim^.dynState) (NonEmpty.fromList [ti, tf]) gradFn
    lastStep = NonEmpty.last steps

    sim' = mkLunarModuleSim (tf % [si| s |]) agcState' (snd lastStep) (gradFn (ti, sim^.dynState))
  in 
    case sim^.agcState^.engineCutoffTime of
      Just (TCutoff tc) | tc <= sim^.time -> Nothing
      _                                   -> Just sim'
-}
