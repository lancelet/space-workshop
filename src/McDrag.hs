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
{-# LANGUAGE RecordWildCards #-}

module McDrag where

-- | Mach number.
newtype Mach a = Mach a

-- | Kinematic viscosity.
newtype KinVisc a = KinVisc a

-- | Free stream velocity.
newtype FreeStreamVel a = FreeStreamVel a


data BoundaryLayer
  = FullyTurbulent  -- ^ Fully-turbulent boundary layer everywhere.
  | LaminarOnNose   -- ^ Laminar boundary layer only on the nose.
  deriving (Show)


data McParams a
  = McParams
    { d_REF :: !a  -- ^ Reference diameter (m)
    , l_T   :: !a  -- ^ Projectile total length (calibers)
    , l_N   :: !a  -- ^ Nose length (calibers)
    , hsp   :: !a  -- ^ Headshape parameter, R_T/R
    , l_BT  :: !a  -- ^ Boattail length (calibers)
    , d_B   :: !a  -- ^ Base diameter (calibers)
    , d_M   :: !a  -- ^ Meplat diameter (calibers)
    , d_RB  :: !a  -- ^ Rotating band diameter (calibers)
    , bl    :: !BoundaryLayer
    } deriving (Show)


data McOut a
  = McOut
    { c_DHS  :: !a  -- ^ Head drag coeff (supersonic).
    , c_DHT  :: !a  -- ^ Head drag coeff (transonic).
    , c_MC   :: !a  -- ^ Critical Mach no. for c_DHT lower transonic limit.
    , c_DBTS :: !a  -- ^ Boattail drag coeff (supersonic).
    , c_DBTT :: !a  -- ^ Boattail drag coeff (transonic).
    , c_DSF  :: !a  -- ^ Skin friction drag ceff.
    , c_DB   :: !a  -- ^ Base drag coeff.
    } deriving (Show)


mcDragBasic
  :: forall a. Floating a
  => McParams a
  -> Mach a
  -> KinVisc a
  -> FreeStreamVel a
  -> McOut a
mcDragBasic McParams{..} (Mach m) (KinVisc nu) (FreeStreamVel u) =
  let
    -- Additional geometric parameters
    beta = undefined
    l_CYL = undefined
    
    -- Ratio of specific heats (C_pressure / C_volume)
    --   Approximately 1.4 for air:
    --   http://hyperphysics.phy-astr.gsu.edu/hbase/Kinetic/shegas.html
    gamma = 1.4
    

    ---- HEAD DRAG
  
    -- Head drag fitting constants
    c1 = 0.7156 - 0.5313*hsp + 0.595*hsp*hsp
    c2 = 0.0796 + 0.0779*hsp
    c3 = 1.587 + 0.049*hsp
    c4 = 0.1122 + 0.1658*hsp

    -- thickness ratio
    tau = (1 - d_M) / l_N

    -- terms for head drag
    c01 = m*m - 1
    c02 = sqrt c01
    c03 = c1 - c2*tau*tau
    c04 = c3 + c4*tau
    c05 = pi/4*k*d_M*d_M
    c06 = 0.368*tau**(9.0/5.0)
    c07 = gamma + 1
    c08 = 1.6*tau*c01/c07/m/m
    k = 0.85/c02

    -- head drag
    c_DHS = c03/c01*(tau*c02)**c04 + c05
    c_DHT = c06 + c08
    c_MC = sqrt (1 + 0.552*tau**(4.0/5.0))


    ---- BOATTAIL DRAG

    -- terms for boattail drag
    c09 = 1 - 3*hsp/5/m
    c10 = 5*tau/6/c02*(tau/2)**2
    c11 = 0.7435/m/m*(tau*m)**1.6
    c12 = sqrt (2/gamma/m/m)
    c13 = tan beta
    c14 = 4*c01 - c07*m**4
    c15 = exp(-c12*l_CYL)
    c16 = c14*c13*c13/2/c01/c01
    c17 = exp(-k*l_BT)
    c18 = exp(-2*l_BT)
    c19 = c17*(l_BT + 1/k) - 1/k
    c20 = c18*(l_BT + 1/2) - 1/2
    c21 = 4*a*c13/k
    c22 = 1 + 1/2*c13
    a1 = c09*(c10 - c11)
    a = a1*c15 + 2*c13/c02 + c16

    -- boattail drag
    c_DBTS = c21*(1 - c17 + 2*c13*c19)
    c_DBTT = 4*c13*c13*c22*(1 - c18 + 2*c13*c20)


    ---- SKIN FRICTION DRAG

    -- terms for skin friction drag
    re = u*l_T/nu
    c23 = 1.328/(sqrt re)
    c24 = (1 + 0.12*m*m)**(-0.12)
    c25 = 0.455 / (logBase 10 re)**2.58
    c26 = (1 + 0.21*m*m)**(-0.32)
    c27 = 1 + 1/8/l_N/l_N
    c28 = 1/3 + 1/50/l_N/l_N
    c_FL = c23*c24
    c_FT = c25*c26
    sw_cyl = pi * (l_T - l_N)
    sw_nose = pi/2*l_N*c27*(1 + c28*hsp)
    sw = sw_cyl + sw_nose
    c_F = case bl of
            FullyTurbulent -> c_FT
            LaminarOnNose ->
              let f = l_N / l_T
              in f*c_FL + (f - 1)*c_FT

    -- skin friction drag
    c_DSF = 4/pi*c_F*sw


    ---- BASE DRAG (BLUNT BASE)

    -- terms for base drag
    c29 = 1 + 1/4*m*m*(1-d_B)
    c30 = 1 - exp (-l_CYL)
    c31 = 2*d_B*d_B/gamma/m/m
    pbi = (1 + 0.09*m*m*c30)*c29

    -- base drag
    c_DB = c31*(1 - pbi)
  

  in McOut{..}
