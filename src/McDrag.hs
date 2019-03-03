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

-- | Speed of sound.
newtype SpeedOfSound a = SpeedOfSound a
  

data BoundaryLayer
  = FullyTurbulent  -- ^ Fully-turbulent boundary layer everywhere.
  | LaminarOnNose   -- ^ Laminar boundary layer only on the nose.
  deriving (Show)


data McParams a
  = McParams
    { d_REF :: !a  -- ^ Reference diameter (m)
    , l_T   :: !a  -- ^ Projectile total length (calibers)
    , l_N   :: !a  -- ^ Nose length (calibers)
    , hsp   :: !a  -- ^ Headshape parameter, R_T/R (dimensionless)
    , l_BT  :: !a  -- ^ Boattail length (calibers)
    , d_B   :: !a  -- ^ Base diameter (calibers)
    , d_M   :: !a  -- ^ Meplat diameter (calibers)
    , d_RB  :: !a  -- ^ Rotating band diameter (calibers)
    , bl    :: !BoundaryLayer
    } deriving (Show)


data McBasicOut a
  = McBasicOut
    { c_D    :: !a  -- ^ Total drag coeff.
    , c_DH   :: !a  -- ^ Head drag coeff.
    , c_DSF  :: !a  -- ^ Skin friction drag ceff.
    , c_DBT  :: !a  -- ^ Boattail drag coeff.
    , c_DB   :: !a  -- ^ Base drag coeff.
    , c_PBI  :: !a  -- ^ Pressure ratio.
    -- , c_PBIA :: !a  -- ^ Alternate pressure ratio (debugging).
    } deriving (Show)


-- | Basic MC DRAG equations.
--
--   This is incomplete because
--
--   - It does not describe blending between:
--     - c_DHS and c_DHT in the transonic region and before c_MC
--     - c_DBTS and c_DBTT in the transonic region and in M < 1
--   - There is no rotating band drag yet.
--   - Boattail drag similarity parameters need to be multiplied.
mcDragBasic
  :: forall a. (Floating a, Ord a)
  => McParams a
  -> Mach a
  -> McBasicOut a
mcDragBasic McParams{..} (Mach m) =
  let
    -- Additional geometric parameters
    l_CYL = l_T - l_N
    
    -- Ratio of specific heats (C_pressure / C_volume)
    --   Approximately 1.4 for air:
    --   http://hyperphysics.phy-astr.gsu.edu/hbase/Kinetic/shegas.html
    gamma = 1.4
    
    m2 = m*m
    chi = (m2 - 1)/(2.4*m2)
    be2 = m2 - 1
    be = sqrt be2

    ---- HEAD DRAG (validated against paper)
  
    -- Head drag fitting constants
    c1 = 0.7156 - 0.5313*hsp + 0.595*hsp*hsp
    c2 = 0.0796 + 0.0779*hsp
    c3 = 1.587 + 0.049*hsp
    c4 = 0.1122 + 0.1658*hsp

    -- thickness ratio
    tau = (1 - d_M) / l_N

    ptp
      | m <= 1    = (1 + 0.2*m2)**3.5
      | otherwise = ((1.2*m2)**3.5)*((6/(7*m2 - 1))**2.5) -- this last 2.5 may be 3.5
    cmep = (1.122*(ptp - 1)*(d_M*d_M))/m2
    cdhm
      | m <= 0.91 = 0
      | m < 1.41  = (0.254 + 2.88*chi)*cmep
      | otherwise = 0.85*cmep
    
    -- critical lower cutoff for subsonic head drag
    xmc = (1 + 0.552*(tau**0.8))**(-0.5)
  
    ssmc = 1 + 0.368*(tau**1.85)
    ze
      | m < ssmc  = sqrt(ssmc*ssmc - 1)
      | otherwise = sqrt(m2 - 1)
    rz2 = 1/(ze*ze)
    cdht
      | m <= xmc          = 0
      | m > xmc && m <= 1 = 0.368*(tau**1.8) + 1.6*tau*chi
      | otherwise         = (c1 - c2*tau*tau)*rz2*((tau*ze)**(c3 + c4*tau))

    c_DH = cdht + cdhm


    ---- BOATTAIL DRAG (validated against paper)

    bp = 0.85/be  -- k in the paper
    tb = (1 - d_B)/(2*l_BT)
    tb23 = 2*tb*tb + tb**3
    fbt = exp(-2*l_BT)
    bbt = 1 - fbt + 2*tb*((fbt*(l_BT + 0.5)) - 0.5)

    aa2 = (5*tau)/(6*be) + (0.5*tau)**2 - (0.7435/m2*(tau*m)**1.6)
    aa1 = (1 - ((0.6*hsp)/m))*aa2
    exl = exp((-1.1952/m)*(l_T - l_N - l_BT))
    xxm = ((2.4*m2*m2 - 4*be2)*(tb*tb))/(2*be2*be2)
    aa = aa1*exl - xxm + (2*tb/be)
    bb = 1/bp  -- 1 / k
    exbt = exp (-bp*l_BT)
    aab = 1 - exbt + (2*tb*(exbt*(l_BT + bb)- bb)) -- part of eq 9

    c_DBT
      | m <= 0.85 = 0
      | m <= 1.00 = 2*tb23*bbt*(1/(0.564 + 1260*chi*chi))
      | m <= 1.10 = 2*tb23*bbt*(1.774 - 9.3*chi)
      | otherwise = 4*aa*tb*aab*bb


    ---- SKIN FRICTION DRAG (validated against paper)
  
    re = 23296.3*m*l_T*(d_REF*1000)
    ret = logBase 10 re
    cft = (0.455/(ret**2.58))*((1 + 0.21*m2)**(-0.32))
    cfl = case bl of
            FullyTurbulent -> cft
            LaminarOnNose -> (1.328/(sqrt re))*((1 + 0.12*m2)**(-0.12))
    swcyl = pi*(l_T - l_N)
    dum = 1 + ((1/3 + (0.02/l_N/l_N))*hsp)
    swn = (pi/2)*l_N*dum*(1 + 1/(8*l_N*l_N))
    sw = swn + swcyl
    cdsfl = (4/pi)*sw*cfl
    cdsft = (4/pi)*sw*cft
    c_DSF = (cdsfl*swn + cdsft*swcyl)/sw


    ---- BASE DRAG (BLUNT BASE) (validated against paper)

    -- from the FORTRAN
    pb2 = if m < 1
          then 1/(1 + 0.1875*m2 + 0.0531*m2*m2)
          else 1/(1 + 0.2477*m2 + 0.0345*m2*m2)
    pb4 = (1 + 0.09*m2*(1 - exp (-l_CYL)))*(1 + 1/4*m2*(1 - d_B))

    -- pressure ratio and base drag
    c_PBI = pb2 * pb4
    c_DB = 2*d_B*d_B/gamma/m2*(1 - c_PBI)

    ---- TOTAL DRAG
    c_D = c_DH + c_DSF {- + C_DBND -} + c_DBT + c_DB
  

  in McBasicOut{..}
