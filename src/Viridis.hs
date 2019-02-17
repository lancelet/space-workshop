{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Viridis
  ( viridis
  ) where

import           Data.Colour.SRGB             (Colour, sRGB24)
import           Data.Maybe                   (fromJust)
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Vector.Unboxed.Sized    (Unbox, Vector)
import qualified Data.Vector.Unboxed.Sized    as V
import           Data.Word                    (Word8)
import           GHC.TypeNats                 (KnownNat)

-- Viridis color map from Python; ORIGINAL is licensed as CC0.
-- https://github.com/BIDS/colormap/blob/84cb3771a38dfe3d3977677df31af55f4ab7985e/colormaps.py


data URGB = URGB {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Word8

derivingUnbox "URGB"
  [t| URGB -> (Word8, Word8, Word8) |]
  [| \(URGB r g b) -> (r, g, b) |]
  [| \(r, g, b) -> URGB r g b |]


fromListUnsafe :: (Unbox a, KnownNat n) => [a] -> Vector n a
fromListUnsafe = fromJust . V.fromListN

viridisTable :: Vector 255 URGB
viridisTable
  = fromListUnsafe
    [ URGB  68   1  84
    , URGB  68   2  86
    , URGB  69   4  87
    , URGB  69   5  89
    , URGB  70   7  90
    , URGB  70   8  92
    , URGB  70  10  93
    , URGB  70  11  94
    , URGB  71  13  96
    , URGB  71  14  97
    , URGB  71  16  99
    , URGB  71  17 100
    , URGB  71  19 101
    , URGB  72  20 103
    , URGB  72  22 104
    , URGB  72  23 105
    , URGB  72  24 106
    , URGB  72  26 108
    , URGB  72  27 109
    , URGB  72  28 110
    , URGB  72  29 111
    , URGB  72  31 112
    , URGB  72  32 113
    , URGB  72  33 115
    , URGB  72  35 116
    , URGB  72  36 117
    , URGB  72  37 118
    , URGB  72  38 119
    , URGB  72  40 120
    , URGB  72  41 121
    , URGB  71  42 122
    , URGB  71  44 122
    , URGB  71  45 123
    , URGB  71  46 124
    , URGB  71  47 125
    , URGB  70  48 126
    , URGB  70  50 126
    , URGB  70  51 127
    , URGB  70  52 128
    , URGB  69  53 129
    , URGB  69  55 129
    , URGB  69  56 130
    , URGB  68  57 131
    , URGB  68  58 131
    , URGB  68  59 132
    , URGB  67  61 132
    , URGB  67  62 133
    , URGB  66  63 133
    , URGB  66  64 134
    , URGB  66  65 134
    , URGB  65  66 135
    , URGB  65  68 135
    , URGB  64  69 136
    , URGB  64  70 136
    , URGB  63  71 136
    , URGB  63  72 137
    , URGB  62  73 137
    , URGB  62  74 137
    , URGB  62  76 138
    , URGB  61  77 138
    , URGB  61  78 138
    , URGB  60  79 138
    , URGB  60  80 139
    , URGB  59  81 139
    , URGB  59  82 139
    , URGB  58  83 139
    , URGB  58  84 140
    , URGB  57  85 140
    , URGB  57  86 140
    , URGB  56  88 140
    , URGB  56  89 140
    , URGB  55  90 140
    , URGB  55  91 141
    , URGB  54  92 141
    , URGB  54  93 141
    , URGB  53  94 141
    , URGB  53  95 141
    , URGB  52  96 141
    , URGB  52  97 141
    , URGB  51  98 141
    , URGB  51  99 141
    , URGB  50 100 142
    , URGB  50 101 142
    , URGB  49 102 142
    , URGB  49 103 142
    , URGB  49 104 142
    , URGB  48 105 142
    , URGB  48 106 142
    , URGB  47 107 142
    , URGB  47 108 142
    , URGB  46 109 142
    , URGB  46 110 142
    , URGB  46 111 142
    , URGB  45 112 142
    , URGB  45 113 142
    , URGB  44 113 142
    , URGB  44 114 142
    , URGB  44 115 142
    , URGB  43 116 142
    , URGB  43 117 142
    , URGB  42 118 142
    , URGB  42 119 142
    , URGB  42 120 142
    , URGB  41 121 142
    , URGB  41 122 142
    , URGB  41 123 142
    , URGB  40 124 142
    , URGB  40 125 142
    , URGB  39 126 142
    , URGB  39 127 142
    , URGB  39 128 142
    , URGB  38 129 142
    , URGB  38 130 142
    , URGB  38 130 142
    , URGB  37 131 142
    , URGB  37 132 142
    , URGB  37 133 142
    , URGB  36 134 142
    , URGB  36 135 142
    , URGB  35 136 142
    , URGB  35 137 142
    , URGB  35 138 141
    , URGB  34 139 141
    , URGB  34 140 141
    , URGB  34 141 141
    , URGB  33 142 141
    , URGB  33 143 141
    , URGB  33 144 141
    , URGB  33 145 140
    , URGB  32 146 140
    , URGB  32 146 140
    , URGB  32 147 140
    , URGB  31 148 140
    , URGB  31 149 139
    , URGB  31 150 139
    , URGB  31 151 139
    , URGB  31 152 139
    , URGB  31 153 138
    , URGB  31 154 138
    , URGB  30 155 138
    , URGB  30 156 137
    , URGB  30 157 137
    , URGB  31 158 137
    , URGB  31 159 136
    , URGB  31 160 136
    , URGB  31 161 136
    , URGB  31 161 135
    , URGB  31 162 135
    , URGB  32 163 134
    , URGB  32 164 134
    , URGB  33 165 133
    , URGB  33 166 133
    , URGB  34 167 133
    , URGB  34 168 132
    , URGB  35 169 131
    , URGB  36 170 131
    , URGB  37 171 130
    , URGB  37 172 130
    , URGB  38 173 129
    , URGB  39 173 129
    , URGB  40 174 128
    , URGB  41 175 127
    , URGB  42 176 127
    , URGB  44 177 126
    , URGB  45 178 125
    , URGB  46 179 124
    , URGB  47 180 124
    , URGB  49 181 123
    , URGB  50 182 122
    , URGB  52 182 121
    , URGB  53 183 121
    , URGB  55 184 120
    , URGB  56 185 119
    , URGB  58 186 118
    , URGB  59 187 117
    , URGB  61 188 116
    , URGB  63 188 115
    , URGB  64 189 114
    , URGB  66 190 113
    , URGB  68 191 112
    , URGB  70 192 111
    , URGB  72 193 110
    , URGB  74 193 109
    , URGB  76 194 108
    , URGB  78 195 107
    , URGB  80 196 106
    , URGB  82 197 105
    , URGB  84 197 104
    , URGB  86 198 103
    , URGB  88 199 101
    , URGB  90 200 100
    , URGB  92 200  99
    , URGB  94 201  98
    , URGB  96 202  96
    , URGB  99 203  95
    , URGB 101 203  94
    , URGB 103 204  92
    , URGB 105 205  91
    , URGB 108 205  90
    , URGB 110 206  88
    , URGB 112 207  87
    , URGB 115 208  86
    , URGB 117 208  84
    , URGB 119 209  83
    , URGB 122 209  81
    , URGB 124 210  80
    , URGB 127 211  78
    , URGB 129 211  77
    , URGB 132 212  75
    , URGB 134 213  73
    , URGB 137 213  72
    , URGB 139 214  70
    , URGB 142 214  69
    , URGB 144 215  67
    , URGB 147 215  65
    , URGB 149 216  64
    , URGB 152 216  62
    , URGB 155 217  60
    , URGB 157 217  59
    , URGB 160 218  57
    , URGB 162 218  55
    , URGB 165 219  54
    , URGB 168 219  52
    , URGB 170 220  50
    , URGB 173 220  48
    , URGB 176 221  47
    , URGB 178 221  45
    , URGB 181 222  43
    , URGB 184 222  41
    , URGB 186 222  40
    , URGB 189 223  38
    , URGB 192 223  37
    , URGB 194 223  35
    , URGB 197 224  33
    , URGB 200 224  32
    , URGB 202 225  31
    , URGB 205 225  29
    , URGB 208 225  28
    , URGB 210 226  27
    , URGB 213 226  26
    , URGB 216 226  25
    , URGB 218 227  25
    , URGB 221 227  24
    , URGB 223 227  24
    , URGB 226 228  24
    , URGB 229 228  25
    , URGB 231 228  25
    , URGB 234 229  26
    , URGB 236 229  27
    , URGB 239 229  28
    , URGB 241 229  29
    , URGB 244 230  30
    , URGB 246 230  32
    , URGB 248 230  33
    , URGB 251 231  35
    , URGB 253 231  37 ]


-- | Takes a value from 0.0 to 1.0 and returns a color lookup from the
--   Viridis color table.
viridis :: forall a. (Floating a, RealFrac a) => a -> Colour Double
viridis x =
  let
    URGB r g b = V.unsafeIndex viridisTable index
  in
    sRGB24 r g b
  where
    index :: Int
    index = floor (255.0 * clamp01 x)

    clamp01 :: a -> a
    clamp01 q = if q > 1.0
                then 1.0
                else if q < 0.0
                     then 0.0
                     else q
