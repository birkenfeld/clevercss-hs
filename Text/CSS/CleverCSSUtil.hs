------------------------------------------------------------------------------------------
-- CleverCSS in Haskell, (c) 2007, 2008 Georg Brandl. Licensed under the BSD license.
--
-- Text.CSS.CleverCSSUtil module: utilities.
------------------------------------------------------------------------------------------ 

module Text.CSS.CleverCSSUtil (
             (+++), (+:+), (~>>), varCount, perhaps,
             Color, HLSColor, colors, reverse_colors,
             hexToColor, brightenColor, darkenColor, modifyChannels,
             unitconv, inrange, readNum, readDim,
             trim, ltrim, rtrim, split, joinStr, joinShow,
             preprocess, hexToString, cssShow, showWithoutPos,
             ratMod, roundRat,
            ) where

import Control.Arrow ((&&&))
import Control.Monad (msum)
import Data.Char
import Data.List hiding (partition)
import Data.Ratio (Ratio, (%), numerator, denominator)
import Numeric (readFloat)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec (choice, count, try, option, GenParser)
import Text.ParserCombinators.Parsec.Error
import qualified Data.Map as Map

-- Monad combinator helpers

type P tok = GenParser tok [Int]

infixl 1 +++, ~>>
infixr 1 +:+
-- return the concatenation of the actions' result lists
{-# INLINE (+++) #-}
(+++) :: P tok [a] -> P tok [a] -> P tok [a]
x +++ y = do { rx <- x; ry <- y; return $ rx ++ ry }
-- return a cons of the actions' results
{-# INLINE (+:+) #-}
(+:+) :: P tok a -> P tok [a] -> P tok [a]
x +:+ y = do { rx <- x; ry <- y; return $ rx:ry }
-- return the result of the first action
{-# INLINE (~>>) #-}
(~>>) :: P tok a -> P tok b -> P tok a
x ~>> y = do { rx <- x; y; return rx }

varCount low high p = choice [try $ count x p | x <- [high,high-1..low]]
perhaps c = option "" (count 1 c)

-- Color types and utilities

type Color = (Int, Int, Int) -- range 0..255
type HLSColor = (Double, Double, Double) -- range 0..1

-- | Convert HLS to RGB.
hls_to_rgb :: HLSColor -> Color
hls_to_rgb (_, l, 0) = (l', l', l') where l' = round (255 * l)
hls_to_rgb (h, l, s) =
  let m2 = if l <= 0.5 then l * (s+1) else l + s - (l*s)
      m1 = 2*l - m2
  in (v m1 m2 (h + 1/3), v m1 m2 h, v m1 m2 (h - 1/3))
  where
    v m1' m2' hue' = round (255 * (v' m1' m2' hue')) where
      v' m1 m2 hue =
        let phue = snd $ properFraction (hue + 2) in
        if      phue < 1/6 then m1 + (m2-m1) * phue * 6
        else if phue < 1/2 then m2
        else if phue < 2/3 then m1 + (m2-m1) * (2/3 - phue) * 6
                           else m1

-- | Convert RGB to HLS.
rgb_to_hls :: Color -> HLSColor
rgb_to_hls (r', g', b') =
  let r    = fromIntegral r' / 255
      g    = fromIntegral g' / 255
      b    = fromIntegral b' / 255
      maxc = max (max r g) b
      minc = min (min r g) b
      mami = maxc - minc
      l    = (minc + maxc) / 2
      s    = if l <= 0.5 then mami / (maxc+minc)
                         else mami / (2-maxc-minc)
      rc   = (maxc-r) / mami
      gc   = (maxc-g) / mami
      bc   = (maxc-b) / mami
      h'   = if      r == maxc then bc - gc
             else if g == maxc then 2 + rc - bc
                               else 4 + gc - rc
      h    = snd $ properFraction (h'/6)
  in if minc == maxc then (0.0, l, 0.0)
     else (h, l, s)


-- | A map of standard color names and their Hex counterparts.
colors = Map.fromList $ map (fst &&& hexToColor . snd) [
   ("aliceblue", "#f0f8ff"),
   ("antiquewhite", "#faebd7"),
   ("aqua", "#00ffff"),
   ("aquamarine", "#7fffd4"),
   ("azure", "#f0ffff"),
   ("beige", "#f5f5dc"),
   ("bisque", "#ffe4c4"),
   ("black", "#000000"),
   ("blanchedalmond", "#ffebcd"),
   ("blue", "#0000ff"),
   ("blueviolet", "#8a2be2"),
   ("brown", "#a52a2a"),
   ("burlywood", "#deb887"),
   ("cadetblue", "#5f9ea0"),
   ("chartreuse", "#7fff00"),
   ("chocolate", "#d2691e"),
   ("coral", "#ff7f50"),
   ("cornflowerblue", "#6495ed"),
   ("cornsilk", "#fff8dc"),
   ("crimson", "#dc143c"),
   ("cyan", "#00ffff"),
   ("darkblue", "#00008b"),
   ("darkcyan", "#008b8b"),
   ("darkgoldenrod", "#b8860b"),
   ("darkgray", "#a9a9a9"),
   ("darkgreen", "#006400"),
   ("darkkhaki", "#bdb76b"),
   ("darkmagenta", "#8b008b"),
   ("darkolivegreen", "#556b2f"),
   ("darkorange", "#ff8c00"),
   ("darkorchid", "#9932cc"),
   ("darkred", "#8b0000"),
   ("darksalmon", "#e9967a"),
   ("darkseagreen", "#8fbc8f"),
   ("darkslateblue", "#483d8b"),
   ("darkslategray", "#2f4f4f"),
   ("darkturquoise", "#00ced1"),
   ("darkviolet", "#9400d3"),
   ("deeppink", "#ff1493"),
   ("deepskyblue", "#00bfff"),
   ("dimgray", "#696969"),
   ("dodgerblue", "#1e90ff"),
   ("firebrick", "#b22222"),
   ("floralwhite", "#fffaf0"),
   ("forestgreen", "#228b22"),
   ("fuchsia", "#ff00ff"),
   ("gainsboro", "#dcdcdc"),
   ("ghostwhite", "#f8f8ff"),
   ("gold", "#ffd700"),
   ("goldenrod", "#daa520"),
   ("gray", "#808080"),
   ("green", "#008000"),
   ("greenyellow", "#adff2f"),
   ("honeydew", "#f0fff0"),
   ("hotpink", "#ff69b4"),
   ("indianred", "#cd5c5c"),
   ("indigo", "#4b0082"),
   ("ivory", "#fffff0"),
   ("khaki", "#f0e68c"),
   ("lavender", "#e6e6fa"),
   ("lavenderblush", "#fff0f5"),
   ("lawngreen", "#7cfc00"),
   ("lemonchiffon", "#fffacd"),
   ("lightblue", "#add8e6"),
   ("lightcoral", "#f08080"),
   ("lightcyan", "#e0ffff"),
   ("lightgoldenrodyellow", "#fafad2"),
   ("lightgreen", "#90ee90"),
   ("lightgrey", "#d3d3d3"),
   ("lightpink", "#ffb6c1"),
   ("lightsalmon", "#ffa07a"),
   ("lightseagreen", "#20b2aa"),
   ("lightskyblue", "#87cefa"),
   ("lightslategray", "#778899"),
   ("lightsteelblue", "#b0c4de"),
   ("lightyellow", "#ffffe0"),
   ("lime", "#00ff00"),
   ("limegreen", "#32cd32"),
   ("linen", "#faf0e6"),
   ("magenta", "#ff00ff"),
   ("maroon", "#800000"),
   ("mediumaquamarine", "#66cdaa"),
   ("mediumblue", "#0000cd"),
   ("mediumorchid", "#ba55d3"),
   ("mediumpurple", "#9370db"),
   ("mediumseagreen", "#3cb371"),
   ("mediumslateblue", "#7b68ee"),
   ("mediumspringgreen", "#00fa9a"),
   ("mediumturquoise", "#48d1cc"),
   ("mediumvioletred", "#c71585"),
   ("midnightblue", "#191970"),
   ("mintcream", "#f5fffa"),
   ("mistyrose", "#ffe4e1"),
   ("moccasin", "#ffe4b5"),
   ("navajowhite", "#ffdead"),
   ("navy", "#000080"),
   ("oldlace", "#fdf5e6"),
   ("olive", "#808000"),
   ("olivedrab", "#6b8e23"),
   ("orange", "#ffa500"),
   ("orangered", "#ff4500"),
   ("orchid", "#da70d6"),
   ("palegoldenrod", "#eee8aa"),
   ("palegreen", "#98fb98"),
   ("paleturquoise", "#afeeee"),
   ("palevioletred", "#db7093"),
   ("papayawhip", "#ffefd5"),
   ("peachpuff", "#ffdab9"),
   ("peru", "#cd853f"),
   ("pink", "#ffc0cb"),
   ("plum", "#dda0dd"),
   ("powderblue", "#b0e0e6"),
   ("purple", "#800080"),
   ("red", "#ff0000"),
   ("rosybrown", "#bc8f8f"),
   ("royalblue", "#4169e1"),
   ("saddlebrown", "#8b4513"),
   ("salmon", "#fa8072"),
   ("sandybrown", "#f4a460"),
   ("seagreen", "#2e8b57"),
   ("seashell", "#fff5ee"),
   ("sienna", "#a0522d"),
   ("silver", "#c0c0c0"),
   ("skyblue", "#87ceeb"),
   ("slateblue", "#6a5acd"),
   ("slategray", "#708090"),
   ("snow", "#fffafa"),
   ("springgreen", "#00ff7f"),
   ("steelblue", "#4682b4"),
   ("tan", "#d2b48c"),
   ("teal", "#008080"),
   ("thistle", "#d8bfd8"),
   ("tomato", "#ff6347"),
   ("turquoise", "#40e0d0"),
   ("violet", "#ee82ee"),
   ("wheat", "#f5deb3"),
   ("white", "#ffffff"),
   ("whitesmoke", "#f5f5f5"),
   ("yellow", "#ffff00"),
   ("yellowgreen", "#9acd32")]

reverse_colors = Map.fromList $ map (snd &&& fst) $ (Map.toList colors)

hexToColor [h1,h2,h3,h4,h5,h6] = (hx [h1,h2], hx [h3,h4], hx [h5,h6])
  where hx x = read ("0x" ++ x) :: Int
hexToColor [h1,h2,h3] = hexToColor [h1,h1,h2,h2,h3,h3]
hexToColor ('#':hs) = hexToColor hs
hexToColor _ = error "invalid hex color string"

brightenColor = modifyColor (\l a -> l*(1+a))
darkenColor = modifyColor (\l a -> l*(1-a))
modifyColor fun col am = hls_to_rgb (h, inrange 0.0 1.0 (fun l am), s)
  where (h, l, s) = rgb_to_hls col

modifyChannels :: (Rational -> Rational -> Rational) -> (Int, Int, Int) ->
                  Rational -> (Int, Int, Int)
modifyChannels op (r, g, b) am = (m r, m g, m b)
  where m x = inrange 0 255 $ floor $ op (fromInteger (toInteger x)) am

inrange low high val = min high (max low val)

-- Unit utilities

units = [[("mm", 1), ("cm", 10), ("in", 254%10), ("pt", 254%720), ("pc", 254%60)],
         [("ms", 1), ("s", 1000)],
         [("Hz", 1), ("kHz", 1000)]]

unitconv :: (Rational, String) -> (Rational, String) -> Maybe (Rational, Rational, String)
unitconv (x, u) (y, v)
  | u == v    = Just (x, y, v)
  | otherwise = msum $ map (ratio u v) units
  where ratio u v list = case (lookup u list, lookup v list) of
          (Just du, Just dv) -> if du < dv then Just (x, dv/du * y, u)
                                           else Just (du/dv * x, y, v)
          _ -> Nothing

readNum x = case readWithSign 1 x of (sign, res) -> sign * fst res
readDim x = case readWithSign 1 x of
              (sign, res) -> (sign * fst res, trim (snd res))

readWithSign _ ('-':num) = readWithSign (-1) num
readWithSign sign num = (sign, head (readFloat num :: [(Rational, String)]))

-- String utilities

ltrim (c:cs) | c `elem` " \t\v\f\r\n" = ltrim cs
ltrim cs = cs

rtrim = reverse . ltrim . reverse

trim = ltrim . rtrim

preprocess :: String -> String
preprocess []        = ['\n']  -- be sure to always have a newline at the end
preprocess ('\t':xs) = ' ':' ':' ':' ':' ':' ':' ':' ' : preprocess xs
preprocess ('\f':xs) = '\n' : preprocess xs
preprocess ('\r':xs) = preprocess xs
preprocess (x:xs)    = x : preprocess xs

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: String -> String -> [String] 
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str in 
    firstline : case remainder of
                  [] -> []
                  x -> if x == delim then [] : []
                       else               split delim (drop (length delim) x)

{-# INLINE joinStr #-}
joinStr d x = concat (intersperse d x)
{-# INLINE joinShow #-} 
joinShow d x = joinStr d (map show x)

hexToString :: String -> Char
hexToString x = toEnum (read $ "0x" ++ x)

cssShow s = '"':cssShow' s where
  cssShow' ('"':cs) = '\\':'"':cssShow' cs
  cssShow' (c:cs) | c `elem` [' '..'~'] = c:cssShow' cs
                  | otherwise = printf "\\%x " (fromEnum c) ++ cssShow' cs
  cssShow' [] = ['"']

-- show a Parsec error without position, but with an additional message
showWithoutPos msg err = msg ++ showErrorMessages "or" "unknown parse error"
                         "expecting" "unexpected" "end of input" (errorMessages err)

-- Ratio arithmetic

ratMod :: Rational -> Rational -> Rational
ratMod x y = (nx `mod` ny) % d where
  dx = denominator x
  dy = denominator y
  d  = lcm dx dy
  nx = numerator x * (d `div` dx)
  ny = numerator y * (d `div` dy)

roundRat :: Rational -> Rational -> Rational
roundRat num places = 
    let exp = round places :: Integer in
    (round (num * (10^exp))) % (10^exp) -- XXX todo
