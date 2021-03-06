import qualified Data.List as List
import qualified Data.Set as Set
import Data.List.HT (tails)

data Scale = EdxScale [NoteData] | CPSScale [NoteData] deriving (Show, Eq)


type Period = Double
type SetSize = Int

type Ratio = Double


data NoteData = EdxNote Period Ratio | CPSNote Period Ratio deriving (Show, Eq)
                                                
type Degree = Int  -- esto eventualmente se debe ir

type Note = (Float, Int) -- the float is in cents and the int is the degree

-- (defn ratio->cents [ratio]
--   (-> (Math/log ratio) (/ (Math/log 2)) (* 1200)))
noteDataToCent:: NoteData -> Float  -- taken from above clojure code
noteDataToCent (EdxNote period ratio) = 
    let p = ((log (realToFrac period :: Float)) / (log 2)) * 1200
        r = ((log (realToFrac ratio :: Float)) / (log 2)) * p
    in r

-- perhaps we need this
-- (defn cents->ratio [cents]
--   (-> (/ cents 1200) (* (Math/log 2)) Math/exp))

-- gets a midinote from a scale and a degree 
getNote :: Scale -> Degree -> Note
getNote scale deg =    -- !!!!!!!!!!!!!!!!!!!!!!!!
    let withDegree = head $ filter (\x -> (snd x) == deg) $ addDegrees scale
        cent = noteDataToCent $ fst withDegree -- resolver el como ver el grado con la nota para ocnvertir esa nota a cent
    in (cent,snd withDegree)

addDegrees:: Scale -> [(NoteData,Degree)]
addDegrees (EdxScale scale) = zip scale [0..]
addDegrees (CPSScale scale) = zip scale [0..]

scaleToNotes :: Scale -> [Note]
scaleToNotes scale = map (getNote scale) $ map (snd) $ addDegrees scale

-- solve this!
-- chordToNotes :: Scale -> [Degree] -> [Float]
-- chordToNotes scale degs = map (getNote scale) degs

scaleToMidiInterval:: Scale -> [Float]
scaleToMidiInterval scale = map (\x -> (fst x)* 0.01) $ scaleToNotes scale

scaleToMelody:: Scale -> [Degree] -> [Float]
scaleToMelody scale degs = undefined

scaleToChord:: Scale -> [Degree] -> [Float]
scaleToChord scale degs = undefined

scaleToChords:: Scale -> [[Degree]] -> [Float]
scaleToChords scale degs = undefined

-- here it is implied that the convertion from cents to midi interval takes place, as in: 1200 cents to 12.0 midi interval
scaleToPunctualCode :: Scale -> String
scaleToPunctualCode scale = 
  let midis = scaleToMidiInterval scale
  in "notes << " ++ (show midis) ++ ";"


scaleToTidalCode :: Scale -> String
scaleToTidalCode scale = 
    let midis = scaleToMidiInterval scale
    in "here goes some Tidal code"



----- to build the CPS and MOS

type SetSize = Int 
type Factor = Rational 
type Generator = Int 

-- from the `combinatorics` library
combinatoricTuples :: Int -> [a] -> [[a]]
combinatoricTuples =
   let go r =
         case compare r 0 of
            LT -> const []
            EQ -> const [[]]
            GT -> concatMap (\(y:ys) -> map (y:) (go (r-1) ys)) . init . tails
   in  go

putWithinPeriod :: (Ord a, Fractional a) => a -> a -> a
putWithinPeriod p n
    | (n < 1) = putWithinPeriod p (n*p) 
    | (n >= 1 && n < p) = n 
    | otherwise = putWithinPeriod p (n/p)

makeCPS :: Period -> SetSize -> [Ratio] -> [NoteData]
makeCPS period n xs = 
    let combinations = map product $ combinatoricTuples n xs
        max = maximum combinations
        ratios = (List.sort) $ map (\x -> putWithinPeriod period (x/max)) combinations
    in map (CPSNote 2) ratios 
