
import qualified Data.Set as Set


data Scale = EdxScale [NoteData] | CPSScale [NoteData] deriving (Show, Eq)


--             Bounding: Period   Ratio        Bounding: Period   Ratio
data NoteData = EdxNote Rational Rational | CPSNote (Set.Set Rational) Rational Rational deriving (Show, Eq)

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

addDegrees:: Scale -> [(NoteData,Int)]
addDegrees (EdxScale scale) = zip scale [0..]
addDegrees (CPSScale scale) = zip scale [0..]

scaleToNotes :: Scale -> [Note]
scaleToNotes scale = map (getNote scale) $ map (snd) $ addDegrees scale

-- solve this!
-- chordToNotes :: Scale -> [Degree] -> [Float]
-- chordToNotes scale degs = map (getNote scale) degs

-- here it is implied that the convertion from cents to midi interval takes place, as in: 1200 cents to 12.0 midi interval
scaleToPunctualCode :: Scale -> String
scaleToPunctualCode scale = 
  let midis = scaleToNotes scale
  in "notes << [" ++ (show midis) ++ "];"

scaleToTidalCode :: Scale -> String
scaleToTidalCode scale = 
    let midis = scaleToNotes scale
    in "here goes some Tidal code"


-- here we could create a function that provides multiple lines of code with multiple chords
-- chordsToPunctualCode
-- chordsToTidalCode






----- to build the CPS and MOS

type Period = Rational 
type SetSize = Int 
type Factor = Rational 
type Generator = Int 

-- makeCPS :: Period SetSize [Factor]

-- makeMOS :: Period SetSize [Generator]


