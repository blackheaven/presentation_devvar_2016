import Data.List(foldl')
import Data.Bits(xor)
-- http://haskell.cs.yale.edu/wp-content/uploads/2011/03/HaskellVsAda-NSWC.pdf
-- http://www.cs.yale.edu/publications/techreports/tr1031.pdf
-- 7.1.1 Points
data Vector a = Pt a a deriving (Eq, Show)
type Point = Vector Float

origin :: Point
origin = Pt 0 0

instance Num a => Num (Vector a) where
    Pt x y + Pt u v = Pt (x+u) (y+v)
    negate (Pt x y) = Pt (-x)  (-y)
    Pt x y - Pt u v = Pt (x-u) (y-v)

sqrDist :: Num a => Vector a -> a
sqrDist (Pt x y) = x*x + y*y
-- 7.1.2 Regions
data Region = Circle Radius
            | HalfPlane Point Point
            | Outside Region
            | Intersect Region Region
            | Union Region Region
            | Intersects [Region]
            | Unions [Region]
            | At Region Point
            deriving (Show)

inRegion :: Point -> Region -> Bool
inRegion p r = case r of
                 Circle r      -> sqrDist p < r*r
                 HalfPlane a b -> let zcross (Pt x y) (Pt u v) = x*v - y*u in zcross (a - p) (b - a) > 0
                 Outside r'    -> not (inRegion p r')
                 Intersect a b -> (inRegion p a) && (inRegion p b)
                 Union a b     -> (inRegion p a) || (inRegion p b)
                 Intersects xs -> foldr1 (&&) (map (inRegion p) xs)
                 Unions xs     -> foldr1 (||) (map (inRegion p) xs)
                 At r' p'      -> inRegion (p - p') r'

-- 7.1.3 Primitive operations
type Radius = Float

circle :: Radius -> Region
circle = Circle

halfPlane :: Point -> Point -> Region
halfPlane = HalfPlane

-- 7.1.4 Regions operations
outside :: Region -> Region
outside = Outside

(/\), (\/) :: Region -> Region -> Region
(/\) = Intersect
(\/) = Union

intersect, union :: [Region] -> Region
intersect = Intersects
union = Unions

at :: Region -> Point -> Region
at = At

-- 7.1.5 Derived Regions
annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

convexPoly :: [Point] -> Region
convexPoly (v:vs) = intersect (zipWith halfPlane ([v]++vs) (vs++[v]))

type Angle = Float

segment :: Angle -> Angle -> Region
segment l u = halfPlane (radial u) origin /\ halfPlane (radial l) origin
  where radial a = Pt (cos theta) (sin theta)
                   where theta = a * (pi / 180)

pie :: Radius -> Angle -> Angle -> Region
pie r l u = segment l u /\ circle r

-- 7.2 Object tracking
-- 7.2.1 Sensor/Track File data
type TrackingData = (Point, [Point], [Point])
-- 7.2.2 Dispositions
type Disposition = ([NamedRegion], [NamedObject])
type NamedObject = (String, Point)
type NamedRegion = (String, Region)

makeDisposition :: TrackingData -> Disposition
makeDisposition (pown, pslaves, pobjs)
  = ([("weapon doctrine", weaponDoctrine `at` pown),
      ("engageability zone", engageability `at` pown)]
    ++ zipWith (\(name, r) p -> (name, r `at` p)) slaves pslaves
    ++ tightZones,
    zip objectIds pobjs)

--7.2.3
assess :: Disposition -> String
assess (regs, objs) = unlines [r | obj <- objs, r <- objectReport obj regs]

-- 7.2.4 Report generation
objectReport :: NamedObject -> [NamedRegion] -> [String]
objectReport (what, p) rs
  = [what ++ " : " ++ showP p] ++
    [rep | reg <- rs, rep <- regionReport p reg]
  where showP (Pt x y) = "(" ++ show x ++ "," ++ show y ++ ")"

regionReport :: Point -> NamedRegion -> [String]
regionReport p (region, r) = ["-- In " ++ region | p `inRegion` r]

-- 7.3 Complete program
program :: [TrackingData] -> [String]
program = map (assess . makeDisposition)

-- 7.4 Sample
weaponDoctrine = pie 59 (-120) 120
engageability = annulus 22 44
slaves = [("carrier slave doctrine", circle 40)]
tightZones = [("tight zone",
              convexPoly [Pt 0 5, Pt 118 32, Pt 118 62, Pt 0 25] \/
              convexPoly [Pt 118 32, Pt 259 5, Pt 259 25, Pt 118 62])]
objectIds = ["commercial aircraft", "hostil craft"]
trackingData :: [TrackingData]
trackingData = [(Pt 113 64, [Pt 180 140], [Pt 38 25, Pt 258 183]),
                (Pt 123 64, [Pt 180 130], [Pt 58 30, Pt 239 164]),
                (Pt 133 64, [Pt 180 119], [Pt 100 43, Pt 210 136]),
                (Pt 163 64, [Pt 180 81],  [Pt 159 36, Pt 148 73]),
                (Pt 192 64, [Pt 180 60],  [Pt 198 27, Pt 110 37])]

timeInterval :: Float
timeInterval = 20

-- demo :: Dialogue
-- demo = display (zipWith (\t r -> "Time " ++ show t ++ ":\n" ++ r ++ "\n")
--                         (iterate (timeInterval+) 0)
--                         (program trackingData))
--   where display = foldr (\rep -> appendChan stdout rep exit . appendFile "trackFile" rep exit) done

-- main :: IO ()
-- main = putStr $ unlines (zipWith (\t r -> "Time " ++ show t ++ ":\n" ++ r ++ "\n")
--                                  (iterate (timeInterval+) 0)
--                                  (program trackingData))

main :: IO ()
main = writeFile "/tmp/out.svg" $ "<svg height=\"" ++ show imgSize ++ "\" width=\"" ++ show imgSize ++ "\">" ++ (concatMap visualize (map snd (slaves ++ tightZones))) ++ "</svg>"

imgSize :: Float
imgSize = 300

visualize :: Region -> String
visualize r = cs ++ "\n<rect x=\"0\" y=\"0\" width=\"" ++ show imgSize ++ "\" height=\"" ++ show imgSize ++ "\" fill=\"red\" clip-path=\"url(#" ++ ci ++ ")\" />"
  where (ci, _, cs) = toClip r

toClip :: Region -> (String, String, String)
toClip r = case r of
             Circle r'      -> clipize "" "" ("<circle cx=\"0\" cy=\"0\" r=\"" ++ show r' ++ "\" />")
             HalfPlane a b  -> let (angle, x, y) = computeHp a b in ((xa, ya), (xb, yb)) = (a, b) in ("", "", "") -- let zcross (Pt x y) (Pt u v) = x*v - y*u in zcross (a - p) (b - a) > 0
             Outside r'     -> ("", "", "") -- not (inRegion p r')
             Intersect a b  -> let (cha, _, ca) = toClip a in let (_, ihb, cb) = toClip b in clipize (ca ++ cb) (" clip-path=\"url(#" ++ cha ++ ")\"") (use ihb)
             Union a b      -> let (_, iha, ca) = toClip a in let (_, ihb, cb) = toClip b in clipize (ca ++ cb) "" (use iha ++ use ihb)
             Intersects xs  -> foldr1 (\(cha, _, ca) (_, ihb, cb) -> clipize (ca ++ cb) (" clip-path=\"url(#" ++ cha ++ ")\"") (use ihb)) $ map toClip xs
             Unions xs      -> foldr1 (\(_, iha, ca) (_, ihb, cb) -> clipize (ca ++ cb) "" (use iha ++ use ihb)) $ map toClip xs
             At r' (Pt x y) -> let (ci, _, cs) = toClip r' in clipize cs "" ("<g transform=\"translate(" ++ show x ++ " " ++ show y ++ ")\">" ++ use ci ++ "</g>")
  where clipize o h e = let ih = hash e in let ic = "<g id=\"" ++ ih ++ "\">" ++ e ++ "</g>" in let ch = hash ic in (ch, ih, o ++ "<clipPath id=\"" ++ ch ++ "\"" ++ h ++ ">" ++ ic ++ "</clipPath>\n")
        hash = show . foldl' (\h c -> 33*h `xor` fromEnum c) 5381
        use i = "<use x=\"0\" y=\"0\" width=\"" ++ show imgSize ++ "\" height=\"" ++ show imgSize ++ "\" xlink:href=\"#"++ i ++"\" />\n"
{-
middle = (xa+xb/2, ya+yb/2)
rotate = (ya-yb)/(xa-xb) 45%360 = 45°
-}
{-
<clipPath id="clip1">
    <polygon id="clip1Shape" points="100,10 40,180 190,60 10,60 160,180 100,10" stroke="blue" />        
</clipPath>
 
<clipPath id="clip2">
    <circle id="clip2Shape" cx="100" cy="100" r="65" />
</clipPath>
 
<!-- Intersection -->
<clipPath id="clipIntersection" clip-path="url(#clip1)">P1C2
    <use x="0" y="0" width="200" height="200" xlink:href="#clip2Shape" />
</clipPath>
 
<!-- Union -->
<clipPath id="clipUnion">C12
    <use x="0" y="0" width="200" height="200" xlink:href="#clip1Shape" />
    <use x="0" y="0" width="200" height="200" xlink:href="#clip2Shape" />
</clipPath>
....
<!-- Clip 1 -->
<rect x="10" y="10" width="180" height="180" fill="url(#myFillGrad)" 
    clip-path="url(#clip1)" />
<!-- Clip 2 -->
<rect x="10" y="10" width="180" height="180" fill="url(#myFillGrad)" 
    clip-path="url(#clip2)" transform="translate(200)"/>
<!-- Intersection -->
<rect x="10" y="10" width="180" height="180" fill="url(#myFillGrad)" 
    clip-path="url(#clipIntersection)" transform="translate(400)" />
<!-- Union -->
<rect x="10" y="10" width="180" height="180" fill="url(#myFillGrad)" 
    clip-path="url(#clipUnion)" transform="translate(600)" />-}
