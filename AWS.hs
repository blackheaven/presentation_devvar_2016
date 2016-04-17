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
type Region = Point -> Bool
inRegion :: Point -> Region -> Bool
p `inRegion` r = r p

-- 7.1.3 Primitive operations
type Radius = Float

circle :: Radius -> Region
circle r = \p -> sqrDist p < r*r

halfPlane :: Point -> Point -> Region
halfPlane a b = \p -> zcross (a - p) (b - a) > 0
  where zcross (Pt x y) (Pt u v) = x*v - y*u

-- 7.1.4 Regions operations
outside :: Region -> Region
outside r = \p -> not (r p)

(/\), (\/) :: Region -> Region -> Region
r1 /\ r2 = \p -> (r1 p && r2 p)
r1 \/ r2 = \p -> (r1 p || r2 p)

intersect, union :: [Region] -> Region
intersect = foldr1 (/\)
union = foldr1 (\/)

at :: Region -> Point -> Region
r `at` p0 = \p -> r (p - p0)

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

main :: IO ()
main = putStr $ unlines (zipWith (\t r -> "Time " ++ show t ++ ":\n" ++ r ++ "\n")
                                 (iterate (timeInterval+) 0)
                                 (program trackingData))
  -- where sim = foldr (\rep -> appendChan stdout rep exit . appendFile "trackFile" rep exit) done

visualize :: Region -> String
visualize r = unlines [[if r (Pt x y) then '.' else ' ' | x <- [1,2..20]] | y <- [1,2..20]]
