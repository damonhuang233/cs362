module HW1 where

--Ayoung Kang, Jianlong Huang, Junze Zhang, Yunqiao Cai

import Data.List (nub,sort)

--Exercise 1

type Bag a = [(a,Int)]

inb :: Eq a => a -> Bag a -> Bool
inb ele bag = elem ele [fst x | x <- bag]

-- a

ins :: Eq a => a -> Bag a -> Bag a
ins ele bag
  | inbag == True = [if fst x == ele then (ele, snd x + 1 ) else x | x <- bag ]
  | inbag == False = (ele,1):bag
  where inbag = inb ele bag

-- b

del :: Eq a => a -> Bag a -> Bag a
del ele bag
  | inbag == True = [x | x <- [if fst x == ele then (ele, snd x - 1 ) else x | x <- bag ] , snd x > 0]
  | inbag == False = bag
  where inbag = inb ele bag

-- c

bag :: Eq a => [a] -> Bag a
bag = foldr ins []

-- d

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((x,n):xs) ys = case lookup x ys of
  Just m  -> n <= m && subbag xs ys
  Nothing -> False

-- e

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag baga bagb = [ if snd x <= snd k then x else k | x <- baga, k <- bagb, fst x == fst k]

-- f

size :: Bag a -> Int
size = sum . map snd

-- Exercise 2

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

-- a

nodes :: Graph -> [Node]
nodes g = norm ( [fst x | x <- g] ++ [snd x | x <- g] )

-- b

suc :: Node -> Graph -> [Node]
suc n g = [snd x | x <- g , fst x == n ]

-- c

detach :: Node -> Graph -> Graph
detach n g = [ x | x <- g, fst x /= n && snd x /= n ]

-- d

cyc :: Int -> Graph
cyc n = [ if x < n then (x,x+1) else (x,1) | x <- [1..n] ]

-- Exercise 3

type Number = Int

type Point = (Number,Number)

type Length = Number

data Shape = Pt Point
            | Circle Point Length
            | Rect Point Length Length
            deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

-- a

width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = 2 * r
width (Rect _ w _) = w

-- b

bbox :: Shape -> BBox
bbox (Pt p) = (p, p)
bbox (Circle p r) = ((fst p - r , snd p - r), (fst p + r , snd p + r))
bbox (Rect p w h) = (p, (fst p + w, snd p + h))

-- c

minX :: Shape -> Number
minX (Pt p) = fst p
minX (Circle p r) = fst p - r
minX (Rect p _ _) = fst p

-- d

addPt :: Point -> Point -> Point
addPt p1 p2 = ((fst p1 + fst p2),(snd p1 + snd p2))

move :: Shape -> Point -> Shape
move (Pt p) p' = Pt (addPt p p')
move (Circle p r) p' = Circle (addPt p p') r
move (Rect p w h) p' = Rect (addPt p p') w h

-- e

moveToX :: Number -> Shape -> Shape
moveToX n (Pt p) = Pt (n, snd p)
moveToX n (Circle p r) = Circle (n+r, snd p) r
moveToX n (Rect p w h) = Rect (n, snd p) w h

alignLeft :: Figure -> Figure
alignLeft f = map (moveToX (minimum [minX x | x <- f] )) f

-- f

pdis :: Floating a => Point -> Point -> a
pdis (x1,y1) (x2,y2) = sqrt(x'*x' + y'*y')
  where x' = fromIntegral x1 - fromIntegral x2
        y' = fromIntegral y1 - fromIntegral y2

inside :: Shape -> Shape -> Bool
inside (Pt p1) (Pt p2) = (fst p1 == fst p2 && snd p1 == snd p2)
inside (Circle _ _) (Pt _) = False
inside (Rect _ _ _) (Pt _) = False
inside (Pt p1) (Circle p2 r) = (pdis p1 p2) <= (fromIntegral r)
inside (Circle p1 r1) (Circle p2 r2) = ((pdis p1 p2)+(fromIntegral r1)) <= (fromIntegral r2)
inside (Rect p1 w h) (Circle p2 r) = ((pdis p1 p2)) <= (fromIntegral r)
                                      && ((pdis p1' p2)) <= (fromIntegral r)
                                      && ((pdis p1'' p2)) <= (fromIntegral r)
                                      && ((pdis p1''' p2)) <= (fromIntegral r)
  where p1' = ((fst p1 + w),(snd p1))
        p1'' = ((fst p1 + w),(snd p1 + h))
        p1''' = ((fst p1),(snd p1 + h))
inside (Pt p1) (Rect p2 w h) = (fst p1 >= fst p2 && fst p1 <= (fst p2 + w))
                                && (snd p1 >= snd p2 && snd p1 <= (snd p2 + h))
inside (Circle p1 r) (Rect p2 w h) = ((fst p1 - r) >= fst p2 && (fst p1 - r) <= (fst p2 + w))
                                      && ((snd p1 - r) >= snd p2 && (snd p1 - r) <= (snd p2 +h))
                                      && ((fst p1 + r) >= fst p2 && (fst p1 + r) <= (fst p2 + w))
                                      && ((snd p1 + r) >= snd p2 && (snd p1 + r) <= (snd p2 +h))
inside (Rect p1 w1 h1) (Rect p2 w2 h2) = (fst p1 >= fst p2 && fst p1 <= (fst p2 + w2))
                                          && (snd p1 >= snd p2 && snd p1 <= (snd p2 + h2))
                                          && ((fst p1 + w1) >= fst p2 && (fst p1 + w1) <= (fst p2 + w2))
                                          && ((snd p1 + h1) >= snd p2 && (snd p1 + h1) <= (snd p2 + h2))
