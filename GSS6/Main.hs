module Main where 

import Data.Monoid
import Data.Function

data BTree a = Leaf !a | Node !a [BTree a]
data Relation = Disjoint | Contain | Other

-- infixl 6 <>
-- a <> b = mappend a b

value (Leaf v) = v
value (Node v _) = v

sumUp [a,b,c,d]
  = sumUp [a,b] ++ sumUp [c,d]
sumUp subs
  = [Node (mconcat $ fmap value subs) subs]

insert check t k v
  = reg $ itr t k
  where
    reg [root] = root
    reg subs = head $ sumUp subs
    itr cur@(Node sum subs) k
      = sumUp $ subsL++nxt'++subsR
      where
        (k', (subsL,nxt,subsR)) = check cur k
        nxt' = itr nxt k'
    itr x _
      = [Leaf v, x]

update check t k f
  = head $ itr t k
  where
    itr cur@(Node sum subs) k
      = sumUp $ subsL++nxt'++subsR
      where
        (k', (subsL,nxt,subsR)) = check cur k
        nxt' = itr nxt k'
    itr (Leaf v) _
      = [Leaf $ f v]

query check t range
  = itr t range
  where
    itr t range
      = case rel of
          Disjoint -> mempty
          Contain -> value t
          Other -> mconcat $ fmap (uncurry itr) $ zip subs ranges
      where 
        (rel,ranges) = check t range
        Node _ subs = t

data Interval = Interval { size :: !Int, lmax :: !Int, rmax :: !Int, gmax :: !Int, gsum :: !Int }

singleton k = Interval 1 k k k k

infixl 1 $$
f $$ x = f x

instance Monoid Interval where
  mempty = Interval 0 0 0 0 0
  mappend l r | size l == 0
              = r
              | size r == 0
              = l
              | otherwise
              = Interval $$ size l + size r
                         $$ max (lmax l) (gsum l + lmax r)
                         $$ max (rmax r) (gsum r + rmax l)
                         $$ maximum [gmax l, gmax r, rmax l + lmax r]
                         $$ gsum l + gsum r

kth (Node _ subs) k
  = itr [] subs 0
  where
    itr subsL (x:xs) offset
      | offset' >= k
      = (k-offset, (subsL, x, xs))
      | otherwise
      = itr (subsL++[x]) xs offset'
      where
        offset' = offset + (size $ value x)

inRange cur @ ~(Node _ subs) (l,r)
  = (rel, ranges)
  where
    int = value cur
    rel | l<=1 && size int <= r
        = Contain
        | size int < l || r < 1
        = Disjoint
        | otherwise
        = Other
    ranges = scanl (\(x,y) z->(x-z,y-z)) (l,r) $ fmap (size.value) subs
    
alwaysFirst (Node _ subs) _ = (fix id,([],head subs,tail subs))

main = do
         _ <- getLine
         lineII <- getLine
         let ps = map (singleton.read) $ words lineII
             tree = foldr (\p t -> insert alwaysFirst t 0 p) (Leaf $ singleton 0) ps
         _ <- getLine
         queryLines <- getContents
         let answers = flip itr tree $ map ((\s->(head $ head s,map read $ tail s)).words) $ lines $ filter (/='\r') queryLines
         putStr $ unlines $ map show answers
     where
       itr (op:ops) tree
         = case op of
             ('I', [k,v]) -> itr ops $ insert kth tree k (singleton v)
             ('D', [k])   -> itr ops $ update kth tree k $ const mempty
             ('R', [k,v]) -> itr ops $ update kth tree k $ const $ singleton v
             ('Q', [l,r]) -> gmax (query inRange tree (l,r)) : itr ops tree
             _            -> itr ops tree
       itr _ _
         = []
