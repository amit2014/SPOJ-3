module Main where

import Data.List

main = do t' <- getLine
          let t = read t'
          iter 1 t

iter cas t
  | cas > t
  = return ()
  | otherwise
  = do l <- getLine
       let [n,m] = map read $ words l
       pairs <- retrieve m
       let pairs' = repair pairs
           costI :: Integer
           costI = cost pairs
           costII :: Integer
           costII = cost pairs'
       putStrLn $ "Case #" ++ show cas ++ ": " ++ show (mod (costI-costII) p)
       iter (cas+1) t

cost :: [(Integer,Integer,Integer)]->Integer
cost [] = 0
cost ((x,y,c):xs) = cost xs - c * div ((y-x-1)*(y-x)) 2

retrieve :: Integer -> IO [(Integer,Integer,Integer)]

retrieve 0 = return []
retrieve k = do l <- getLine
                xs <- retrieve (k-1)
                let [x,y,c] = map read $ words l
                return ((x,y,c):xs)

p = 1000002013

repair :: [(Integer, Integer,   Integer)]->[(Integer,Integer,Integer)]
repair xs = extract $ myNub $ sort $ concat $ map (\(x,y,c)->[(x,-c),(y,c)]) xs

myNub ((xI,0):xs)
  = myNub xs
myNub ((xI,vI):(xII,vII):xs)
  | xI==xII
  = myNub ((xI,vI+vII):xs)
  | otherwise
  = (xI,vI):myNub ((xII,vII):xs)
myNub x = x

extract :: [(Integer,Integer)]->[(Integer,Integer,Integer)]
extract ((x,xc):bs)
  | (sum $ map snd bs'') == 0
  = (x,y,c) : extract bs''
  where (y,c, yc,bs') = extractOne xc bs
        bs'' = putback (x,xc+c) $ putback (y,yc-c) bs'

extract []
  = []

putback (_,0) xs = xs
putback k xs = sort (k:xs)

extractOne k ((x,c):bs)
  | k+c==0
  = (x,c,c,bs)
  | k+c<0
  = (x',min (-k) c', c'',(x,c):bs')
  where (x',c',c'',bs') = extractOne (k+c) bs

