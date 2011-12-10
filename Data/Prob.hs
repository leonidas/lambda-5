module Data.Prob where
import Data.Ratio
import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe

data Prob a = Prob [(a, Rational)] deriving Show

evenDist :: [a] -> Prob a
evenDist ls = Prob $ map (\v-> (v, 1 / len)) ls where
    len = fromIntegral $ length ls

prob :: Prob a -> [(a, Rational)]
prob (Prob a) = a

weighted :: [(a, Rational)] -> Prob a
weighted ls = Prob $ map (\(v,s) -> (v, s / sumprob)) ls
    where sumprob = sum $ map snd ls

normalize :: Eq a => Prob a -> Prob a
normalize (Prob a) = Prob $ go a where
    go []     = []
    go ((v,p):as) = (v,p') : go withoutV where
        (withV,withoutV) = partition ((==v).fst) as
        p' = p + (sum $ map snd withV)

probOf :: Eq a => a -> Prob a -> Rational
probOf a = fromMaybe 0 . lookup a . prob . normalize

probThat :: (a -> Bool) -> Prob a -> Rational
probThat f (Prob a) = sum $ map snd $ filter (f.fst) a

instance Functor Prob where
    fmap f (Prob ls) = Prob $ map (\(v,p) -> (f v, p)) ls

instance Applicative Prob where
    pure a = Prob [(a, 1)]
    (<*>) (Prob fs) (Prob ps) = Prob $ g <$> fs <*> ps where
        g (f, fp) (p, pp) = (f p, fp*pp)

instance Monad Prob where
    return = pure

    (Prob as) >>= f = Prob $ concatMap g as where
        g (v, p) = map h bs where
            h (v', p') = (v', p*p')
            bs = prob $ f v
