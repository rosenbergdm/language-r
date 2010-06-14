{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Judy as J
import qualified Data.Map as Map
import Data.StateRef
import Control.Monad


instance Show (IO (J.JudyL Int)) where
  show a = "JudyMap -> J.JudyL Int: representation repressed"

data SExp = SExp
  { sexp_type :: Int
  , sexp_name :: String
  , dummy_refs :: [Int]
  } deriving (Eq, Show)

data AstGraph = AstGraph 
  { ast_table :: J.JudyL Int
  , ast_map   :: Ref IO (Map.Map Int SExp)
  } deriving Show

data HNil = HNil

instance Show (Ref IO (Map.Map Int SExp)) where
  show a = "IntMap -> Ref IO (Map.Map Int SExp): representation repressed"


-- newGraph :: AstGraph
newGraph =  do
  jnew <- J.new :: IO (J.JudyL Int)
  let mnew = Map.empty :: Map.Map Int SExp
  mnewref <- newRef mnew
  let ngraph = AstGraph jnew mnewref
  return ngraph

class SyntaxTree a where
  addNode    :: a -> b -> Maybe Integer
  lookupNode :: a -> Integer -> Either Integer HNil
  numNodes   :: a -> Integer
  delNode    :: a -> Integer -> Maybe Bool
{-  
instance SyntaxTree AstGraph where
  addNode ag b = do
-}  
  



{-
main = do
   g  <- getStdGen
   rs <- randoms g
   j  <- J.new :: IO (J.JudyL Int)
   forM_ (take 1000000 rs) $ \n ->
       J.insert n 1 j
   v  <- J.findMax j
   case v of
        Nothing    -> print "Done."
        Just (k,_) -> print k
-}
