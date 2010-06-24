import Control.Monad.State.Lazy

import Data.Unique
import Data.Maybe
import Prelude hiding ( id )

type WorldM = StateT World IO

data World = World { objects :: [ WorldObject ] }
data WorldObject = WorldObject { id :: Unique }

addObject :: WorldObject -> WorldM ()
addObject wo = do
  wst <- get
  put $ World $ wo : ( objects wst )

getObject :: Unique -> WorldM ( Maybe WorldObject )
getObject oid = do
  wst <- get
  return $ listToMaybe $ filter ( \wo -> id wo == oid ) ( objects wst )
