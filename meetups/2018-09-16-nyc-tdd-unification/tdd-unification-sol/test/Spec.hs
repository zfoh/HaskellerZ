
import Lib
import Control.Monad
import Hedgehog


main :: IO ()
main =
  void $ checkParallel Lib.allTests
  -- recheck (Size 12) (Seed 10287130471727777850 4183678970034932545) prop_mgu_unifiable_unifies
  -- recheck (Size 18) (Seed 880072205916096000 3549529521053128539) prop_mgu_valid
  -- recheck (Size 30) (Seed 11685782471373585527 7060805521631146313) prop_mgu_unifiable_unifies
  -- recheck (Size 27) (Seed 13702547476763580423 10682284304597811837) prop_mgu_valid
