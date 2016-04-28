import GHC.Stack

f :: HasCallStack => Int -> IO ()
f _ = print $ getCallStack callStack

main = f 23
