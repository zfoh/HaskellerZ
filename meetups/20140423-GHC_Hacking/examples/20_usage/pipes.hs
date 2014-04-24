import Pipes
import Pipes.Prelude

main = runEffect $ stdinLn >-> stdoutLn
