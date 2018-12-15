module InterPreter.Env where
import Data.Map

type Env = Map String Int

init :: Env
init = fromList []
