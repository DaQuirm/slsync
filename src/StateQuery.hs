module StateQuery where

import           Query ( Query )
import           State ( State )

data ServiceError = SomethingWentWrong
  deriving ( Show )

type StateQuery a b = Query State ServiceError a b
