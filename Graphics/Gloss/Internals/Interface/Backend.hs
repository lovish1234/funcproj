{-# LANGUAGE CPP #-}

-- Import window managed backend specific modules. 
-- We need to use #ifdef here because if the backend library hasn't been installed
-- then we won't be able to build it, so it can't be in the import list.
module Graphics.Gloss.Internals.Interface.Backend
        ( module Graphics.Gloss.Internals.Interface.Backend.Types
        , module Graphics.Gloss.Internals.Interface.Backend.GLUT
        , defaultBackendState)
where

import Graphics.Gloss.Internals.Interface.Backend.Types
import Graphics.Gloss.Internals.Interface.Backend.GLUT
defaultBackendState :: GLUTState
defaultBackendState = initBackendState
