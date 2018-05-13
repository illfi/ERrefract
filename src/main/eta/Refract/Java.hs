module Refract.Java where

import qualified Control.Concurrent.MVar as M
import           Control.Monad
import Refract.Event
import Refract.Bus as B

data {-# CLASS "fi.ill.event.Bus" #-} Bus = Bus (Object# Bus)
-- TODO: Move everything to java instance?

foreign export java "@static fi.ill.event.Bus.createBlankBus" createBlankBus
    :: Event e => e -> IO ( M.MVar (Bus e f) )

foreign export java "@static fi.ill.event.Bus.fire" fire
    :: (Event a) => M.MVar (Bus a f) -> a -> IO ()

foreign export java "@static fi.ill.event.Bus.associate" associate
    :: Int -> (a -> IO ()) -> M.MVar (Bus a f) -> IO ()

foreign export java "@static fi.ill.event.Bus.easyAssociate" associate'
    :: (a -> IO ()) -> M.MVar (Bus a f) -> IO ()

foreign export java "@static fi.ill.event.Bus.batchAssociate" batchAssociate
    :: Int -> [(a -> IO ())] -> M.MVar (Bus a f) -> [IO ()]

foreign export java "@static fi.ill.event.Bus.batchEasyAssociate" batchAssociate'
    :: [(a -> IO())] -> M.MVar (Bus a f) -> [IO ()]
