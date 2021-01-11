module Util.Maybe (
    flipMaybe
) where

flipMaybe :: Maybe (Maybe a) -> Maybe (Maybe a)
flipMaybe Nothing = Just Nothing
flipMaybe (Just Nothing) = Nothing
flipMaybe x = x 