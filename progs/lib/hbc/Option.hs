module Option(Option(..), thenO) where
import Maybe renaming (Maybe to Option, Nothing to None, Just to Some, thenM to thenO)

