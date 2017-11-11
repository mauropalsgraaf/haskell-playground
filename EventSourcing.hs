module EventSourcing where

import Data.Foldable

type Name = String
type Password = String

data Aggregate = Person Name Password deriving (Show)
data Event = PersonNameChanged Name | PersonPasswordChanged Password

replay :: Foldable f => f Event -> Aggregate
replay fa = Data.Foldable.foldl' applyEventToAggregate (Person "" "") fa

applyEventToAggregate :: Aggregate -> Event -> Aggregate
applyEventToAggregate (Person oldName oldPassword) (PersonNameChanged newName) = Person newName oldPassword
applyEventToAggregate (Person oldName oldPassword) (PersonPasswordChanged newPassword) = Person oldName newPassword
