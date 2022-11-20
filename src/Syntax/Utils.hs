module Syntax.Utils where

import Types

class Parsable a where
  parse :: Parser a
