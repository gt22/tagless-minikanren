
module TFKanren.Core.Logic
(
  Var
, Logic(Free, Ground)
, NoVars
, LogicVar, Deref
, vmapM, vmapMVal
, unifyVal
, LogicShow(LogicShow)
) where

import TFKanren.Core.Internal.Logic
import TFKanren.Utils.Logic