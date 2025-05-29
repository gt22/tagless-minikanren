
module TFKanren.Core.Logic
(
  Var
, Logic(Free, Ground)
, LogicType(unifyVal, derefVal, reify, project)
, vmapM, vmapMVal
, reify', project', generate
, showLogic
) where

import TFKanren.Core.Internal.Logic
import TFKanren.Utils.Logic