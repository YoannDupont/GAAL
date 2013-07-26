--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : ihm-arity_of.adb                           --
--                                                                            --
-- Author: Yoann Dupont                                                       --
-- Copyright (C) 2013 Yoann Dupont - all right reserved                       --
--                                                                            --
--  This program is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  This program is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.     --
--------------------------------------------------------------------------------

separate(tui)

function Arity_Of(Command : in Command_Type) return String is
begin
   case Command is
      when Arity_Zero => return "[0]";
      when Arity_ZeroToOne => return "[0,1]";
      when Arity_One => return "[1]";
      when Arity_OneToTwo => return "[1,2]";
      when Arity_Two => return "[2]";
      when Arity_ThreeToFour => return "[3,4]";
   end case;
end Arity_Of;
