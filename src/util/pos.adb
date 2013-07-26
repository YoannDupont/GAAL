--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                               file : pos.adb                               --
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

with Ada.Characters.Handling;

package body POS is
   function Image(T : in TAGS) return String is
   begin
      case T is
         when ADJ .. PONCT | PREF .. VPP => return TAGS'Image(T);
         when P_PLUS_D => return "P+D";
         when P_PLUS_PRO => return "P+PRO";
      end case;
   end Image;

   function Value(S : in String) return TAGS is
   begin
      if S = "P+D" then
         return P_PLUS_D;
      elsif S = "P+PRO" then
         return P_PLUS_PRO;
      else
         return TAGS'Value(S);
      end if;
   end Value;

   function Valid(C : in Character) return Boolean is
   begin
      return Ada.Characters.Handling.Is_Letter(C) or C = '_' or C = '+';
   end Valid;
end POS;
