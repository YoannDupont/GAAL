--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --                                                                         --
--                    file : gaal-automata-state-tags.ads                     --
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
generic
package Gaal.Automata.State_Tags is
   type State_Tagging is interface;
   -- A State_Tagging allows user to define a set of tags for every single state
   -- in the automaton. Those tags are words which add complementary information
   -- like, for example, a set of prefixes.

   function Tags
     (Target : in State_Tagging;
      State  : in Universal_Integer)
     return Bags_Of_Words.Set is abstract;

   procedure Add_Tag
     (Target : in out State_Tagging;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word) is abstract;

   procedure Remove_Tag
     (Target : in out State_Tagging;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word) is abstract;

   procedure Clear_Tags
     (Target : in out State_Tagging;
      State  : in Universal_Integer) is abstract;
end Gaal.Automata.State_Tags;
