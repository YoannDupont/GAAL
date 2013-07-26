--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                        file : string_operations.ads                        --
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

with Ada.Containers.Vectors,
     Ada.Strings.Unbounded;

package String_Operations is
   package Slice_Vectors is new Ada.Containers.Vectors
     (Positive,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   function Element
     (Target : in Slice_Vectors.Vector;
      I      : in Positive)
      return String;

   function Join
     (Target  : in Slice_Vectors.Vector;
      Pattern : in String)
      return String;

   function Split
     (Target  : in String;
      Pattern : in String)
      return Slice_Vectors.Vector;

   function Sub_Slice
     (Target : in Slice_Vectors.Vector;
      lo_nth : in Positive;
      hi_nth : in Positive)
      return Slice_Vectors.Vector;
end String_Operations;
