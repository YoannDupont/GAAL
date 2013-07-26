--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                              file : gaal.ads                               --
--                                                                            --
-- The root package. Only defines "vital" types.                              --
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

with System;

package Gaal is
   pragma Pure(Gaal);

   type Universal_Integer is mod System.Max_Binary_Modulus;
   -- This type can be used to "numerify" any scalar type

   subtype Universal_Positive is Universal_Integer range 1 .. Universal_Integer'Last;

   subtype Long_Natural is Long_Integer range 0 .. Long_Integer'Last;

   subtype Long_Positive is Long_Natural range 1 .. Long_Integer'Last;

   subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   subtype Long_Long_Positive is Long_Long_Natural range 1 .. Long_Long_Integer'Last;

   function Image(UI : in Universal_Integer) return String;
   -- Returns a trimmed version of Universal_Integer'Image(UI).

   type Matching_Strategy is (Short_Match, Long_Match);

private
   pragma Inline(Image);
end Gaal;
