--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                     file : gaal.io.enumeration-io.ads                      --
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
   type Enumeration is (<>);
   with function Image(E : in Enumeration) return String is <>;
   with function Value(S : in String) return Enumeration is <>;
   Valid_Character : access function(C : Character) return Boolean := null;

package Gaal.IO.Enumeration_IO is

   procedure Get(File : in Ada.Text_IO.File_Type; Item : out Enumeration);
   procedure Put(File : in Ada.Text_IO.File_Type; Item : in Enumeration);

   Not_Enum : exception;
end Gaal.IO.Enumeration_IO;
