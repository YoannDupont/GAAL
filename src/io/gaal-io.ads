--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                             file : gaal.io.ads                             --
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

with Ada.Text_IO;

package Gaal.IO is

   procedure Get_Character
     (File    : in     Ada.Text_IO.File_Type;
      To_Read : in     Character;
      Read    :    out Character;
      EOL     :    out Boolean);
   -- A "constrained" Get procedure.
   -- It has to get either To_Read or and EOL character.
   -- Raises an exception if Read /= To_Read.
   -- Read only "Looked Ahead" in such condition.

   function Get_Token(File : in Ada.Text_IO.File_Type) return String;
   -- Gets a token in File, that is to say a non-space sequence.

   function Get_Token
     (File       : in Ada.Text_IO.File_Type;
      End_Marker : not null access function(C : in Character) return Boolean)
      return String;
   -- Gets a token in File until an End_Marker is found.
   -- Also ends at the end of line/file.

   Unexpected_Character : exception;

private
   package TIO renames Ada.Text_IO;
end Gaal.IO;
