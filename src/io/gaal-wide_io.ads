--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : gaal.wide_io.ads                           --
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

with Ada.Wide_Text_IO;

package Gaal.Wide_IO is

   procedure Get_Wide_Character
     (File : in Ada.Wide_Text_IO.File_Type;
      To_Read : in Wide_Character;
      Read : out Wide_Character;
      EOL : out Boolean);

   function Get_Token(File : in Ada.Wide_Text_IO.File_Type) return Wide_String;

   generic
      type Enumeration is (<>);
      with function Wide_Image(E : in Enumeration) return Wide_String is <>;
      with function Wide_Value(S : in Wide_String) return Enumeration is <>;
   package Enumeration_IO is
      procedure Get(File : in Ada.Wide_Text_IO.File_Type; Item : out Enumeration);
      procedure Put(File : in Ada.Wide_Text_IO.File_Type; Item : in Enumeration);

      Not_Enum : exception;
   end Enumeration_IO;

   Unexpected_Character : exception;

end Gaal.Wide_IO;
