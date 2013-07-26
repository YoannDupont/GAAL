--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --                                                                         --
--                        file : gaal-automata-io.ads                         --
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

with Gaal.IO.Enumeration_IO;

generic
   with package Sigma_IO is new Gaal.IO.Enumeration_IO
     (Sigma, Image, Value,
      Valid_Character => <>
     );

package Gaal.Automata.IO is

   procedure Read(File : in Ada.Text_IO.File_Type; A : out Automaton'Class);

   procedure Write(File : in Ada.Text_IO.File_Type; A : in Automaton'Class);

   procedure PTA(File : in Ada.Text_IO.File_Type; A : out Automaton'Class);

   procedure Dot_Write(File : in Ada.Text_IO.File_Type; A : in Automaton'Class);

   procedure Dot_Read(File : in Ada.Text_IO.File_Type; A : in out Automaton'Class);

   procedure Apply
     (In_File   : in Ada.Text_IO.File_Type;
      Out_File  : in Ada.Text_IO.File_Type;
      A         : in Automata.Automaton'Class;
      Strategy  : in Matching_Strategy;
      Appendice : in String := "");

   Unexpected_Character,
   Unreachable_State,
   Bad_Format : Exception;

private
   package Buffers is new Ada.Containers.Vectors(Positive,
                                                 Ada.Strings.Unbounded.Unbounded_String,
                                                 Ada.Strings.Unbounded."=");

end Gaal.Automata.IO;
