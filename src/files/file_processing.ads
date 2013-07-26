--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                         file : file_processing.ads                         --
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

with Chunks,
     Presentations;

with Gaal.IO.Enumeration_IO,
     Gaal.Automata;

with Ada.Text_IO;

private with Ada.Strings.Unbounded,
     Ada.Containers.Vectors;

generic
   type POS is (<>);

   with function Image(P : in POS) return String is <>;

   with function Value(S : in String) return POS is <>;

   with package POS_IO is new Gaal.IO.Enumeration_IO(POS,
                                                     Image,
                                                     Value,
                                                     Valid_Character => <>);

   with package Automata is new Gaal.Automata(POS,
                                              Image,
                                              Value);

   with package POS_Presentations is new Presentations(POS,
                                                       Image,
                                                       Value,
                                                       Automata);

   with package Chunking is new Chunks (<>);

package File_Processing is
   package USW renames Automata.Unbounded_Sigma_Words;
   package Presentations renames POS_Presentations.Presentations;
   package Weighted_Presentations renames POS_Presentations.Weighted_Presentations;

   type Presentation is array(Chunking.Identifier'Range)
     of Presentations.Vector;

   type Weighted_Presentation is array(Chunking.Identifier'Range)
     of Weighted_Presentations.Vector;

   type Automata_Array is array(Chunking.Identifier'Range)
     of not null access Automata.Automaton'Class;

   procedure Get_Presentation
     (File : in Ada.Text_IO.File_Type;
      P    : out Presentation);

   procedure Get_Weighted_Presentation
     (File : in     Ada.Text_IO.File_Type;
      WP   :    out Weighted_Presentation);

   procedure Apply
     (In_File  : in Ada.Text_IO.File_Type;
      Out_File : in Ada.Text_IO.File_Type;
      A        : in Automata.Automaton'Class;
      Strategy : in Gaal.Matching_Strategy);

   procedure Apply
     (In_File  : in Ada.Text_IO.File_Type;
      Out_File : in Ada.Text_IO.File_Type;
      A        : in Automata_Array;
      Strategy : in Gaal.Matching_Strategy);

   BAD_FORMAT : exception;

private
   package Buffers is new Ada.Containers.Vectors(Positive,
                                                 Ada.Strings.Unbounded.Unbounded_String,
                                                 Ada.Strings.Unbounded."=");
end File_Processing;
