--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                            file : chunk-io.ads                             --
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

with Gaal.IO.Enumeration_IO;

package body Chunks.IO is
   package TIO renames Ada.Text_IO;

   package Identifier_IO is new Gaal.IO.Enumeration_IO(Identifier);

   package Position_IO is new Gaal.IO.Enumeration_IO(Position,
                                                     Position'Image,
                                                     Position'Value);

   procedure Get(F : in TIO.File_Type; Item : out Identifier) renames Identifier_IO.Get;

   procedure Put(F : in TIO.File_Type; Item : in Identifier) renames Identifier_IO.Put;

   procedure Get(F : in TIO.File_Type; Item : out Position) renames Position_IO.Get;

   procedure Put(F : in TIO.File_Type; Item : in Position) renames Position_IO.Put;

   procedure Get(F : in TIO.File_Type; Item : out Chunk) is
      c : Character;
      eol : Boolean;
   begin
      Get(F, Item.prefix);

      case Item.prefix is
         when O => Item.id := None;
         when B | I =>
            Gaal.IO.Get_Character(F, '-', c, eol);
            Get(F, Item.id);
      end case;
   end Get;

   procedure Put(F : in TIO.File_Type; Item : in Chunk) is
   begin
      TIO.Put(F, Image(item));
   end Put;

end Chunks.IO;
