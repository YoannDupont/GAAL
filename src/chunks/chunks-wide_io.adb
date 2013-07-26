--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : chunk-wide_io.adb                          --
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

with Library.Wide_IO;

package body Chunks.Wide_IO is
   package WTIO renames Ada.Wide_Text_IO;

   package Identifier_IO is new Gaal.Wide_IO.Enumeration_IO(Identifier);

   package Position_IO is new Gaal.Wide_IO.Enumeration_IO(Position,
                                                          Position'Wide_Image,
                                                          Position'Wide_Value);

   procedure Get(F : in WTIO.File_Type; Item : out Identifier) renames Identifier_IO.Get;

   procedure Put(F : in WTIO.File_Type; Item : in Identifier) renames Identifier_IO.Put;

   procedure Get(F : in WTIO.File_Type; Item : out Position) renames Position_IO.Get;

   procedure Put(F : in WTIO.File_Type; Item : in Position) renames Position_IO.Put;

   procedure Get(F : in WTIO.File_Type; Item : out Chunk) is
      w_c : Wide_Character;
      eol : Boolean;
   begin
      Get(F, Item.prefix);

      case Item.prefix is
         when O => Item.id := None;
         when B | I =>
            Library.Wide_IO.Get_Wide_Character(F, '-', w_c, eol);
            Get(F, Item.id);
      end case;
   end Get;

   procedure Put(F : in WTIO.File_Type; Item : in Chunk) is
   begin
      WTIO.Put(F, Wide_Image(item));
   end Put;

end Gaal.Wide_IO;
