--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                              file : chunk.ads                              --
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
   type Identifier is (<>);

   None : Identifier;

   with function "="(Left, Right : Identifier) return Boolean is <>;

   with function Image(id : Identifier) return String is <>;

   with function Value(str : String) return Identifier is <>;

   with function Wide_Image(id : Identifier) return Wide_String is <>;

   with function Wide_Value(Wide_str : Wide_String) return Identifier is <>;

package Chunks is
   type Position is (B, I, O);

   type Chunk is record
      prefix : Position;
      id : Identifier;
   end record;

   Empty_Chunk : constant Chunk;

   function Create(Prefix : in Position; Id : in Identifier) return Chunk;

   function Image(chk : Chunk) return String;

   function Value(str : String) return Chunk;

   function Wide_Image(chk : Chunk) return Wide_String;

   function Wide_Value(str : Wide_String) return Chunk;

private
   Empty_Chunk : constant Chunk := (O, None);

   Hyphen : constant Character := '-';

   Wide_Hyphen : constant Wide_character := '-';

end Chunks;
