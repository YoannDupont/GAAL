--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                              file : chunk.adb                              --
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

with Ada.Characters.Handling;

package body Chunks is

   package ACH renames Ada.Characters.Handling;

   function Create(Prefix : in Position; Id : in Identifier) return Chunk is
   begin
      if prefix = O then
         return Empty_Chunk;
      else
         return (Prefix, Id);
      end if;
   end Create;

   -----------
   -- Image --
   -----------

   function Image(chk : Chunk) return String is
   begin
      case chk.prefix is
         when B | I =>
            if chk.id = None then
               return Position'Image(chk.prefix);
            else
               return Position'Image(chk.prefix) & Hyphen & Image(chk.id);
            end if;
         when O => return Position'Image(chk.prefix);
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value(str : String) return Chunk is
   begin
      if str = Position'Image(O) then
         return (prefix => O,
                 id => None);
      elsif str'length > 2 and str(str'First .. str'First) /= Position'Image(O) then
         if str(str'First + 1) = Hyphen then
            return (prefix => Position'Value(str(str'First .. str'First)),
                    id     => Value(str(str'First + 2 .. str'Last)));
         else
            raise Constraint_Error with "Invalid chunk type or format: " & str;
         end if;
      else
         raise Constraint_Error with "Invalid chunk type or format: " & str;
      end if;
   end Value;

   ----------------
   -- Wide_Image --
   ----------------

   function Wide_Image(chk : Chunk) return Wide_String is
   begin
      case chk.prefix is
         when B | I =>
            if chk.id = None then
               return Position'Wide_Image(chk.prefix);
            else
               return Position'Wide_Image(chk.prefix) & Wide_Hyphen & Wide_Image(chk.id);
            end if;
         when O => return Position'Wide_Image(chk.prefix);
      end case;
   end Wide_Image;

   ----------------
   -- Wide_Value --
   ----------------

   function Wide_Value(str : Wide_String) return Chunk is
   begin
      if str'length = 1 then
         return (prefix => Position'Wide_Value(str),
                 id => None);
      elsif str'length > 2 and str(str'First .. str'First) /= Position'Wide_Image(O) then
         if str(str'First + 1) = Wide_Hyphen then
            return (prefix => Position'Wide_Value(str(str'First .. str'First)),
                    id     => Wide_Value(str(str'First + 2 .. str'Last))
                   );
         else
            raise Constraint_Error with "Invalid chunk type or format: " & ACH.To_String(str);
         end if;
      else
         raise Constraint_Error with "Invalid chunk type or format: " & ACH.To_String(str);
      end if;
   end Wide_Value;
end Chunks;
