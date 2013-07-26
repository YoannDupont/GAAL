--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : pretreatment.adb                           --
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

with Gaal.IO,
     Ada.Text_IO,
     Ada.Characters.Handling;

package body Pretreatment is
   package RT_IO is new Ada.Text_IO.Integer_IO(Range_Type);

   ----------
   -- Hash --
   ----------

   function Hash (R : Range_Type) return Ada.Containers.Hash_Type is
   begin
      return Range_Type'Pos(R);
   end Hash;

   --------------------------
   -- Load_Correspondances --
   --------------------------

   function Load_Correspondances(Name : String) return Correspondance
   is
      char : Character;
      eol  : Boolean;
      key  : Range_Type;
      M    : Range_To_Str.Map;
      F    : Ada.Text_IO.File_Type;
      valids : Valid_Characters := (others => False);
   begin
      Ada.Text_IO.Open(F,
                       Ada.Text_IO.In_File,
                       Name);

      while not Ada.Text_IO.End_Of_File(F) loop
         declare
            tmp : constant String := Gaal.IO.Get_Token(F);
         begin
            Gaal.IO.Get_Character(F, ASCII.HT, char, eol);
            RT_IO.Get(F, key);
            M.Include(key, Ada.Strings.Unbounded.To_Unbounded_String(tmp));

            for I in tmp'Range loop
               valids(tmp(I)) := True;
            end loop;
         end;

         if not Ada.Text_IO.End_Of_File(F) then
            Ada.Text_IO.Skip_Line(F);
         end if;
      end loop;

      Ada.Text_IO.Close(F);

      return (Map   => M,
              Valid => Valids);
   end Load_Correspondances;

   -------------------------
   -- Get_Correspondances --
   -------------------------

   function Get_Correspondance(RT : Range_Type; Corr : Correspondance) return String is
   begin
      return Ada.Strings.Unbounded.To_String(Corr.Map.Element(RT));
   end Get_Correspondance;

   -------------
   -- Get_Key --
   -------------

   function Get_Key(Str : String; Corr : Correspondance) return Range_Type
   is
      Candidate : Range_To_Str.Cursor := Corr.Map.First;
      Upper : constant String := Ada.Characters.Handling.To_Upper(Str);
   begin
      while Range_To_Str.Has_Element(Candidate) loop
         if Upper = Ada.Characters.Handling.To_Upper(Ada.Strings.Unbounded.To_String(Range_To_Str.Element(Candidate))) then
            return Range_To_Str.Key(Candidate);
         else
            Candidate := Range_To_Str.Next(Candidate);
         end if;
      end loop;

      raise No_Key with Str & " cannot be found in Map.";
   end Get_Key;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid(C : Character; Corr : Correspondance) return Boolean is
   begin
      return Corr.Valid(C);
   end Is_Valid;

end Pretreatment;
