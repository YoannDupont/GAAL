--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : gaal.wide_io.adb                           --
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

with Library.Vector_Sets;

with Ada.Finalization,
     Ada.Characters.Handling,
     Ada.Wide_Characters.Handling;

package body Gaal.Wide_IO is

   package WTIO renames Ada.Wide_Text_IO;
   package ACH renames Ada.Characters.Handling;
   package AWCH renames Ada.Wide_Characters.Handling;

   -------------------
   -- Get_Character --
   -------------------

   procedure Get_Wide_Character
     (File : in WTIO.File_Type;
      To_Read : in Wide_Character;
      Read : out Wide_Character;
      EOL : out Boolean)
   is
   begin
      WTIO.Look_Ahead(File, Read, EOL);

      if EOL then
         return;
      end if;

      if Read = To_Read then
         WTIO.Get(File, Read);
      else
         declare
            Line : constant String := WTIO.Count'Image(WTIO.Line(File));
            Col : constant String := WTIO.Count'Image(WTIO.Col(File));
         begin
            raise Unexpected_Character with
            WTIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
              & Col(Col'First + 1 .. Col'Last) & ") : "
              & Character'Image(ACH.To_Character(Read));
         end;
      end if;
   end Get_Wide_Character;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token(File : in WTIO.File_Type) return Wide_String
   is
      function Get_Token return Wide_String is
         char : Wide_Character;
         eol : Boolean;
      begin
         WTIO.Look_Ahead(File, char, eol);

         if eol or char = ' ' or char = ACH.To_Wide_Character(ASCII.HT) then
            return "";
         end if;

         WTIO.Get(File, char);

         return char & Get_Token;
      end;
   begin
      return Get_Token;
   end Get_Token;

   package body Enumeration_IO is
      max : Positive;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid(C : in Wide_Character) return Boolean is
      begin
         return AWCH.Is_Alphanumeric(C) or C = '_';
      end Is_Valid;

      ---------
      -- Get --
      ---------

      procedure Get(File : in WTIO.File_Type; Item : out Enumeration)
      is
         buffer : Wide_String(1 .. max);
         length : Natural := 0;
         c : Wide_Character;
         continue, eol : Boolean;
      begin
         WTIO.Look_Ahead(File, c, eol);

         if eol then
            raise WTIO.End_Error;
         elsif not AWCH.Is_Alphanumeric(c) then
            declare
               Line : constant String := WTIO.Count'Image(WTIO.Line(File));
               Col : constant String := WTIO.Count'Image(WTIO.Col(File));
            begin
               raise Unexpected_Character with
               WTIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                 & Col(Col'First + 1 .. Col'Last) & ") : "
                 & Character'Image(ACH.To_Character(c));
            end;
         end if;

         length := 1;
         WTIO.Get(File, buffer(length));
         continue := True;

         while length <= max and continue loop
            WTIO.Look_Ahead(File, c, eol);

            if not eol and Is_Valid(c) then
               length := length + 1;
               WTIO.Get(File, buffer(length));
            else
               continue := False;
            end if;
         end loop;

         begin
            Item := Wide_Value(buffer(1 .. length));
         exception
            when Constraint_Error =>
               declare
                  use type WTIO.Positive_Count;
                  Line : constant String := WTIO.Count'Image(WTIO.Line(File));
                  Col : constant String := WTIO.Count'Image(WTIO.Col(File) - WTIO.Positive_Count(length));
               begin
                  raise Not_Enum with
                  WTIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                    & Col(Col'First + 1 .. Col'Last) & ") : "
                    & ACH.To_String(buffer(1 .. length));
               end;
         end;
      end Get;

      ---------
      -- Put --
      ---------

      procedure Put(File : in WTIO.File_Type; Item : in Enumeration) is
      begin
         WTIO.Put(File, Wide_Image(Item));
      end Put;
   begin
      max := Wide_Image(Enumeration'First)'Length;

      for E in Enumeration'Succ(Enumeration'First) .. Enumeration'Last loop
         if max < Wide_Image(E)'Length then
            max := Wide_Image(E)'Length;
         end if;
      end loop;
   end Enumeration_IO;

end Gaal.Wide_IO;
