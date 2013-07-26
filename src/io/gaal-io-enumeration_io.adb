--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                     file : gaal.io.enumeration-io.adb                      --
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

package body Gaal.IO.Enumeration_IO is

   package ACH renames Ada.Characters.Handling;

   max : Positive;

   -------------------
   -- Default_Valid --
   -------------------

   function Default_Valid(C : in Character) return Boolean is
   begin
      return ACH.Is_Alphanumeric(C) or C = '_';
   end Default_Valid;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid(C : in Character) return Boolean is
   begin
      if Valid_Character = null then
         return Default_Valid(C);
      else
         return Valid_Character.all(C);
      end if;
   end Is_Valid;

   ---------
   -- Get --
   ---------

   procedure Get(File : in TIO.File_Type; Item : out Enumeration)
   is
      buffer : String(1 .. max);
      length : Natural := 0;
      c : Character;
      continue, eol : Boolean;
   begin
      TIO.Look_Ahead(File, c, eol);

      if eol then
         raise TIO.End_Error with TIO.Count'Image(TIO.Line(File)) & " " & TIO.Count'Image(TIO.Col(File));
         -- missing column
         -- TODO : proper exception message !
      end if;

      length := 1;
      TIO.Get(File, buffer(length));
      continue := True;

      while length <= max and continue loop
         TIO.Look_Ahead(File, c, eol);

         if not eol and Is_Valid(c) then
            length := length + 1;
            TIO.Get(File, buffer(length));
         else
            continue := False;
         end if;
      end loop;

      Item := Value(buffer(1 .. length));

   exception
      when Constraint_Error =>
         declare
            use type TIO.Positive_Count;
            Line : constant String := TIO.Count'Image(TIO.Line(File));
            Col  : constant String := TIO.Count'Image(TIO.Col(File) - TIO.Positive_Count(length));
         begin
            raise Not_Enum with
            TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
              & Col(Col'First + 1 .. Col'Last) & ") : "
              & buffer(1 .. Positive'Min(max, length));
         end;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put(File : in TIO.File_Type; Item : in Enumeration) is
   begin
      TIO.Put(File, Image(Item));
   end Put;
begin
   max := Image(Enumeration'First)'Length;

   for E in Enumeration'Succ(Enumeration'First) .. Enumeration'Last loop
      if max < Image(E)'Length then
         max := Image(E)'Length;
      end if;
   end loop;
end Gaal.IO.Enumeration_IO;
