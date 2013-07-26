--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                             file : gaal.io.adb                             --
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

package body Gaal.IO is

   -------------------
   -- Get_Character --
   -------------------

   procedure Get_Character
     (File    : in     Ada.Text_IO.File_Type;
      To_Read : in     Character;
      Read    :    out Character;
      EOL     :    out Boolean)
   is
   begin
      TIO.Look_Ahead(File, Read, EOL);

      if EOL then
         return;
      end if;

      if Read = To_Read then
         TIO.Get(File, Read);
      else
         declare
            Line : constant String := TIO.Count'Image(TIO.Line(File));
            Col : constant String := TIO.Count'Image(TIO.Col(File));
         begin
            raise Unexpected_Character with
            TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
              & Col(Col'First + 1 .. Col'Last) & ") : "
              & Character'Image(Read);
         end;
      end if;
   end Get_Character;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token(File : in TIO.File_Type) return String
   is
      function Get_Token return String is
         char : Character;
         eol : Boolean;
      begin
         TIO.Look_Ahead(File, char, eol);

         if eol or char = ' ' or char = ASCII.HT then
            return "";
         end if;

         TIO.Get(File, char);

         return char & Get_Token;
      end;
   begin
      return Get_Token;
   end Get_Token;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (File       : in Ada.Text_IO.File_Type;
      End_Marker : not null access function(C : in Character) return Boolean)
      return String
   is
      function Get_Token return String is
         char : Character;
         eol : Boolean;
      begin
         TIO.Look_Ahead(File, char, eol);

         if eol or End_Marker.all(char) then
            return "";
         end if;

         TIO.Get(File, char);

         return char & Get_Token;
      end;
   begin
      return Get_Token;
   end Get_Token;

end Gaal.IO;
