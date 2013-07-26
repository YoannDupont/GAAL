--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                         file : ihm-from_string.adb                         --
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

separate(tui)

function From_String(input : in String) return Command_Type is
   last : Natural := 0;
begin
   if input'Length > 2 and then input(input'First .. input'First + 1) = "--" then
      for I in input'First + 2 .. input'Last loop
         if input(I) = ' ' then
            last := I - 1;
            exit;
         end if;
      end loop;

      if last = 0 then
         last := input'Last;
      end if;

      begin
         return Command_Type'Value(input(input'First + 2 .. last));
      exception
         when Constraint_Error =>
            raise Invalid_Command with "Invalid command: " & input;
      end;
   else
      raise Invalid_Command with "Invalid command: " & input;
   end if;
end From_String;
