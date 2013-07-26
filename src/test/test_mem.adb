--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                            file : test_mem.adb                             --
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

with Gaal.Automata.Hash;

with Ada.Text_IO;

procedure Test_Mem is
   use Gaal;

   subtype Alphabet is Character range 'A' .. 'Z';

   function img(A : in Alphabet) return String is
   begin
      return (1 => A);
   end img;

   package Automata is new Gaal.Automata(Sigma => Alphabet,
                                         Image => img,
                                         Value => Alphabet'Value);
   package Hash is new Automata.Hash;

   package AUIS renames Automata.UIS;

   procedure Put(E : AUIS.Cursor) is
   begin
      Ada.Text_IO.Put(Universal_Integer'Image(AUIS.Element(E)));
   end Put;

begin
   declare
      A : Hash.Automaton;
   begin
      Ada.Text_IO.Put_Line("creating");

      for From in 0 .. 128 loop
         for By in Alphabet loop
            for To in 0 .. 128 loop
               A.Make_Transition(Universal_Integer(From), By, Universal_Integer(To));
            end loop;
         end loop;
         Ada.Text_IO.Put(".");
      end loop;
      Ada.Text_IO.New_Line;

--        Ada.Text_IO.Put_Line("deleting");
--
--        for From in 0 .. 128 loop
--           for By in Alphabet loop
--              for To in 0 .. 128 loop
--                 A.Remove_Transition(Universal_Integer(From), By, Universal_Integer(To));
--              end loop;
--           end loop;
--           Ada.Text_IO.Put(".");
--        end loop;
--        Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line("Merging");

      for S in 0 .. 128 loop
         A.Merge(Universal_Integer(S), Universal_Integer(0));
         Ada.Text_IO.Put(".");
      end loop;
      Ada.Text_IO.New_Line;

      for S in Alphabet loop
         AUIS.Iterate(A.Delta_Function(Universal_Integer(0), S), Put'Access);
         Ada.Text_IO.New_Line;
      end loop;

      A.Clear;
   end;

   Ada.Text_IO.Put_Line("waiting");
   delay 2.0;
end Test_Mem;
