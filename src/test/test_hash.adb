--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                            file : test_hash.adb                            --
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

with Gaal.Automata.Hash,
     Gaal.Inference;

with Ada.Text_IO,
     Ada.Containers;

procedure Test_Hash is
   use Gaal;

   subtype Alphabet is Character range 'A' .. 'C';

   function img(A : in Alphabet) return String is
   begin
      return (1 => A);
   end img;

   package Automata is new Gaal.Automata(Sigma => Alphabet,
                                         Image => img,
                                         Value => Alphabet'Value);
   package AUIS renames Automata.UIS;
   package Hash is new Automata.Hash;

   package Inference is new Gaal.Inference(Automata);

   A : Hash.Automaton;

   QS : AUIS.Set;
   test : Boolean;

   procedure Put(E : AUIS.Cursor) is
   begin
      Ada.Text_IO.Put(Universal_Integer'Image(AUIS.Element(E)));
   end Put;
begin
   A.Set_Initial(0);
   Ada.Text_IO.Put_Line("Set 0 initial for A...");
   A.Set_Final(1);
   Ada.Text_IO.Put_Line("Set 1 final for A...");
   A.Set_Final(2);
   Ada.Text_IO.Put_Line("Set 2 final for A...");

   A.Make_Transition(0, 'A', 0);
   Ada.Text_IO.Put_Line("Added (0, A, 0) transition for A...");
   A.Make_Transition(0, 'A', 1);
   Ada.Text_IO.Put_Line("Added (0, A, 1) transition for A...");
   A.Make_Transition(0, 'B', 2);
   Ada.Text_IO.Put_Line("Added (0, B, 2) transition for A...");

   QS := A.Delta_Function(3, 'B');

   Ada.Text_IO.Put_Line(Ada.Containers.Count_Type'Image(QS.Length));
   QS.Iterate(Put'Access);
   Ada.Text_IO.Put_Line(ASCII.LF & A.Image);

   Ada.Text_IO.Put_Line("Size of A ?" & Universal_Integer'Image(A.Size));

--     A.Merge(2, 0);
--     Ada.Text_IO.Put_Line("Merging 2 into 0 in A...");
   A.Merge_Determinization(test);
   Ada.Text_IO.Put_Line("Determinizing A through merges...");
   Ada.Text_IO.Put_Line(ASCII.LF & A.Image);

   A.Clear;
   Ada.Text_IO.Put_Line(ASCII.LF & "Clearing A...");
   Ada.Text_IO.Put_Line(A.Image);

   A.Set_Initial(0);
   Ada.Text_IO.Put_Line("Set 0 initial for A...");

   A.Make_Transition(0, 'A', 1);
   Ada.Text_IO.Put_Line("Added (0, A, 1) transition for A...");
   A.Make_Transition(1, 'C', 2);
   Ada.Text_IO.Put_Line("Added (1, C, 2) transition for A...");
   A.Set_Final(2);
   Ada.Text_IO.Put_Line("Set 2 final for A...");
   A.Make_Transition(1, 'B', 3);
   Ada.Text_IO.Put_Line("Added (1, B, 3) transition for A...");
   A.Make_Transition(3, 'C', 4);
   Ada.Text_IO.Put_Line("Added (3, C, 4) transition for A...");
   A.Set_Final(4);
   Ada.Text_IO.Put_Line("Set 4 final for A...");
   Ada.Text_IO.Put_Line(A.Image & ASCII.LF);

   Ada.Text_IO.Put_Line("0-RI(A)...");
   Inference.K_Reversible_Inference(A, 0);
   Ada.Text_IO.Put_Line(A.Image);

   Ada.Text_IO.Put_Line("--------------------------------------------------------------------------------");

   A.Clear;
   A.Set_Initial(0);
   Ada.Text_IO.Put("A.States =");
   A.States.Iterate(Put'Access);
   Ada.Text_IO.New_Line;

   A.Make_Transition(2, 'A', 3);
   A.Make_Transition(3, 'A', 3);
   A.Make_Transition(3, 'B', 4);
   A.Make_Transition(4, 'A', 5);
   A.Make_Transition(4, 'B', 15);
   A.Make_Transition(5, 'A', 6);
   A.Make_Transition(5, 'B', 16);
   A.Make_Transition(6, 'A', 7);
   A.Make_Transition(6, 'B', 17);
   A.Make_Transition(7, 'A', 8);
   A.Make_Transition(7, 'B', 18);
   A.Make_Transition(8, 'A', 9);
   A.Make_Transition(8, 'B', 19);
   A.Make_Transition(9, 'A', 10);
   A.Make_Transition(9, 'B', 110);

   A.Make_Transition(10, 'A', 10);
   A.Make_Transition(10, 'B', 110);
   A.Make_Transition(11, 'A', 10);
   A.Make_Transition(12, 'B', 110);
   A.Make_Transition(13, 'A', 10);
   A.Make_Transition(14, 'B', 110);
   A.Make_Transition(1, 'C', 14);
   A.Make_Transition(0, 'C', 1);

   Ada.Text_IO.Put("A.States =");
   A.States.Iterate(Put'Access);
   Ada.Text_IO.New_Line;

   A.Make_Transition(0, 'A', 2);

   Ada.Text_IO.Put("A.States =");
   A.States.Iterate(Put'Access);
   Ada.Text_IO.New_Line;
end Test_Hash;
