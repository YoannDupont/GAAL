--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                           file : test_match.adb                            --
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

procedure test_match is
   use type Gaal.Universal_Integer;

   type tags is (DET, ADV, ADJ);

   package Auto is new Gaal.Automata(tags, tags'Image, tags'Value);
   package Hash is new Auto.Hash;

   test : Auto.Automaton'class := Hash.Empty_Automaton;

   word : constant Auto.Sigma_Word := (1 => DET, 2 => ADV, 3 .. 80 => ADJ);
   s : String(1 .. 80);
begin
   test.Set_Initial(0);
   test.Make_Transition(0, DET, 1);
   test.Make_Transition(1, ADJ, 2); test.Set_Final(2);
   test.Make_Transition(1, ADV, 3);
   test.Make_Transition(3, ADJ, 4); test.Set_Final(4);
--     test.Make_Transition(4, ADJ, 5); test.Set_Final(5);

   s := test.Matching_Sequences(word, Gaal.Short_Match);
   Ada.Text_IO.Put_Line(s);
   s := test.Matching_Sequences(word, Gaal.Long_Match);
   Ada.Text_IO.Put_Line(s);
   Ada.Text_IO.Put_Line(Boolean'Image(test.Matches(word)));

   test.Make_Transition(4, ADJ, 4);
   test.Make_Transition(0, ADJ, 0); test.Set_Final(0);

   s := test.Matching_Sequences(word, Gaal.Short_Match);
   Ada.Text_IO.Put_Line(s);
   s := test.Matching_Sequences(word, Gaal.Long_Match);
   Ada.Text_IO.Put_Line(s);
   Ada.Text_IO.Put_Line(Boolean'Image(test.Matches(word)));

   Ada.Text_IO.Put_Line(test.Img);
end test_match;
