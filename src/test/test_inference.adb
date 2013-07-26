--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                         file : test_inference.adb                          --
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
     Gaal.Inference,
     Gaal.Automata.IO,
     Gaal.IO.Enumeration_IO;

with Ada.Text_IO;

procedure test_inference is
   use type Gaal.Universal_Integer;

   type tags is (DET, ADV, ADJ);

   function Image(t : tags) return String renames tags'Image;
   function Value(s : string) return tags renames tags'Value;

   package Tags_IO is new Gaal.IO.Enumeration_IO(tags, Image, Value);

   package Auto is new Gaal.Automata(tags, Image, Value);
   package Hash is new Auto.Hash;

   package Inference is new Gaal.Inference(Auto);

   test : Auto.Automaton'class := Hash.Empty_Automaton;

   word : constant Auto.Sigma_Word := (1 => DET, 2 => ADV, 3 => ADV, 4 => ADJ);

   K : constant Gaal.Universal_Integer := 0;
begin
   Auto.Set_Initial(test, 0);

   test.Set_Initial(0);
   test.Make_Transition(0, Det, 1);
   test.Make_Transition(1, adj, 2); test.Set_Final(2);
   test.Make_Transition(1, adv, 3);
   test.Make_Transition(3, adj, 4); test.Set_Final(4);

   Ada.Text_IO.Put_Line(Auto.Image(test));
   Ada.Text_IO.Put_Line("Matches DET ADV ADV ADJ ? " & Boolean'Image( Auto.Matches(test, word) ));
   Ada.Text_IO.Put_Line(Gaal.Image(K) & "-RI...");
   Inference.K_Reversible_Inference(test, K);
   Ada.Text_IO.Put_Line(Auto.Image(test));
   Ada.Text_IO.Put_Line("Matches DET ADV ADV ADJ ? " & Boolean'Image( Auto.Matches(test, word) ));
   Ada.Text_IO.New_Line;


   declare
      package IO is new Auto.IO(Tags_IO);

      F : Ada.Text_IO.File_Type;
      b : Hash.Automaton;
   begin
      Ada.Text_IO.Open(F, Ada.Text_IO.In_File, "f");

      IO.PTA(F, b);
      Ada.Text_IO.Close(F);
      IO.Write(Ada.Text_IO.Standard_Output, b);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Open(F, Ada.Text_IO.In_File, "g");
      -- TODO : inconsistant automata > must check consistency of read automata

      IO.Read(F, b);
      IO.Write(Ada.Text_IO.Standard_Output, b);
      Ada.Text_IO.Close(F);
   end;
end test_inference;
