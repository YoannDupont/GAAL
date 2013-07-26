--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : append_chunks.adb                          --
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

with Gaal.Automata.Hash.Prefix,
     Gaal.Automata.IO,
     Gaal.Inference;

with Chunks.IO,
     POS.IO;

with Presentations,
     File_Processing;

with Ada.Text_IO,
     Ada.Command_Line;
use ada.Text_IO;

procedure Append_Chunks is
   package Automata is new Gaal.Automata(POS.TAGS, POS.Image, POS.Value);
   package Hash is new Automata.Hash;
   package Prefix is new Hash.Prefix;

   package Automata_IO is new Automata.IO(POS.IO);

   type CHUNK is (ADP, AP, CONJ, NP, VN, PP, UNKNOWN, None);

   package Chunking is new Chunks(CHUNK, None, "=", CHUNK'Image, CHUNK'Value,
                                  CHUNK'Wide_Image, CHUNK'Wide_Value);
   package CHUNK_IO is new Chunking.IO;

   package TAGS_Pres is new Presentations(POS.TAGS,
                                          POS.Image,
                                          POS.Value,
                                          Automata);

   package FP is new File_Processing(POS.TAGS,
                                     POS.Image,
                                     POS.Value,
                                     POS.IO,
                                     Automata,
                                     TAGS_Pres,
                                     Chunking);

   H : File_Type;
   H1 : File_Type;

   Au_Table : constant FP.Automata_Array := (others => new Prefix.Automaton'(Prefix.Empty_Automaton));
   deletion : Automata.Automaton_Object;

   K       : constant Gaal.Universal_Integer := Gaal.Universal_Integer'Value(Ada.Command_Line.Argument(1));
   InFile  : constant String                 := Ada.Command_Line.Argument(2);
   OutFile : constant String                 := Ada.Command_Line.Argument(3);
begin
   declare
      P : FP.Presentation;
--        P : FP.Weighted_Presentation;
      V : TAGS_Pres.Presentations.Vector;
--        V : TAGS_Pres.Weighted_Presentations.Vector;
      W : FP.USW.Vector;
--        W : TAGS_Pres.Weighted_Vector;
      pres : File_Type;
   begin
      Open(H, In_File, InFile);
      FP.Get_Presentation(h, P);
--        FP.Get_Weighted_Presentation(h, P);
      Close(H);
      Create(pres, Out_File, "presentations");

      for CHK in CHUNK loop
         if CHK /= CHUNK'First then
            New_Line(pres);
            New_Line(pres);
         end if;

         V := P(CHK);

         CHUNK_IO.Put(pres, CHK);
         Put(pres, ':');

         for I in V.First_Index .. V.Last_Index loop
            W := TAGS_Pres.Presentations.Element(V, I);
--              W := TAGS_Pres.Weighted_Presentations.Element(V, I);

            New_Line(pres);

            for J in W.First_Index .. W.Last_Index loop
--              for J in W.word.First_Index .. W.word.Last_Index loop
               if J /= W.First_Index then
--                 if J /= W.word.First_Index then
                  Put(pres, " ");
               end if;

               POS.IO.Put(pres, W.Element(J));
--                 TAGS_IO.Put(pres, W.word.Element(J));
            end loop;
            W.Clear;
--              Put(pres, Natural'Image(W.n_occur));
--              W.word.Clear;
         end loop;
      end loop;

      Close(pres);
      for TARGET in CHUNK loop
         TAGS_Pres.From_Presentation(Au_Table(TARGET).all, P(TARGET));
      end loop;
   end;

   New_Line;
   Put_Line(Gaal.Image(K) & "-Reversible Inference...");
   declare
      package Inference is new Gaal.Inference(Automata);
      tmp : File_Type;
   begin
      for TARGET in CHUNK loop
         if TARGET /= NONE then
            Put_Line(CHUNK'Image(TARGET));
            Inference.K_Reversible_Inference(Au_Table(TARGET).all, K, True);
            Create(tmp, Out_File, "kri/" & CHUNK'Image(TARGET) & "-" & Gaal.Image(K) & ".gv");
            Automata_IO.Dot_Write(tmp, Au_Table(TARGET).all);
            Close(tmp);
         end if;
      end loop;
   end;

   Open(H, In_File, InFile);
   Create(H1, Out_File, OutFile);
   FP.Apply(H, H1, Au_Table, Gaal.Long_Match);

   for TARGET in CHUNK loop
      deletion := Automata.Automaton_Object(Au_Table(TARGET));
      Automata.Free(deletion);
   end loop;
end Append_Chunks;
