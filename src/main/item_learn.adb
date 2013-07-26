--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                           file : item_learn.adb                            --
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
     Gaal.IO.Enumeration_IO,
     Gaal.Automata.IO,
     Gaal.Inference,
     Pretreatment;
use Gaal;

with Ada.Text_IO,
     Ada.Containers,
     Ada.Command_Line,
     Ada.Directories;
use ada.Text_IO;

procedure Item_Learn is
   CORRESP : constant String            := Ada.Command_Line.Argument(1);
   NAME    : constant String            := Ada.Command_Line.Argument(2);
   K       : constant Universal_Integer := Universal_Integer'Value(Ada.Command_Line.Argument(3));
   VERBOSE : constant Boolean           := Boolean'Value(Ada.Command_Line.Argument(4));
   OUTPUT  : constant String            := Ada.Command_Line.Argument(5);

   package Pret is new Pretreatment(Ada.Containers.Count_Type);
   Correspondance : constant Pret.Correspondance := Pret.Load_Correspondances(CORRESP);

   subtype TAGS is Ada.Containers.Count_Type range 1 .. Correspondance.Map.Length;

   function Image(T : TAGS) return String is
   begin
      return Pret.Get_Correspondance(T, Correspondance);
   end Image;

   function Value(S : String) return TAGS is
   begin
      return Pret.Get_Key(S, Correspondance);
   exception
      when Pret.No_Key => return TAGS'Value(S);
   end Value;

   function Valid(C : Character) return Boolean is
   begin
      return Pret.Is_Valid(C, Correspondance);
   end Valid;

   package POS_IO is new Gaal.IO.Enumeration_IO(TAGS, Image, Value, Valid'Access);

   package Automata is new Gaal.Automata(TAGS, Image, Value);-- TAGS'Value);
   package Hash is new Automata.Hash;
   package Prefix is new Hash.Prefix;

   package Inference is new Gaal.Inference(Automata);

   package Automata_IO is new Automata.IO(POS_IO);

   auto : Automata.Automaton'Class := Prefix.Empty_Automaton;

   file : File_Type;
begin
   if Ada.Directories.Exists(OUTPUT) then
      raise Ada.Directories.Status_Error with "file already exists: " & OUTPUT;
   end if;

   Open(file, In_file, NAME);
   Automata_IO.PTA(file, auto);
   Close(file);

   Inference.K_Reversible_Inference(auto, K, VERBOSE);
   Ada.Text_IO.New_Line(Ada.Text_IO.Standard_Output);

   Create(file, Out_file, OUTPUT);
   Automata_IO.Dot_Write(file, auto);
end Item_Learn;
