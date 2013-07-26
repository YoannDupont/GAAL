--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                         file : gaal.inference.adb                          --
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

with Ada.Containers;

use type Ada.Containers.Count_Type;

with Ada.Text_IO,
     Ada.Calendar;

package body Gaal.Inference is
   package P_IO is new Ada.Text_IO.Integer_IO(Positive);
   package UI_IO is new Ada.Text_IO.Modular_IO(Universal_Integer);
   package Du_IO is new Ada.Text_IO.Fixed_IO(Duration);

   procedure Dump
     (Iter             : in Positive;
      Target, Into     : in Universal_Integer;
      Size             : in Universal_Integer;
      Time_Laps, total : in Duration) is
   begin
      Ada.Text_IO.Put("[");
      P_IO.Put(Iter, 6);
      Ada.Text_IO.Put("]");
      Ada.Text_IO.Put("    merge=(");
      UI_IO.Put(Target, 6);
      Ada.Text_IO.Put(" > ");
      UI_IO.Put(Into, 6);
      Ada.Text_IO.Put(")");
      Ada.Text_IO.Put("    size=");
      UI_IO.Put(Size, 6);
      Ada.Text_IO.Put("    laps=");
      Du_IO.Put(Time_Laps, 7, 2);
      Ada.Text_IO.Put("/");
      Du_IO.Put(Total, 7, 2);
      Ada.Text_IO.New_Line;
   end Dump;

   procedure Dump
     (Iter             : in Positive;
      Size             : in Universal_Integer;
      Time_Laps, total : in Duration) is
   begin
      Ada.Text_IO.Put("[");
      P_IO.Put(Iter, 6);
      Ada.Text_IO.Put("]");
      Ada.Text_IO.Put("    merge=<<DETERMIZATION>>");
      Ada.Text_IO.Put("    size=");
      UI_IO.Put(Size, 6);
      Ada.Text_IO.Put("    laps=");
      Du_IO.Put(Time_Laps, 7, 2);
      Ada.Text_IO.Put("/");
      Du_IO.Put(Total, 7, 2);
      Ada.Text_IO.New_Line;
   end Dump;

   procedure Init (A : in Automata.Automaton'class) is
   begin
      Ada.Text_IO.Put("[");
      UI_IO.Put(0, 6);
      Ada.Text_IO.Put("]");
      Ada.Text_IO.Put("            size(q0)=");
      UI_IO.Put(Universal_Integer(A.q0.Length), 6);
      Ada.Text_IO.Put("    size=");
      UI_IO.Put(A.Size, 6);
      Ada.Text_IO.Put("    size(qf)=");
      UI_IO.Put(Universal_Integer(A.qf.Length), 6);
      Ada.Text_IO.New_Line;
   end Init;

   ----------------------------
   -- K_Reversible_Inference --
   ----------------------------

   procedure K_Reversible_Inference
     (A : in out Automata.Automaton'class;
      K : in Universal_Integer
      ; verbose : in Boolean := False
     )
   is
      -- dumping variables
      use type Ada.Calendar.Time;
      iter         : Positive := 1;
      target, into : Universal_Integer;
      size         : Universal_Integer;
      laps, total  : Duration := 0.0;
      start        : Ada.Calendar.Time;
      dumped       : Boolean := False;

      package AUIS renames Automata.UIS;
      use type AUIS.Cursor;
      function IsF(UI : in Universal_Integer) return Boolean renames A.Is_Final;
      function SP(L, R, Length : in Universal_Integer) return Boolean renames A.Share_Prefix;
      function ST(L, R : in Universal_Integer) return Boolean renames A.Share_Transition;

      modified   : Boolean;
      states, qf : AUIS.Set;
      L, R       : AUIS.cursor;
      fst_qf     : Universal_Integer;
      fst_states : Universal_Integer;
   begin
      if A.qf.Is_Empty or A.States.Is_Empty then
         if verbose then
            Ada.Text_IO.Put_Line("Automaton is empty.");
         end if;
         return;
      end if;

      fst_qf     := A.qf.First_Element;
      fst_states := A.States.First_Element;

      if verbose then
         Init(A);
      end if;

      -- using a "minimum backwarding" heuristic. If two nodes q1 and q2
      -- are merged in a certain phase with q1 < q2, we do not check any node
      -- qn such as qn < q1 ... Unless:
      --   | two nodes are merged in another phase (one of them might be q1)
      --   | no candidates we found while not starting from the beginning
      -- k-RI being insensitive to the order of merges, it does not affect
      -- the integrity of the algorithm and allows to amortise complexity.
      main:loop
         if verbose then
            start := Ada.Calendar.Clock;
         end if;

         -- Phase 1 : checking automaton determinism
         A.Merge_Determinization(modified);

         if verbose and modified then
            dumped := True;
         end if;

         if modified then
            fst_qf := A.qf.First_Element;
            fst_states := A.States.First_Element;
         else
            qf := A.qf;
         end if;

         -- Phase 2 : checking candidates in qf
         if not(modified) and then qf.Length > 1 then
            -- L := qf.First;
            L := qf.Find(fst_qf);
            while not(modified) and L < qf.Last loop
               R := AUIS.Next(L);
               while not(modified) and AUIS.Has_Element(R) loop
                  Target := AUIS.Element(R);
                  Into   := AUIS.Element(L);

                  if SP(Target, Into, k) then
                     A.Merge(Target, Into);
                     modified := True;
                     fst_qf   := Into;
                     fst_states := A.States.First_Element;
                  end if;

                  if not(modified) then
                     AUIS.Next(R);
                  end if;
               end loop;

               if not(modified) then
                  AUIS.Next(L);
               end if;
            end loop;
         end if;

         if not modified then
            states := A.States;
         end if;

         -- Phase 3: checking candidates that are not both final.
         -- Check on phase 3 being the same as in phase 2 but with an additional
         -- constraint, it is useless to check couples of final states, which
         -- would have been merged in phase 2.
         if not(modified) and then states.Length > 1 then
            -- L := states.First;
            L := states.Find(fst_states);
            while not(modified) and L < states.Last loop
               R := AUIS.Next(L);
               while not(modified) and AUIS.Has_Element(R) loop
                  Target := AUIS.Element(R);
                  Into   := AUIS.Element(L);

                  if not(IsF(Target) and IsF(Into)) then
                     if SP(Target, Into, k) and then ST(Target, Into) then
                        A.Merge(Target, Into);
                        modified := True;
                        fst_states := Into;
                        fst_qf := A.qf.First_Element;
                     end if;
                  end if;

                  if not(modified) then
                     AUIS.Next(R);
                  end if;
               end loop;

               if not(modified) then
                  AUIS.Next(L);
               end if;
            end loop;
         end if;

         -- The order of merges is not important, but some merges might change
         -- the result of Share_Prefix on *incoming* nodes, which therefore have
         -- to be checked to keep the integrity of the result when using the
         -- heuristic of "minimum backwarding".
         if not modified then
            if fst_qf /= A.qf.First_Element then
               fst_qf := A.qf.First_Element;
            elsif fst_states /= A.States.First_Element then
               fst_states := A.States.First_Element;
            else
               exit main;
            end if;
         end if;
         -- exit when not(modified);

         if verbose and modified then
            laps := Ada.Calendar.Clock - start;
            total := total + laps;
            size := A.Size;

            if dumped then
               Dump(iter, size, laps, total);
               dumped := False;
            else
               Dump(iter, target, into, size, laps, total);
            end if;

            Iter := Iter + 1;
         end if;
      end loop main;
   end K_Reversible_Inference;

end Gaal.Inference;
