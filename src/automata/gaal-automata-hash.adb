--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                       file : gaal-automata-hash.adb                        --
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

package body Gaal.Automata.Hash is

   -----------------------------------------------------------------------------
   --                            local subprograms                            --
   -----------------------------------------------------------------------------

   -- Updates accessible states starting from a set of states.
   -- If at least one state is not already accessible, updating is abandoned.
   procedure Insert_From (A : in out Automaton; Init : UIS.Set)
   is
      use type UIS.Set;

      set    : UIS.Set;
      tmp    : UIS.Set;
      cursor : UIS.Cursor;
   begin
      if not Init.Is_Subset(A.States) then
         return;
      end if;

      set := Init;

      loop
         tmp    := set;
         cursor := set.First;

         while UIS.Has_Element(cursor) loop
            tmp.Union(A.Outgoing(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;

         tmp.Difference(A.States);

         exit when tmp.Is_Empty;

         A.states.Union(tmp);
         set := tmp;
      end loop;
   end Insert_From;

   --------------------
   -- Delta_Function --
   --------------------

   function Delta_Function
     (A : in Automaton;
      From : in Universal_Integer;
      By : in Sigma)
      return UIS.Set is
   begin
      if A.table.Contains(From) then
         if A.table.Element(From).Contains(By) then
            return A.table.Element(From).Element(By);
         else
            return UIS.Empty_Set;
         end if;
      else
         return UIS.Empty_Set;
      end if;
   end Delta_Function;

   --------------
   -- Incoming --
   --------------

   function Incoming
     (A : in Automaton;
      To : in Universal_Integer)
      return UIS.Set
   is
      result          : UIS.Set          := UIS.Empty_Set;
      states          : constant UIS.Set := A.States;
      cursor          : UIS.Cursor       := states.First;
      state           : Universal_Integer;
      symbol          : Sigma;
      found, continue : Boolean;
   begin
      while UIS.Has_Element(cursor) loop
         found    := False;
         continue := True;
         symbol   := Sigma'First;

         while not(found) and continue loop
            state := UIS.Element(cursor);
            found := A.Delta_Function(state, Symbol).Contains(To);
            if found then
               result.Include(state);
            end if;

            continue := not(found) and Symbol /= Sigma'Last;

            if continue then
               symbol := Sigma'Succ(symbol);
            end if;
         end loop;
         UIS.Next(cursor);
      end loop;

      return result;
   end Incoming;

   ----------------------
   -- Is_Deterministic --
   ----------------------

   function Is_Deterministic (A : in Automaton) return Boolean
   is
      use type Ada.Containers.Count_Type;

      states            : constant UIS.Set    := A.States;
      cursor            : UIS.Cursor          := states.First;
      checked, continue : Boolean             := True;
      I                 : Universal_Integer;
      J                 : Sigma;
   begin
      if A.q0.Length > 1 then
         return False;
      end if;

      while checked and UIS.Has_Element(cursor) loop
         I := UIS.Element(cursor);
         J := Sigma'First;

         while checked and continue loop
            checked := A.Delta_Function(I, J).Length <= 1;

            if J /= Sigma'Last then
               J := Sigma'Succ(J);
            else
               continue := False;
            end if;
         end loop;

         continue := True;

         UIS.Next(cursor);
      end loop;

      return checked;
   end Is_Deterministic;

   --------------
   -- Is_Final --
   --------------

   function Is_Final
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean is
   begin
      return A.qf.Contains(UI);
   end Is_Final;

   ----------------
   -- Is_Initial --
   ----------------

   function Is_Initial
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean is
   begin
      return A.q0.Contains(UI);
   end Is_Initial;

   --------------
   -- Outgoing --
   --------------

   function Outgoing
     (A : in Automaton;
      From : in Universal_Integer)
      return UIS.Set
   is
      checker : constant Q_To_Sigma.Cursor := A.table.Find(From);
   begin
      if not Q_To_Sigma.Has_Element(checker) then
         return UIS.Empty_Set;
      end if;

      declare
         table   : constant Sigma_To_Set.Map := Q_To_Sigma.Element(checker);
         cursor  : Sigma_To_Set.Cursor := table.First;
         result  : UIS.Set;
      begin
         while Sigma_To_Set.Has_Element(cursor) loop
            result.Union(Sigma_To_Set.Element(cursor));
            Sigma_To_Set.Next(cursor);
         end loop;

         return result;
      end;
   end Outgoing;

   --------
   -- q0 --
   --------

   function q0 (A : in Automaton) return UIS.Set is
   begin
      return A.q0;
   end q0;

   --------
   -- qf --
   --------

   function qf (A : in Automaton) return UIS.Set is
   begin
      return A.qf;
   end qf;

   ----------------------------
   -- Reverse_Delta_Function --
   ----------------------------

   function Reverse_Delta_Function
     (A : in Automaton;
      To : in Universal_Integer;
      By : in Sigma)
      return UIS.Set
   is
      result : UIS.Set          := UIS.Empty_Set;
      states : constant UIS.Set := A.States;
      cursor : UIS.Cursor       := states.First;
      from   : Universal_Integer;
   begin
      while UIS.Has_Element(cursor) loop
         from := UIS.Element(cursor);
         if A.Delta_Function(from, By).Contains(To) then
            result.Include(from);
         end if;
         UIS.Next(cursor);
      end loop;

      return result;
   end Reverse_Delta_Function;

   ------------------
   -- Share_Prefix --
   ------------------

   function Share_Prefix
     (A : in Automaton;
      Left, Right : in Universal_Integer;
      Length : in Universal_Integer)
      return Boolean
   is
      abandon : Boolean := False;
      found   : Boolean := False;
      LRDF    : UIS.Set := UIS.Empty_Set;
      RRDF    : UIS.Set := UIS.Empty_Set;
      L, R    : UIS.Cursor;
      S       : Sigma;
   begin
      if Length = 0 then
         return True;
      end if;

      S := Sigma'First;
      while not(abandon or found) loop
         LRDF := A.Reverse_Delta_Function(Left, S);
         RRDF := A.Reverse_Delta_Function(Right, S);

         if not(LRDF.Is_Empty or RRDF.Is_Empty) then
            L := LRDF.First;
            while not(found) and UIS.Has_Element(L) loop
               R := RRDF.First;
               while not(found) and UIS.Has_Element(R) loop
                  found := A.Share_Prefix(UIS.Element(L),
                                          UIS.Element(R),
                                          Length - 1);
                  UIS.Next(R);
               end loop;
               UIS.Next(L);
            end loop;
         end if;

         if not found then
            if S < Sigma'Last then
               S := Sigma'Succ(S);
            else
               abandon := True;
            end if;
         end if;
      end loop;

      return found;
   end Share_Prefix;

   ----------------------
   -- Share_Transition --
   ----------------------

   function Share_Transition
     (A : in Automaton;
      Left, Right : in Universal_Integer)
      return Boolean
   is
      buffer : UIS.Set;
   begin
      for S in Sigma loop
         buffer := UIS.Intersection(A.Delta_Function(Left, S), A.Delta_Function(Right, S));
         if not buffer.Is_Empty then
            return True;
         end if;
      end loop;

      return False;
   end Share_Transition;

   ------------
   -- States --
   ------------

   function States (A : in Automaton) return UIS.Set
   is
   begin
      return A.states;
   end States;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : in out Automaton) is
   begin
      A.states.Clear;
      A.q0.Clear;
      A.qf.Clear;
      A.table.Clear;
   end Clear;

   ---------------------
   -- Make_Transition --
   ---------------------

   procedure Make_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
   is
      procedure Add_Transition(Key : in Sigma; Element : in out UIS.Set)
      is
         pragma Unreferenced(Key);
      begin
         Element.Include(To);
      end Add_Transition;

      procedure Add(Key : in Universal_Integer; Element : in out Sigma_To_Set.Map)
      is
         pragma Unreferenced(Key);
         s_cursor : Sigma_To_Set.Cursor;
      begin
         s_cursor := Sigma_To_Set.Find(Element, By);
         if Sigma_To_Set.Has_Element(s_cursor) then
            Element.Update_Element(s_cursor, Add_Transition'Access);
         else
            Element.Include(By, UIS.To_Set(To));
         end if;
      end Add;

      q_cursor : constant Q_To_Sigma.Cursor := A.table.Find(From);
      buffer : Sigma_To_Set.Map;
   begin
      if Q_To_Sigma.Has_Element(q_cursor) then
         A.table.Update_Element(q_cursor, Add'Access);
      else
         buffer.Include(By, UIS.To_Set(To));
         A.table.Include(From, Buffer);
      end if;

--        A.states.Include(To);
      if A.States.Contains(From) then
         Insert_From(A, UIS.To_Set(From));
      end if;
   end Make_Transition;

   -----------
   -- Merge --
   -----------

   procedure Merge(A : out Automaton; Target, Into : in Universal_Integer)
   is
      use type UIS.Set;

      states   : constant UIS.Set := A.States;
      q_cursor : UIS.Cursor;
      element  : Universal_Integer;
      tmp      : UIS.Set;
   begin
      if Target = Into then
         return;
      end if;

      if A.Is_Initial(Target) then
         A.Set_Initial(Into);
         A.Unset_Initial(Target);
      end if;

      if A.Is_Final(Target) then
         A.Set_Final(Into);
         A.Unset_Final(Target);
      end if;

      for S in Sigma loop
         tmp      := A.Delta_Function(Target, S);
         q_cursor := tmp.First;
         while UIS.Has_Element(q_cursor) loop
            element := UIS.Element(q_cursor);
            A.Make_Transition(Into, S, element);
            A.Remove_Transition(Target, S, element);
            UIS.Next(q_cursor);
         end loop;

         if A.Delta_Function(Into, S).Contains(Target) then
            A.Make_Transition(Into, S, Into);
            A.Remove_Transition(Into, S, Target);
         end if;

         q_cursor := states.First;
         while UIS.Has_Element(q_cursor) loop
            element := UIS.Element(q_cursor);
            if A.Delta_Function(element, S).Contains(Target) then
               A.Make_Transition(element, S, Into);
               A.Remove_Transition(element, S, Target);
            end if;
            UIS.Next(q_cursor);
         end loop;
      end loop;

      A.table.Exclude(Target);
      A.states.Exclude(Target);
   end Merge;

   ---------------------------
   -- Merge_Determinization --
   ---------------------------

   procedure Merge_Determinization
     (A : in out Automaton;
      modified : out Boolean)
   is
      use type Ada.Containers.Count_Type;
      use type UIS.Cursor;

      states   : UIS.Set := A.States;
      cursor   : UIS.Cursor := states.First;
      continue : Boolean := states.Length > 1;
      changed  : Boolean := False;
      element  : Universal_Integer;
      to_merge : Universal_Integer;
   begin
      modified := False;

      while continue loop
         changed := False;
         element := UIS.Element(cursor);

         for S in Sigma loop
            while A.Delta_Function(element, S).Length > 1 loop
               to_merge := UIS.Element(UIS.Next(A.Delta_Function(element, S).First));

               A.Merge(to_merge,
                       UIS.Element(A.Delta_Function(element, S).First));
               states.Exclude(to_merge);

               changed := True;
               modified := True;
            end loop;
         end loop;

         if changed then
            cursor := states.First;
         elsif cursor < states.Last then
            UIS.Next(cursor);
         else
            continue := False;
         end if;
      end loop;
   end Merge_Determinization;

   -----------------------
   -- Remove_Transition --
   -----------------------

   procedure Remove_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
   is
      procedure Exclude(Key : Sigma; Element : in out UIS.Set)
      is
         pragma Unreferenced(Key);
      begin
         Element.Exclude(To);
      end Exclude;

      procedure Remove(Key : Universal_Integer; Element : in out Sigma_To_Set.Map)
      is
         pragma Unreferenced(Key);
         cursor : Sigma_To_Set.Cursor;
      begin
         cursor := Element.Find(By);

         if Sigma_To_Set.Has_Element(cursor) then
            Element.Update_Element(cursor, Exclude'Access);
         end if;
      end Remove;

      cursor : constant Q_To_Sigma.Cursor := A.table.Find(From);
   begin
      if Q_To_Sigma.Has_Element(cursor) then
         A.table.Update_Element(cursor, Remove'Access);
      end if;
   end Remove_Transition;

   ---------------
   -- Set_Final --
   ---------------

   procedure Set_Final (A : out Automaton; UI : in Universal_Integer) is
   begin
      A.qf.Include(UI);
   end Set_Final;

   -----------------
   -- Set_Initial --
   -----------------

   procedure Set_Initial (A : out Automaton; UI : in Universal_Integer) is
   begin
      A.q0.Include(UI);
      if not A.States.Contains(UI) then
         A.states.Include(UI);
         Insert_From(A, UIS.To_Set(UI));
      end if;
   end Set_Initial;

   -----------------
   -- Unset_Final --
   -----------------

   procedure Unset_Final (A : out Automaton; UI : in Universal_Integer) is
   begin
      A.qf.Exclude(UI);
   end Unset_Final;

   -------------------
   -- Unset_Initial --
   -------------------

   procedure Unset_Initial (A : out Automaton; UI : in Universal_Integer) is
   begin
      A.q0.Exclude(UI);
   end Unset_Initial;

   -----------------------------------------------------------------------------
   --                           private subprograms                           --
   -----------------------------------------------------------------------------

   ----------
   -- Hash --
   ----------

   function Hash (S : in Sigma) return Ada.Containers.Hash_Type is
   begin
      return Sigma'Pos(S);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (UI : in Universal_Integer) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type(UI);
   end Hash;

end Gaal.Automata.Hash;
