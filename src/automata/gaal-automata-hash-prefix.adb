--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                    file : gaal-automata-hash-prefix.adb                    --
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

package body Gaal.Automata.Hash.Prefix is

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   -----------
   -- Image --
   -----------

   function Image(Target : in Automaton) return String
   is
      prev_img : constant String  := Automata.Hash.Automaton(Target).Image;
      states   : constant UIS.Set := Target.States;
      cursor   : UIS.Cursor       := states.First;
      state    : Universal_Integer;
      prefixes : Bags_Of_Words.Set;
      pref_cur : Bags_Of_Words.Cursor;
      buffer   : Unbounded_Sigma_Words.Vector;
      res      : ASU.Unbounded_String;
   begin
      while UIS.Has_Element(cursor) loop
         state    := UIS.Element(cursor);
         prefixes := Tags(Target, State);

         if not prefixes.Is_Empty then
            ASU.Append(res, ASCII.LF & Image(State) & " {");
            pref_cur := prefixes.First;
            buffer   := Bags_Of_Words.Element(pref_cur);

            ASU.Append(res, Image(buffer.First_Element));
            for J in buffer.First_Index + 1 .. buffer.Last_Index loop
               ASU.Append(res, " " & Image(buffer.Element(J)));
            end loop;

            Bags_Of_Words.Next(pref_cur);
            while Bags_Of_Words.Has_Element(pref_cur) loop
               buffer := Bags_Of_Words.Element(pref_cur);

               ASU.Append(res, "," & Image(buffer.Element(buffer.First_Index)));
               for J in buffer.First_Index + 1 .. buffer.Last_Index loop
                  ASU.Append(res, " " & Image(buffer.Element(J)));
               end loop;

               Bags_Of_Words.Next(pref_cur);
            end loop;

            ASU.Append(res, "}");
         end if;

         UIS.Next(cursor);
      end loop;

      return Prev_Img & ASU.To_String(res);
   end Image;

   ------------------
   -- Share_Prefix --
   ------------------

   function Share_Prefix
     (A : in Automaton;
      Left, Right : in Universal_Integer;
      Length : in Universal_Integer)
      return Boolean is
   begin
      if Length = 0 then
         return True;
      end if;

      declare
         L_BoW : constant Bags_Of_Words.Set := Tags(A, Left);
         l_cur : Bags_Of_Words.Cursor       := L_BoW.First;
         R_BoW : constant Bags_Of_Words.Set := Tags(A, Right);
         r_cur : Bags_Of_Words.Cursor       := R_BoW.First;
         Shift : constant Universal_Integer := Length - 1;
         found : Boolean := False;
      begin
         while Bags_Of_Words.Has_Element(l_cur) and not found loop
            declare
               L : constant Sigma_Word := To_Word(Bags_Of_Words.Element(l_cur));
            begin
               while Bags_Of_Words.Has_Element(r_cur) and not found loop
                  declare
                     R : constant Sigma_Word := To_Word(Bags_Of_Words.Element(r_cur));
                  begin
                     if not(L'Length < Length or R'Length < Length) then
                        found := L(L'Last - Shift .. L'Last) = R(R'Last - Shift .. R'Last);
                     end if;
                  end;
                  Bags_Of_Words.Next(r_cur);
               end loop;
            end;
            Bags_Of_Words.Next(l_cur);
         end loop;

         return found;
      end;
   end Share_Prefix;

   ----------
   -- Tags --
   ----------

   function Tags
     (Target : in Automaton;
      State  : in Universal_Integer)
      return Bags_Of_Words.Set
   is
      cursor : constant Tag_Maps.Cursor := Target.tags.Find(State);
   begin
      if Tag_Maps.Has_Element(cursor) then
         return Tag_Maps.Element(cursor);
      else
         return Bags_Of_Words.Empty_Set;
      end if;
   end Tags;

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   -------------
   -- Add_Tag --
   -------------

   procedure Add_Tag
     (Target : in out Automaton;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word)
   is
      procedure Add_Tag(Key : in Universal_Integer; Element : in out Bags_Of_Words.Set)
      is
         pragma Unreferenced(Key);
      begin
         Element.Include(Automata.To_Unbounded_Word(Value));
      end Add_Tag;

      cursor : constant Tag_Maps.Cursor := Target.tags.Find(State);
   begin
      if Tag_Maps.Has_Element(cursor) then
         Target.tags.Update_Element(cursor, Add_Tag'Access);
      else
         Target.tags.Include(State, Bags_Of_Words.To_Set(To_Unbounded_Word(Value)));
      end if;
   end Add_Tag;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : in out Automaton) is
   begin
      Automata.Hash.Automaton(A).Clear;
      A.tags.Clear;
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
      from_tags : constant Bags_Of_Words.Set := Tags(A, From);
      cursor    : Bags_Of_Words.Cursor;
   begin
      Automata.Hash.Automaton(A).Make_Transition(From, By, To);

      if from_tags.Is_Empty then
         A.Add_Tag(To, (1 => By));
      else
         cursor := from_tags.First;
         while Bags_Of_Words.Has_Element(cursor) loop
            A.Add_Tag(To, To_Word(Bags_Of_Words.Element(cursor)) & By);
            Bags_Of_Words.Next(cursor);
         end loop;
      end if;
   end Make_Transition;

   -----------
   -- Merge --
   -----------

   procedure Merge (A : out Automaton; Target, Into : in Universal_Integer)
   is
      procedure Include(Key : Universal_Integer; Set : in out Bags_Of_Words.Set)
      is
         pragma Unreferenced(Key);
      begin
         Set.Union(Tags(A, Target));
      end Include;

      cursor : Tag_Maps.Cursor;
   begin
      Automata.Hash.Automaton(A).Merge(Target, Into);

      -- into may be initial, thus having no element in A.tags
      cursor := A.tags.Find(Into);
      if Tag_Maps.Has_Element(cursor) then
         A.tags.Update_Element(cursor, Include'Access);
      end if;

      A.Clear_Tags(Target);
   end Merge;

   -----------------------
   -- Remove_Transition --
   -----------------------

   procedure Remove_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
   is
   begin
      Automata.Hash.Automaton(A).Remove_Transition(From, By, To);
   end Remove_Transition;

   -----------------
   -- Set_Initial --
   -----------------

   procedure Set_Initial
     (A : out Automaton;
      UI : in Universal_Integer)
   is
   begin
      Automata.Hash.Automaton(A).Set_Initial(UI);
      A.Clear_Tags(UI);
   end Set_Initial;

   -------------------
   -- Unset_Initial --
   -------------------

   procedure Unset_Initial (A : out Automaton; UI : in Universal_Integer) is
   begin
      Automata.Hash.Automaton(A).Unset_Initial(UI);
   end Unset_Initial;

   ----------------
   -- Remove_Tag --
   ----------------

   procedure Remove_Tag
     (Target : in out Automaton;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word)
   is
      cursor : constant Tag_Maps.Cursor := Target.tags.Find(State);

      procedure Remove(Key : Universal_Integer; Set : in out Bags_Of_Words.Set)
      is
         pragma Unreferenced(Key);
         val : constant Unbounded_Sigma_Words.Vector := To_Unbounded_Word(Value);
      begin
         Set.Exclude(val);
      end Remove;
   begin
      if Tag_Maps.Has_Element(cursor) then
         Target.tags.Update_Element(cursor, Remove'Access);
      end if;
   end Remove_Tag;

   ----------------
   -- Clear_Tags --
   ----------------

   procedure Clear_Tags
     (Target : in out Automaton;
      State  : in Universal_Integer)
   is
      procedure Clear(Key : Universal_Integer; Set : in out Bags_Of_Words.Set)
      is
         pragma Unreferenced(Key);
      begin
         Set.Clear;
      end Clear;

      cursor : constant Tag_Maps.Cursor := Target.tags.Find(State);
   begin
      if Tag_Maps.Has_Element(cursor) then
         Target.tags.Update_Element(cursor, Clear'Access);
      end if;
   end Clear_Tags;

end Gaal.Automata.Hash.Prefix;
