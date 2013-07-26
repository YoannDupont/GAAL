--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --                                                                         --
--                          file : gaal-automata.adb                          --
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

with Ada.Exceptions,
     Ada.Unchecked_Deallocation;

package body Gaal.Automata is

   -----------------------------------------------------------------------------
   --                               Observators                               --
   -----------------------------------------------------------------------------

   -----------
   -- Image --
   -----------

   function Image(A : in Automaton) return String is
   begin
      return Img(A);
   end;

   -----------------------------------------------------------------------------
   --                                  Class                                  --
   -----------------------------------------------------------------------------

   ---------
   -- Img --
   ---------

   function Img(A : in Automaton'Class) return String
   is
      states   : constant UIS.Set := A.States;
      q0       : constant UIS.Set := A.q0;
      qf       : constant UIS.Set := A.qf;
      cursor   : UIS.Cursor := UIS.No_Element;
      delta_fn : UIS.Set;
      delta_cu : UIS.Cursor := UIS.No_Element;
      is_first : Boolean;
      auto_img : ASU.Unbounded_String;

      function Sep(Is_First : in Boolean) return String is
      begin
         if Is_First then
            return "";
         else
            return ",";
         end if;
      end Sep;
   begin
      ASU.Append(auto_img, "{" & Image(Sigma'First));
      for S in Sigma'Succ(Sigma'First) .. Sigma'Last loop
         ASU.Append(auto_img, "," & Image(S));
      end loop;

      ASU.Append(auto_img, "}" & ASCII.LF & "{");

      if not states.Is_Empty then
         ASU.Append(auto_img, Image(states.First_Element));
         cursor := UIS.Next(states.First);
         while UIS.Has_Element(cursor) loop
            ASU.Append(auto_img, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      ASU.Append(auto_img, "}" & ASCII.LF & "{");

      if not q0.Is_Empty then
         ASU.Append(auto_img, Image(q0.First_Element));
         cursor := UIS.Next(q0.First);
         while UIS.Has_Element(cursor) loop
            ASU.Append(auto_img, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      ASU.Append(auto_img, "}" & ASCII.LF & "{");

      if not qf.Is_Empty then
         ASU.Append(auto_img, Image(qf.First_Element));
         cursor := UIS.Next(qf.First);
         while UIS.Has_Element(cursor) loop
            ASU.Append(auto_img, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      ASU.Append(auto_img, "}");
      cursor := states.First;

      while UIS.Has_Element(cursor) loop
         is_first := True;
         for By in Sigma loop
            delta_fn := A.Delta_Function(UIS.Element(cursor), By);
            if not delta_fn.Is_Empty then
               if is_first then
                  ASU.Append(auto_img, ASCII.LF & Image(UIS.Element(cursor)) & " {");
               end if;

               ASU.Append(auto_img,
                          sep(is_first) & Image(By) & ":{" & Image(delta_fn.First_Element));
               delta_cu := UIS.Next(delta_fn.First);

               while UIS.Has_Element(delta_cu) loop
                  ASU.Append(auto_img, "," & Image(UIS.Element(delta_cu)));
                  UIS.Next(delta_cu);
               end loop;

               ASU.Append(auto_img, "}");
               is_first := False;
            end if;
         end loop;

         if not is_first then
            ASU.Append(auto_img, "}");
         end if;

         UIS.Next(cursor);
      end loop;

      return ASU.To_String(auto_img);
   end Img;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (A : in Automaton'class;
      From : in Universal_Integer;
      By : in Sigma)
      return Boolean is
   begin
      return not A.Delta_Function(From, By).Is_Empty;
   end Is_Defined;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (A : in Automaton'class;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
      return Boolean is
   begin
      return A.Delta_Function(From, By).Contains(To);
   end Is_Defined;

   -------------
   -- Matches --
   -------------

   function Matches
     (A : in Automaton'Class;
      Word : in Sigma_Word)
      return Boolean
   is
      use type UIS.Set;

      states   : UIS.Set := A.q0;
      current  : UIS.Cursor;
      tmp      : UIS.Set := UIS.Empty_Set;
      index    : Universal_Integer := Word'First;
      continue : Boolean := index <= Word'Last and not(states.Is_Empty);
   begin
      while continue loop
         current := states.First;
         while UIS.Has_Element(current) loop
            UIS.Union(tmp,
                      A.Delta_Function(UIS.Element(current),
                                       Word(index)));
            UIS.Next(current);
         end loop;

         states := tmp;
         tmp.Clear;

         continue := not(states.Is_Empty) and index < Word'Last;

         if continue then
            index := index + 1;
         end if;
      end loop;

      states := states and A.qf;

      return not states.Is_Empty;
   end Matches;

   ------------------------
   -- Matching_Sequences --
   ------------------------

   function Matching_Sequences
     (A : in Automaton'Class;
      Word : in Sigma_Word;
      MS : in Matching_Strategy)
      return String
   is
      use type UIS.Set;

      matcher : String(1 .. Word'Length) := (others => 'O');

      procedure Short_Matching_Sequences
      is
         first, last : Universal_Integer := Word'First;
         states      : UIS.Set           := A.q0;
         current     : Universal_Integer := 0;
         tmp         : UIS.Set           := UIS.Empty_Set;
         found       : Boolean           := False;
         cursor      : UIS.Cursor;
      begin
         while last <= Word'Last loop
            cursor := states.First;
            while UIS.Has_Element(cursor) loop
               UIS.Union(tmp, A.Delta_Function(UIS.Element(cursor),
                                               Word(Last)));
               UIS.Next(cursor);
            end loop;

            if tmp.Is_Empty then
               first  := first + 1;
               last   := first;
               states := A.q0;
            else
               cursor := tmp.First;
               found  := False;

               while UIS.Has_Element(cursor) and not found loop
                  current := UIS.Element(cursor);
                  found   := A.Is_Final(current);

                  if found then
                     matcher(matcher'First + Positive(first) - Positive(Word'First)) := 'B';
                     matcher
                       (matcher'First + Positive(first) - Positive(Word'First) + 1
                        .. matcher'First + Positive(last) - Positive(Word'First)
                       ) := (others => 'I');
                     first := last + 1;
                     last := first;
                  else
                     UIS.Next(cursor);
                  end if;
               end loop;

               if found then
                  states := A.q0;
               else
                  states := tmp;
                  last := last + 1;
               end if;

               tmp.Clear;
            end if;
         end loop;
      end Short_Matching_Sequences;

      procedure Long_Matching_Sequences
      is
         length,                           -- global length of the matching sequence
         n       : Universal_Integer := 0; -- length of the "append candidate" matching sequence
         states  : UIS.Set           := A.q0;
         first,
         last    : Universal_Integer := Word'First;
         tmp     : UIS.Set;
         found   : Boolean;
         cursor  : UIS.Cursor;
      begin
         while last <= Word'Last loop
            cursor := states.First;
            while UIS.Has_Element(cursor) loop
               UIS.Union(tmp, A.Delta_Function(UIS.Element(cursor),
                                               Word(Last)));
               UIS.Next(cursor);
            end loop;

            if tmp.Is_Empty then
               if length = 0 then
                  first := first + 1;
               else
                  matcher(matcher'First + Positive(first) - Positive(Word'First)) := 'B';
                  matcher
                    (matcher'First + Positive(first) - Positive(Word'First) + 1
                     .. matcher'First + Positive(first) - Positive(Word'First) + Positive(length) - 1
                    ) := (others => 'I');
                  first := first + length;
                  length := 0;
                  tmp.Clear;
               end if;

               last := first;
               n := 0;
               states := A.q0;
            else
               cursor := tmp.First;
               found  := False;
               n      := n + 1;

               while UIS.Has_Element(cursor) and not found loop
                  found := A.Is_Final(UIS.Element(cursor));

                  if found then
                     if last = Word'Last then
                        matcher(matcher'First + Positive(first) - Positive(Word'First)) := 'B';
                        matcher
                          (matcher'First + Positive(first) - Positive(Word'First) + 1
                           .. matcher'Last) := (others => 'I');
                     else
                        length := n;
                     end if;
                  else
                     UIS.Next(cursor);
                  end if;
               end loop;

               states := tmp;
               tmp.Clear;
               last := last + 1;
            end if;
         end loop;
      end Long_Matching_Sequences;
   begin
      if MS = Short_Match then
         Short_Matching_Sequences;
      else
         Long_Matching_Sequences;
      end if;

      return matcher;
   end Matching_Sequences;

   ----------
   -- Size --
   ----------

   function Size(A : in Automaton'Class) return Universal_Integer is
   begin
      return Universal_Integer(A.States.Length);
   end Size;

   -----------------------------------------------------------------------------
   --                                  Other                                  --
   -----------------------------------------------------------------------------

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : in Unbounded_Sigma_Words.Vector) return Boolean is
      use type System.Address;
   begin
      if Left'Address = Right'Address then
         return False;
      end if;

      return To_Word(Left) < To_Word(Right);
   end "<";

   ----------
   -- Free --
   ----------

   procedure Deallocate is
     new Ada.Unchecked_Deallocation(Automaton'Class, Automaton_Object);

   procedure Free(AO : in out Automaton_Object) is
   begin
      Deallocate(AO);
   end Free;

   -----------------------
   -- To_Unbounded_Word --
   -----------------------

   function To_Unbounded_Word(Word : in Sigma_Word) return Unbounded_Sigma_Words.Vector
   is
      result : Unbounded_Sigma_Words.Vector := Unbounded_Sigma_Words.To_Vector(Word'Length);
      holder : Long_Positive;
      first : constant Long_Natural := Long_Positive(Word'First) - 1;
   begin
      for I in Word'Range loop
         holder := Long_Positive(I) - first;
         result.Replace_Element(holder, Word(I));
      end loop;

      return result;
   end To_Unbounded_Word;

   -------------
   -- To_Word --
   -------------

   function To_Word(USW : in Unbounded_Sigma_Words.Vector) return Sigma_Word
   is
      result : Sigma_Word(1 .. Universal_Integer(USW.Length));
      index  : Universal_Integer            := result'First;
      cursor : Unbounded_Sigma_Words.Cursor := USW.First;
   begin
      while Unbounded_Sigma_Words.Has_Element(cursor) loop
         result(index) := Unbounded_Sigma_Words.Element(cursor);
         index         := index + 1;
         Unbounded_Sigma_Words.Next(cursor);
      end loop;

      return result;
   end To_Word;

end Gaal.Automata;
