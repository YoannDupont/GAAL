--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                        file : string_operations.adb                        --
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

with Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed;

package body String_Operations is

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   -----------
   -- Split --
   -----------

   function Split
     (Target  : in String;
      Pattern : in String)
      return Slice_Vector
   is
      Length : constant Natural      := Target'Length;
      Last   : constant Natural      := Target'Last;
      buffer : Slicing_Vector_Access := null;

      procedure Split(First : Natural; N : Positive)
      is
         Index : constant Natural := Ada.Strings.Fixed.Index(Target(First .. Last), Pattern);
      begin
         if Last < First then
            buffer         := new Slicing_Vector(Length, N - 1);
            buffer.all.Raw := Target;
            return;
         end if;

         if Index = 0 then
            buffer               := new Slicing_Vector(Length, N);
            buffer.all.Raw       := Target;
            buffer.all.Slices(N) := (First - Target'First + 1, Last - Target'First + 1);
         else
            Split(index + Pattern'Length, N + 1);
            buffer.all.Slices(N) := (First - Target'First + 1, index - Target'First);
         end if;
      end Split;
   begin
      Split(Target'First, 1);

      return (Ada.Finalization.Controlled with
                Element => buffer);
   end Split;

   ---------------
   -- Sub_Slice --
   ---------------

   function Sub_Slice
     (Target : in Slice_Vector;
      lo_nth : in Positive;
      hi_nth : in Positive)
      return Slice_Vector is
   begin
      if hi_nth < lo_nth
        or lo_nth > Length(Target)
        or hi_nth > Length(Target)
      then
         return Empty_Slice_Vector;
      end if;

      if lo_nth = 1 and hi_nth = Length(Target) then
         return Target;
      end if;

      declare
         S : Slicing_Vector(Target.Element.all.Slices(hi_nth).Last - Target.Element.all.Slices(lo_nth).First + 1,
                            hi_nth - lo_nth + 1);
         Shift : constant Natural := Target.Element.all.Slices(lo_nth).First - 1;
      begin
         S.Raw :=
           Target.Element.all.Raw
             (Target.Element.all.Slices(lo_nth).First
              .. Target.Element.all.Slices(hi_nth).Last);

         for I in 1 .. S.Number loop
            S.Slices(I).First := Target.Element.all.Slices(lo_nth + I - 1).First - Shift;
            S.Slices(I).Last := Target.Element.all.Slices(lo_nth + I - 1).Last - Shift;
         end loop;

         return (Ada.Finalization.Controlled with
                 Element => new Slicing_Vector'(S));
      end;
   end Sub_Slice;

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   ----------
   -- Join --
   ----------

   function Join
     (Target  : in Slice_Vector;
      Pattern : in String)
      return String
   is
      function Join(I : Positive) return String is
      begin
         if I = Target.Element.all.Number then
            return Nth(Target, I);
         else
            return Nth(Target, I) & Pattern & Join(I + 1);
         end if;
      end Join;
   begin
      if Length(Target) = 0 then
         return "";
      end if;

      return Join(1);
   end;

   ------------
   -- length --
   ------------

   function Length(Target : Slice_Vector) return Natural is
   begin
      if Target.Element /= null then
         return Target.Element.all.Number;
      else
         return 0;
      end if;
   end Length;

   ---------
   -- Nth --
   ---------

   function Nth
     (Target   : in Slice_Vector;
      Position : in Positive)
      return String
   is
      S : constant Slice := Target.Element.all.Slices(Position);
   begin
      return Target.Element.all.Raw(S.First .. S.Last);
   exception
      when others =>
         raise Constraint_Error with """" & Target.Element.all.Raw & """ " & Positive'Image(Target.Element.all.Raw'First) & Positive'Image(Target.Element.all.Raw'Last) & Positive'Image(Position) & Natural'Image(S.First) & Natural'Image(S.Last);
   end Nth;

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   -----------
   -- Split --
   -----------

   procedure Split
     (Input   : in String;
      Pattern : in String;
      Target  : out Slice_Vector)
   is
      Length : constant Natural      := Input'Length;
      Last   : constant Natural      := Input'Last;
      buffer : Slicing_Vector_Access := null;

      procedure Split(First : Natural; N : Positive)
      is
         Index : constant Natural := Ada.Strings.Fixed.Index(Input(First .. Last), Pattern);
      begin
         if Last < First then
            buffer         := new Slicing_Vector(Length, N - 1);
            buffer.all.Raw := Input;
            return;
         end if;

         if Index = 0 then
            buffer               := new Slicing_Vector(Length, N);
            buffer.all.Raw       := Input;
            buffer.all.Slices(N) := (First - Input'First + 1, Last - Input'First + 1);
         else
            Split(index + Pattern'Length, N + 1);
            buffer.all.Slices(N) := (First - Input'First + 1, index - Input'First);
         end if;
      end Split;
   begin
      Finalize(Target);
      Split(Input'First, 1);
      Target.Element := buffer;
   end Split;

   -----------------------------------------------------------------------------
   --                           private subprograms                           --
   -----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(Slicing_Vector,
                                                    Slicing_Vector_Access);

   procedure Adjust(Target : in out Slice_Vector) is
   begin
      if Target.Element /= null then
         Target.Element := new Slicing_Vector'(Target.Element.all);
      end if;
   end Adjust;

   procedure Finalize(Target : in out Slice_Vector) is
   begin
      if Target.Element /= null then
         Free(Target.Element);
      end if;
   end Finalize;
end String_Operations;
