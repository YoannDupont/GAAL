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

with Ada.Strings.Fixed;

package body String_Operations is

   package ASU renames Ada.Strings.Unbounded;
   package ASF renames Ada.Strings.Fixed;

   -------------
   -- Element --
   -------------

   function Element
     (Target : in Slice_Vectors.Vector;
      I      : in Positive)
      return String is
   begin
      return ASU.To_String(Target.Element(I));
   end Element;

   ----------
   -- Join --
   ----------

   function Join
     (Target  : in Slice_Vectors.Vector;
      Pattern : in String)
      return String
   is
      function Join(I : Positive) return String is
      begin
         if I = Target.Last_Index then
            return ASU.To_String(Target.Element(I));
         else
            return ASU.To_String(Target.Element(I)) & Pattern & Join(I + 1);
         end if;
      end Join;
   begin
      return Join(Target.First_Index);
   end Join;

   -----------
   -- Split --
   -----------

   function Split
     (Target  : in String;
      Pattern : in String)
      return Slice_Vectors.Vector
   is
      Last   : constant Natural := Target'Last;
      first  : Positive := Target'First;
      index  : Natural;
      result : Slice_Vectors.Vector;
   begin
      if Last < first then
         return result;
      end if;

      index := ASF.Index(Target(first .. Last), Pattern);
      while index /= 0 loop
         if index /= first then
            result.Append(ASU.To_Unbounded_String(Target(first .. index - 1)));
         end if;
         first := index + 1;

         index := ASF.Index(Target(first .. Last), Pattern);
      end loop;

      if first <= Last then
         result.Append(ASU.To_Unbounded_String(Target(first .. Last)));
      end if;

      return result;
   end Split;

   ---------------
   -- Sub_Slice --
   ---------------

   function Sub_Slice
     (Target : in Slice_Vectors.Vector;
      lo_nth : in Positive;
      hi_nth : in Positive)
      return Slice_Vectors.Vector
   is
      use type Ada.Containers.Count_Type;

      result : Slice_Vectors.Vector;
      lo     : constant Positive := Target.First_Index + lo_nth - 1;
      hi     : constant Positive := Target.First_Index + hi_nth - 1;
   begin
      if hi < lo
        or (result.Length < Ada.Containers.Count_Type(hi))
        or (result.Length < Ada.Containers.Count_Type(lo))
      then
         return result;
      end if;

      for I in lo .. hi loop
         result.Append(Target.Element(I));
      end loop;

      return result;
   end Sub_Slice;

end String_Operations;
