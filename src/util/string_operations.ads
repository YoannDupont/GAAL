--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                        file : string_operations.ads                        --
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

private with Ada.Finalization;

package String_Operations is
   type Slice_Vector is private;
   -- A Slice Vector is a given String "tokenised" according to a certain
   -- pattern. In fact, it is a raw String with every sub-slices (an array
   -- of "ranges") found for a given pattern.

   Empty_Slice_Vector : constant Slice_Vector;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   function Split(Target  : in String; Pattern : in String) return Slice_Vector;

   function Sub_Slice
     (Target : in Slice_Vector;
      lo_nth : in Positive;
      hi_nth : in Positive)
      return Slice_Vector;

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   function Join(Target : in Slice_Vector; Pattern : in String) return String;

   function Length(Target : Slice_Vector) return Natural;

   function Nth(Target : in Slice_Vector; Position : in Positive) return String;

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   procedure Split
     (Input   : in String;
      Pattern : in String;
      Target  : out Slice_Vector);

private
   type Slice is record
      First : Positive;
      Last  : Natural;
   end record;

   type Slice_Array is array (Positive range <>) of Slice;

   type Slicing_Vector(Length, Number : Natural) is record
      Raw    : String(1 .. Length);
      Slices : Slice_Array(1 .. Number);
   end record;

   type Slicing_Vector_Access is access Slicing_Vector;

   type Slice_Vector is new Ada.Finalization.Controlled with record
      Element : Slicing_Vector_Access;
   end record;

   procedure Adjust(Target : in out Slice_Vector);

   procedure Finalize(Target : in out Slice_Vector);

   Empty_Slice_Vector : constant Slice_Vector :=
     (Ada.Finalization.Controlled with
      Element => null);
end String_Operations;

