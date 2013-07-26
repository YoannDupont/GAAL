--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : pretreatment.ads                           --
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

with Ada.Containers.Hashed_Maps,
     Ada.Strings.Unbounded;

generic
   type Range_Type is range <>;
package Pretreatment is
   function Hash(R : Range_Type) return Ada.Containers.Hash_Type;

   package Range_To_Str is new Ada.Containers.Hashed_Maps
     (Key_Type        => Range_Type,
      Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Ada.Strings.Unbounded."=");

   type Valid_Characters is array(Character'Range) of Boolean;
   for Valid_Characters'Component_Size use 1;

   type Correspondance is limited record
      Map : Range_To_Str.Map;
      Valid : Valid_Characters := (others => False);
   end record;

   function Load_Correspondances(Name : String) return Correspondance;

   function Get_Correspondance(RT : Range_Type; Corr : Correspondance) return String;

   function Get_Key(Str : String; Corr : Correspondance) return Range_Type;

   function Is_Valid(C : Character; Corr : Correspondance) return Boolean;

   No_Key : Exception;
end Pretreatment;
