--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : presentations.ads                          --
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

package body Presentations is

   package USW renames Automata.Unbounded_Sigma_Words;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Weighted_Vector) return Boolean is
      use type Automata.Unbounded_Sigma_Words.Vector;
   begin
      return Left.word = Right.word;
   end "=";

   -----------------------
   -- From_Presentation --
   -----------------------

   procedure From_Presentation
     (A : in out Automata.Automaton'Class;
      P : in Presentations.Vector)
   is
      use type Gaal.Universal_Integer;

      index : Gaal.Universal_Integer := 0;
      first_free : Gaal.Universal_Positive := 1;
      symbol : Sigma;
      current : USW.Vector;
   begin
      A.Clear;
      A.Set_Initial(index);

      for I in Presentations.First_Index(P) .. Presentations.Last_Index(P) loop
         current := Presentations.Element(P, I);
         index := 0;

         for J in USW.First_Index(current) .. USW.Last_Index(current) loop
            symbol := USW.Element(current, J);

            if A.Is_Defined(index, symbol) then
               index := A.Delta_Function(index, symbol).First_Element;
            else
               A.Make_Transition(index, symbol, first_free);
               index := first_free;
               first_free := first_free + 1;
            end if;

            if J = USW.Last_Index(current) then
               A.Set_Final(index);
            end if;
         end loop;
      end loop;
   end From_Presentation;

end Presentations;
