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

with Gaal.Automata;

with Ada.Containers.Vectors;

generic
   type Sigma is (<>);

   with function Image(S : in Sigma) return String is <>;

   with function Value(S : in String) return Sigma is <>;

   with package Automata is new Gaal.Automata(Sigma,
                                              Image,
                                              Value);

package Presentations is
   package Presentations is new Ada.Containers.Vectors
     (Positive,
      Automata.Unbounded_Sigma_Words.Vector,
      Automata.Unbounded_Sigma_Words."=");

   type Weighted_Vector is record
      word : Automata.Unbounded_Sigma_Words.Vector := Automata.Unbounded_Sigma_Words.Empty_Vector;
      n_occur : Natural := 0;
   end record;

   function "="(Left, Right : in Weighted_Vector) return Boolean;

   package Weighted_Presentations is new Ada.Containers.Vectors
     (Positive,
      Weighted_Vector);

   procedure From_Presentation
     (A : in out Automata.Automaton'Class;
      P : in Presentations.Vector);
end Presentations;
