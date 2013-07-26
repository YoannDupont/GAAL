--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                               file : pos.ads                               --
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

package POS is
   pragma Elaborate_Body;

   type TAGS is
     (ADJ, ADJWH, ADV, ADVWH, CC, CS, CL, CLO, CLR, CLS, DET, DETWH, ET, I, NC,
      NPP, P, PONCT, P_PLUS_D, P_PLUS_PRO, PREF, PRO, PROREL, PROWH, V, VPR, VS,
      VINF, VIMP, VPP);

   function Image(T : in TAGS) return String;

   function Value(S : in String) return TAGS;

   function Valid(C : in Character) return Boolean;
end POS;
