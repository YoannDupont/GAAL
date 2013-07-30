--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                    file : gaal-automata-hash-prefix.ads                    --
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

with Gaal.Automata.State_Tags;

generic
package Gaal.Automata.Hash.Prefix is
   package S_T is new State_Tags;

   type Automaton is new Automata.Hash.Automaton and S_T.State_Tagging with private;
   -- This automaton will keep track of the shortest prefixes when built as a PTA.

   Empty_Automaton : constant Automaton;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   function Image(Target : in Automaton) return String;

   function Share_Prefix
     (A : in Automaton;
      Left, Right : in Universal_Integer;
      Length : in Universal_Integer)
      return Boolean;

   function Tags
     (Target : in Automaton;
      State  : in Universal_Integer)
     return Bags_Of_Words.Set;

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   procedure Add_Tag
     (Target : in out Automaton;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word);

   procedure Clear(A : in out Automaton);

   procedure Make_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer);

   procedure Merge(A : out Automaton; Target, Into : in Universal_Integer);

   procedure Remove_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer);

   procedure Set_Initial
     (A : out Automaton;
      UI : in Universal_Integer);

   procedure Unset_Initial(A : out Automaton; UI : in Universal_Integer);

   procedure Remove_Tag
     (Target : in out Automaton;
      State  : in     Universal_Integer;
      Value  : in     Sigma_Word);

   procedure Clear_Tags
     (Target : in out Automaton;
      State  : in Universal_Integer);

private
   package Tag_Maps is new Ada.Containers.Hashed_Maps(Key_Type        => Universal_Integer,
                                                      Element_Type    => Bags_Of_Words.Set,
                                                      Hash            => Hash,
                                                      Equivalent_Keys => "=",
                                                      "="             => Bags_Of_Words."=");

   type Automaton is new Automata.Hash.Automaton and S_T.State_Tagging with record
      tags : Tag_Maps.Map := Tag_Maps.Empty_Map;
   end record;

   Empty_Automaton : constant Automaton :=
     (Ada.Finalization.Controlled with
      states => UIS.Empty_Set,
      q0     => UIS.Empty_Set,
      qf     => UIS.Empty_Set,
      table  => Q_To_Sigma.Empty_Map,
      tags  => Tag_Maps.Empty_Map
     );

end Gaal.Automata.Hash.Prefix;
