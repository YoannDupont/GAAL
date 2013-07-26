--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                       file : gaal-automata-hash.ads                        --
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

private with Ada.Containers.Hashed_Maps;

generic
package Gaal.Automata.Hash is
   type Automaton is new Automata.Automaton with private;

   Empty_Automaton : constant Automaton;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   -----------------------------------------------------------------------------
   --                               Observators                               --
   -----------------------------------------------------------------------------

   function Delta_Function
     (A : in Automaton;
      From : in Universal_Integer;
      By : in Sigma)
      return UIS.Set;

   function Incoming
     (A : in Automaton;
      To : in Universal_Integer)
      return UIS.Set;

   function Is_Deterministic(A : in Automaton) return Boolean;

   function Is_Final
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean;

   function Is_Initial
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean;

   function Outgoing
     (A : in Automaton;
      From : in Universal_Integer)
      return UIS.Set;

   function q0(A : in Automaton) return UIS.Set;

   function qf(A : in Automaton) return UIS.Set;

   function Reverse_Delta_Function
     (A : in Automaton;
      To : in Universal_Integer;
      By : in Sigma)
      return UIS.Set;

   function Share_Prefix
     (A : in Automaton;
      Left, Right : in Universal_Integer;
      Length : in Universal_Integer)
      return Boolean;

   function Share_Transition
     (A : in Automaton;
      Left, Right : in Universal_Integer)
      return Boolean;

   function States(A : in Automaton) return UIS.Set;

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   procedure Clear(A : in out Automaton);

   procedure Make_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer);

   procedure Merge(A : out Automaton; Target, Into : in Universal_Integer);

   procedure Merge_Determinization(A : in out Automaton; modified : out Boolean);

   procedure Remove_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer);

   procedure Set_Final(A : out Automaton; UI : in Universal_Integer);

   procedure Set_Initial(A : out Automaton; UI : in Universal_Integer);

   procedure Unset_Final(A : out Automaton; UI : in Universal_Integer);

   procedure Unset_Initial(A : out Automaton; UI : in Universal_Integer);

private
   function Hash(S : in Sigma) return Ada.Containers.Hash_Type;
   function Hash(UI : in Universal_Integer) return Ada.Containers.Hash_Type;

   package Sigma_To_Set is new Ada.Containers.Hashed_Maps(Key_Type        => Sigma,
                                                          Element_Type    => UIS.Set,
                                                          Hash            => Hash,
                                                          Equivalent_Keys => "=",
                                                          "="             => UIS."=");

   package Q_To_Sigma is new Ada.Containers.Hashed_Maps(Key_Type        => Universal_Integer,
                                                        Element_Type    => Sigma_To_Set.Map,
                                                        Hash            => Hash,
                                                        Equivalent_Keys => "=",
                                                        "="             => Sigma_To_Set."=");

   type Automaton is new Automata.Automaton with record
      states : UIS.Set        := UIS.Empty_Set;
      q0     : UIS.Set        := UIS.Empty_Set;
      qf     : UIS.Set        := UIS.Empty_Set;
      table  : Q_To_Sigma.Map := Q_To_Sigma.Empty_Map;
   end record;

   Empty_Automaton : constant Automaton :=
     (Ada.Finalization.Controlled with
      states => UIS.Empty_Set,
      q0     => UIS.Empty_Set,
      qf     => UIS.Empty_Set,
      table  => Q_To_Sigma.Empty_Map
     );

   pragma Inline(Is_Final);
   pragma Inline(Is_Initial);
   pragma Inline(q0);
   pragma Inline(qf);
   pragma Inline(States);
   pragma Inline(Set_Final);
   pragma Inline(Unset_Final);
   pragma Inline(Unset_Initial);
   pragma Inline(Hash);

end Gaal.Automata.Hash;
