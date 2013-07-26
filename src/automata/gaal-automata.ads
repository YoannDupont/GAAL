--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --                                                                         --
--                          file : gaal-automata.ads                          --
--                                                                            --
-- This package defines the root type for Automata.                           --
-- Automata defined here are Non-deterministic Finite Automata (NFA).         --
--                                                                            --
-- A NFA is a quintuple <Σ, ℚ, I, F, δ> such as:                              --
--   | Σ be a set of symbols                                                  --
--   | ℚ be a set of states                                                   --
--   | I ∈ 2**ℚ the set of initial states                                     --
--   | F ∈ 2**ℚ the set of final (accepting) states                           --
--   | δ : ℚ × Σ → 2**ℚ the non-deterministic transition function             --
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

with Ada.Containers.Ordered_Sets,
     Ada.Containers.Vectors,
     Ada.Finalization;

private with Ada.Strings.Unbounded;
-- Since it is not used in specification, the compiler suggests to move it to
-- the body. The reason why it is privately withed is because a withed entity in
-- the body is not withed in children packages. This method allows to with the
-- entity only once while remaining invisible to the user.

generic
   type Sigma is (<>);

   with function Image(S : in Sigma) return String is <>;

   with function Value(S : in String) return Sigma is <>;

package Gaal.Automata is
   package UIS is new Ada.Containers.Ordered_Sets(Element_Type => Universal_Integer);

   type Sigma_Word is array (Universal_Positive range <>) of Sigma;

   Epsilon : constant Sigma_Word;

   package Unbounded_Sigma_Words is new Ada.Containers.Vectors(Long_Positive, Sigma);

   function "<" (Left, Right : in Unbounded_Sigma_Words.Vector) return Boolean;

   package Bags_Of_Words is new
     Ada.Containers.Ordered_Sets
       (Unbounded_Sigma_Words.Vector,
        "<",
        Unbounded_Sigma_Words."=");

   type Automaton is abstract new Ada.Finalization.Controlled with private;

   type Automaton_Object is access all Automaton'Class;

   -----------------------------------------------------------------------------
   --                               Observators                               --
   -----------------------------------------------------------------------------

   function Image(A : in Automaton) return String;
   -- Returns a String representation of A.

   function Delta_Function
     (A : in Automaton;
      From : in Universal_Integer;
      By : in Sigma)
      return UIS.Set is abstract;
   -- Returns δ(A, From, By)

   function Incoming
     (A : in Automaton;
      To : in Universal_Integer)
      return UIS.Set is abstract;
   -- returns the Set of States containing every From such as:
   -- δ(A, From, _) -> To
   -- is defined

   function Is_Defined
     (A : in Automaton'class;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
      return Boolean;
   -- returns whether
   -- δ(A, From, By) -> To
   -- is defined

   function Is_Deterministic (A : in Automaton) return Boolean is abstract;
   -- Returns whether A is deterministic or not.
   --
   -- size(A.q0) = 1
   --   and
   -- for q ∈ ℚ ->
   --   for σ ∈ Σ ->
   --     size(δ(A, q, σ)) <= 1

   function Is_Final
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean is abstract;
   -- Returns whether A.qf contains UI

   function Is_Initial
     (A : in Automaton;
      UI : in Universal_Integer)
      return Boolean is abstract;
   -- Returns whether A.q0 contains UI

   function Outgoing
     (A : in Automaton;
      From : in Universal_Integer)
      return UIS.Set is abstract;
   -- returns the Set of States containing every To such as:
   -- δ(A, From, _) -> To
   -- is defined

   function q0(A : in Automaton)
               return UIS.Set is abstract;
   -- Returns A.q0

   function qf(A : in Automaton)
               return UIS.Set is abstract;
   -- Returns A.qf

   function Reverse_Delta_Function
     (A : in Automaton;
      To : in Universal_Integer;
      By : in Sigma)
      return UIS.Set is abstract;
   -- Returns the Set of States containing every From such as:
   -- δ(A, From, By) -> To
   -- is defined

   function Share_Prefix
     (A : in Automaton;
      Left, Right : in Universal_Integer;
      Length : in Universal_Integer)
      return Boolean is abstract;
   -- Returns
   -- ∃? a sequence of σ in Σ SEQ such as:
   -- SEQ = <σ_0, σ_1, ... , σ_Length>
   -- can lead to both Left and Right.
   --
   -- Returns:
   -- if Length = 0 then
   --   TRUE
   -- else
   --   ∃? σ ∈ Σ, q_1 and q_2 ∈ ℚ such as:
   --     δ(A, q_1, σ) ↦ Left
   --       and
   --     δ(A, q_2, σ) ↦ Right
   --       and
   --     Share_Prefix(A, q_1, q_2, Length - 1)

   function Share_Transition
     (A : in Automaton;
      Left, Right : in Universal_Integer)
      return Boolean is abstract;
   -- Returns
   -- ∃? σ ∈ Σ such as:
   --   δ(A, Left, σ) ∩ δ(A, Right, σ) /= ∅

   function States(A : in Automaton) return UIS.Set is abstract;
   -- Returns the Set of accessible States from A.q0

   -----------------------------------------------------------------------------
   --                                Modifiers                                --
   -----------------------------------------------------------------------------

   procedure Clear(A : in out Automaton) is abstract;
   -- Empties the Automaton

   procedure Make_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
   is abstract;
   -- Adds the transition
   -- δ(A, From, By) -> To

   procedure Merge
     (A : out Automaton;
      Target, Into : in Universal_Integer)
   is abstract;
   -- Merges Target into Into in the Automaton A.
   -- Merge is non-recursive for Automata that are not constrained to be
   -- deterministic, and is recursive for deterministic Automata to preserve
   -- their determinism.

   procedure Merge_Determinization
     (A : in out Automaton;
      modified : out Boolean)
   is abstract;
   -- Not equivalent to the "common" determinization algorithm.
   -- This procedure will merge every state violating automaton's deterministism
   -- the result being often an automaton recognizing a larger language than the
   -- non-deterministic one.

   procedure Remove_Transition
     (A : out Automaton;
      From : in Universal_Integer;
      By : in Sigma;
      To : in Universal_Integer)
   is abstract;
   -- Removes the transition
   -- δ(A, From, By) -> To

   procedure Set_Final
     (A : out Automaton;
      UI : in Universal_Integer)
   is abstract;
   -- Adds UI to A.qf

   procedure Set_Initial
     (A : out Automaton;
      UI : in Universal_Integer)
   is abstract;
   -- Adds UI A.q0

   procedure Unset_Final
     (A : out Automaton;
      UI : in Universal_Integer)
   is abstract;
   -- Removes UI from A.qf

   procedure Unset_Initial
     (A : out Automaton;
      UI : in Universal_Integer)
   is abstract;
   -- Removes UI from A.q0

   -----------------------------------------------------------------------------
   --                                  Class                                  --
   -----------------------------------------------------------------------------

   function Img(A : in Automaton'Class) return String;
   -- Returns the "minimum" String representation of A.
   -- It should contain the following:
   --   | The Alphabet
   --   | The set of accessible states
   --   | The set of initial states
   --   | The set of final states
   --   | The transition table

   function Is_Defined
     (A : in Automaton'class;
      From : in Universal_Integer;
      By : in Sigma)
      return Boolean;
   -- Returns whether δ(A, From, By) is non-empty

   function Matches
     (A : in Automaton'Class;
      Word : in Sigma_Word)
      return Boolean;
   -- Returns whether A can match Word entierely.

   function Matching_Sequences
     (A : in Automaton'Class;
      Word : in Sigma_Word;
      MS : in Matching_Strategy)
      return String;
   -- Returns a String containing every "matching sequence" found in Word by A
   -- in a sequential pass according to MS.
   -- A matching sequence is a String whose first character is 'B' (for Begin)
   -- and every other one is 'I' (for In). The end of a sequence is reached when
   -- the following character is an 'O' (for Out), a 'B' (beginning of another
   -- matching sequence) or then reaching the String's end.

   function Size(A : in Automaton'Class) return Universal_Integer;
   -- Return the Size of A in terms of accessible States.

   -----------------------------------------------------------------------------
   --                                  Other                                  --
   -----------------------------------------------------------------------------

   function To_Unbounded_Word(Word : in Sigma_Word) return Unbounded_Sigma_Words.Vector;

   function To_Word(USW : in Unbounded_Sigma_Words.Vector) return Sigma_Word;

   procedure Free(AO : in out Automaton_Object);

   -----------------------------------------------------------------------------
   --                               Exceptions                                --
   -----------------------------------------------------------------------------

   No_Initial,
   No_Final : Exception;

private
   package ASU renames Ada.Strings.Unbounded;

   Epsilon : constant Sigma_Word(1 .. 0) := (others => Sigma'First);

   type Automaton is abstract new Ada.Finalization.Controlled with null record;

end Gaal.Automata;
