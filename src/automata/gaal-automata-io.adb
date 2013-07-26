--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --                                                                      --
--                        file : gaal-automata-io.adb                         --
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

with Ada.Exceptions;

package body Gaal.Automata.IO is
   package TIO renames Ada.Text_IO;
   package IO renames Gaal.IO;
   package AE renames Ada.Exceptions;
   package UI_IO is new TIO.Modular_IO(Universal_Integer);

   package USW renames Unbounded_Sigma_Words;

   -----------------------------------------------------------------------------
   --                              local methods                              --
   -----------------------------------------------------------------------------

   -------------------
   -- Get_Character --
   -------------------

   procedure Get_Character
     (File : in TIO.File_Type;
      To_Read : in Character;
      Read : out Character;
      EOL : out Boolean)
   is
   begin
      TIO.Look_Ahead(File, Read, EOL);

      if EOL then
         return;
      end if;

      if Read = To_Read then
         TIO.Get(File, Read);
      else
         declare
            Line : constant String := TIO.Count'Image(TIO.Line(File));
            Col : constant String := TIO.Count'Image(TIO.Col(File));
         begin
            raise Unexpected_Character with
            TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
              & Col(Col'First + 1 .. Col'Last) & ") : "
              & Character'Image(Read);
         end;
      end if;
   end Get_Character;

   -----------------------------------------------------------------------------
   --                             package methods                             --
   -----------------------------------------------------------------------------

   ----------
   -- Read --
   ----------

   procedure Read(File : in TIO.File_Type; A : out Automaton'Class)
   is
      use type TIO.Positive_Count;

      eol      : Boolean := True;
      symbol   : Sigma;
      state    : Universal_Integer;
      c        : Character;
      states   : Automata.UIS.Set;
      outgoing : Automata.UIS.Set;
      q0       : Automata.UIS.Set;
      qf       : Automata.UIS.Set;
      cursor   : UIS.Cursor;

      function Get_Token return String
      is
         function End_Marker(C : in Character) return Boolean is
         begin
            return C = '}' or C = ',';
         end End_Marker;

         EMP : constant access function(C : in Character) return Boolean := End_Marker'Access;
      begin
         return IO.Get_Token(File, EMP);
      end Get_Token;

      procedure Get_States(Target : out UIS.Set; Within : in UIS.Set := UIS.Empty_Set) is
         char : Character;
         endl : Boolean;
         current : Universal_Integer;
      begin
         UIS.Clear(Target);
         Get_Character(File, '{', char, endl);

         while not endl loop
            UI_IO.Get(File, current);

            if not UIS.Is_Empty(Within) then
               if not UIS.Contains(Within, current) then
                  raise Unreachable_State;
               end if;
            end if;

            UIS.Insert(target, current);
            TIO.Look_Ahead(File, char, endl);

            case char is
            when '}' =>
               TIO.Get(File, char);
               endl := True;
            when ',' => TIO.Get(File, char);
            when others =>
               declare
                  Line : constant String := TIO.Count'Image(TIO.Line(File));
                  Col : constant String := TIO.Count'Image(TIO.Col(File));
               begin
                  raise Unexpected_Character with
                  TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                    & Col(Col'First + 1 .. Col'Last) & ") : "
                    & Character'Image(char);
               end;
            end case;
         end loop;
      exception
         when Unreachable_State =>
            declare
               Repr : constant String := Image(current);
               Line : constant String := TIO.Count'Image(TIO.Line(File));
               Col  : constant String := TIO.Count'Image(TIO.Col(File) - Repr'Length);
            begin
               raise Unreachable_State with
               TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                 & Col(Col'First + 1 .. Col'Last) & ") : "
                 & Image(current);
            end;
         when E:Constraint_Error => -- inserting element already in Set
            declare
               Repr : constant String := Image(current);
               Line : constant String := TIO.Count'Image(TIO.Line(File));
               Col  : constant String := TIO.Count'Image(TIO.Col(File) - Repr'Length);
            begin
               AE.Raise_Exception
                 (AE.Exception_Identity(E),
                  AE.Exception_Message(E) & ". " & TIO.Name(File) & " ("
                    & Line(Line'First + 1 .. Line'Last) & ":"
                    & Col(Col'First + 1 .. Col'Last) & ") : "
                    & Image(current));
            end;
      end Get_States;
   begin
      A.Clear;
      Get_Character(File, '{', c, eol);

      -- there may or may not be a Sigma enumeration to read.
      -- If there is one, the line can be skipped.
      -- Otherwise, it means we have a state that should be memorized.
      declare
         token : constant String := Get_Token;
      begin
         symbol := Automata.Value(Token);
         eol := True;
         TIO.Skip_Line(File);
      exception
         when Constraint_Error =>
            eol := False;
            state := Universal_Integer'Value(token);
      end;

      -- reading the various sets of states

      if eol then
         Get_Character(File, '{', c, eol);
         UI_IO.Get(File, state);
      end if;

      UIS.Insert(states, state);

      TIO.Look_Ahead(File, c, eol);

      while not eol loop
         case c is
            when '}' => eol := True;
            when ',' => TIO.Get(File, c);
            when others =>
            declare
               Line : constant String := TIO.Count'Image(TIO.Line(File));
               Col : constant String := TIO.Count'Image(TIO.Col(File));
            begin
               raise Unexpected_Character with
               TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                 & Col(Col'First + 1 .. Col'Last) & ") : "
                 & Character'Image(c);
            end;
         end case;

         if not eol then
            UI_IO.Get(File, state);
            UIS.Insert(states, state);
            TIO.Look_Ahead(File, c, eol);
         end if;
      end loop;

      TIO.Skip_Line(File);
      Get_States(q0, states);
      TIO.Skip_Line(File);
      Get_States(qf, states);

      cursor := q0.First;
      while UIS.Has_Element(cursor) loop
         A.Set_Initial(UIS.Element(cursor));
         UIS.Next(cursor);
      end loop;

      cursor := qf.First;
      while UIS.Has_Element(cursor) loop
         A.Set_Final(UIS.Element(cursor));
         UIS.Next(cursor);
      end loop;

      -- reading transitions

      TIO.Skip_Line(File);
      eol := False;

      while not eol loop
         UI_IO.Get(File, state);
         Get_Character(File, ' ', c, eol);
         Get_Character(File, '{', c, eol);

         while not eol loop
            Sigma_IO.Get(File, symbol);
            Get_Character(File, ':', c, eol);
            Get_States(outgoing, states);
            cursor := outgoing.First;
            while UIS.Has_Element(cursor) loop
               A.Make_Transition(state, symbol, UIS.Element(cursor));
               UIS.Next(cursor);
            end loop;
            TIO.Look_Ahead(File, c, eol);
            case c is
            when '}' =>
               TIO.Get(File, c);
               eol := True;
               if not TIO.End_Of_File(File) then
                  TIO.Skip_Line(File);
               end if;
               -- reading next table
            when ',' =>
               TIO.get(File,c);
               -- reading next couple symbol:{outgoing_states}
            when others =>
               declare
                  Line : constant String := TIO.Count'Image(TIO.Line(File));
                  Col : constant String := TIO.Count'Image(TIO.Col(File));
               begin
                  raise Unexpected_Character with
                  TIO.Name(File) & " (" & Line(Line'First + 1 .. Line'Last) & ":"
                    & Col(Col'First + 1 .. Col'Last) & ") : "
                    & Character'Image(c);
               end;
            end case;
         end loop;
         TIO.Look_Ahead(File, c, eol);
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write(File : in TIO.File_Type; A : in Automaton'Class)
   is
      states   : constant UIS.Set := A.States;
      q0       : constant UIS.Set := A.q0;
      qf       : constant UIS.Set := A.qf;
      cursor   : UIS.Cursor := UIS.No_Element;
      delta_cu : UIS.Cursor := UIS.No_Element;
      delta_fn : UIS.Set;
      is_first : Boolean;

      function Sep(Is_First : in Boolean) return String is
      begin
         if Is_First then
            return "";
         else
            return ",";
         end if;
      end Sep;
   begin
      TIO.Put(File, "{" & Image(Sigma'First));
      for S in Sigma'Succ(Sigma'First) .. Sigma'Last loop
         TIO.Put(File, "," & Image(S));
      end loop;

      TIO.Put_Line(File, "}");
      TIO.Put(File, "{");

      if not states.Is_Empty then
         TIO.Put(File, Image(states.First_Element));
         cursor := UIS.Next(states.First);
         while UIS.Has_Element(cursor) loop
            TIO.Put(File, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      TIO.Put_Line(File, "}");
      TIO.Put(File, "{");

      if not q0.Is_Empty then
         TIO.Put(File, Image(q0.First_Element));
         cursor := UIS.Next(q0.First);
         while UIS.Has_Element(cursor) loop
            TIO.Put(File, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      TIO.Put_Line(File, "}");
      TIO.Put(File, "{");

      if not qf.Is_Empty then
         TIO.Put(File, Image(qf.First_Element));
         cursor := UIS.Next(qf.First);
         while UIS.Has_Element(cursor) loop
            TIO.Put(File, "," & Image(UIS.Element(cursor)));
            UIS.Next(cursor);
         end loop;
      end if;

      TIO.Put(File, "}");
      cursor := states.First;

      while UIS.Has_Element(cursor) loop
         is_first := True;
         for By in Sigma loop
            delta_fn := A.Delta_Function(UIS.Element(cursor), By);
            if not delta_fn.Is_Empty then
               if is_first then
                  TIO.New_Line(File);
                  TIO.Put(File, Image(UIS.Element(cursor)) & " {");
               end if;

               TIO.Put(File, sep(is_first) & Image(By) & ":{" & Image(delta_fn.First_Element));
               delta_cu := UIS.Next(delta_fn.First);

               while UIS.Has_Element(delta_cu) loop
                  TIO.Put(File, "," & Image(UIS.Element(delta_cu)));
                  UIS.Next(delta_cu);
               end loop;

               TIO.Put(File, "}");
               is_first := False;
            end if;
         end loop;

         if not is_first then
            TIO.Put(File, "}");
         end if;

         UIS.Next(cursor);
      end loop;
   end Write;

   ---------
   -- PTA --
   ---------

   procedure PTA(File : in TIO.File_Type; A : out Automaton'Class)
   is
      use type TIO.File_Mode;

      current : Universal_Integer  := 0;           -- current State in PTA
      first   : Universal_Positive := 1;           -- first free State
      symbol  : Sigma              := Sigma'First;
      eol     : Boolean            := False;
      c       : Character;
   begin
      if TIO.Mode(File) /= TIO.In_File then
         raise TIO.Status_Error;
      end if;

      A.Clear;
      A.Set_Initial(current);

      while not (TIO.End_Of_File(File) or eol) loop
         Sigma_IO.Get(File, symbol);

         if A.Is_Defined(current, symbol) then
            current := A.Delta_Function(current, symbol).First_Element;
         else
            A.Make_Transition(current, symbol, first);
            current := first;
            first   := first + 1;
         end if;

         Get_Character(File, ' ', c, eol);

         if eol then
            A.Set_Final(current);
            current := 0;

            if not TIO.End_Of_File(File) then
               TIO.Skip_Line(File);
               TIO.Look_Ahead(File, c, eol);
            end if;
         end if;
      end loop;
   end PTA;

   ---------------
   -- Dot_Write --
   ---------------

   procedure Dot_Write(File : in TIO.File_Type; A : in Automaton'Class)
   is
      states   : constant UIS.Set := A.States;
      q0       : constant UIS.Set := A.q0;
      qf       : constant UIS.Set := A.qf;
      cursor   : UIS.Cursor := UIS.No_Element;
      delta_cu : UIS.Cursor := UIS.No_Element;
      delta_fn : UIS.Set;
      from     : Universal_Integer;
   begin
      TIO.Put_Line(File, "digraph g {");

      cursor := q0.First;
      while UIS.Has_Element(cursor) loop
         TIO.Put_Line(File, Image(UIS.Element(cursor)) & " [shape=diamond];");
         UIS.Next(cursor);
      end loop;

      cursor := qf.First;
      while UIS.Has_Element(cursor) loop
         TIO.Put_Line(File, Image(UIS.Element(cursor)) & " [color=red];");
         UIS.Next(cursor);
      end loop;

      cursor := states.First;
      while UIS.Has_Element(cursor) loop
         from := UIS.Element(cursor);
         for By in Sigma loop
            delta_fn := A.Delta_Function(from, By);
            delta_cu := delta_fn.First;

            while UIS.Has_Element(delta_cu) loop
               TIO.Put_Line(File, Image(from) & " -> " & Image(UIS.Element(delta_cu)) & " [label=""" & Image(By) & """];");
               UIS.Next(delta_cu);
            end loop;
         end loop;
         UIS.Next(cursor);
      end loop;

      TIO.Put(File, "}");
   end Dot_Write;

   --------------
   -- Dot_Read --
   --------------

   procedure Dot_Read(File : in Ada.Text_IO.File_Type; A : in out Automaton'Class)
   is
      use type TIO.Count;

      from, to : Universal_Integer;
      c        : Character;
      eol, eof : Boolean;
   begin
      A.Clear;

      TIO.Skip_Line(File); -- first line : "digraph g {"
      TIO.Look_Ahead(File, c, eol);

      if c = '}' then
         eof := True;
      else
         eof := False;
      end if;

      while not eof loop
         UI_IO.Get(File, from);
         Get_Character(File, ' ', c, eol);

         TIO.Look_Ahead(File, c, eol);
         if c = '[' then -- we are describing the state
            declare
               Line : constant string := TIO.Get_Line(File);
            begin
               if line = "[shape=diamond];" then -- initial state
                  A.Set_Initial(from);
               elsif line = "[color=red];" then -- final state
                  A.Set_Final(from);
               else
                  -- in this case, exception message is the faulty input
                  declare
                     Line_n : constant String := TIO.Count'Image(TIO.Line(File) - 1);
                     Col    : constant String := Positive'Image(1 + Universal_Integer'Image(from)'Length);
                  begin
                     raise Bad_Format with
                       "Unexpected input in " & TIO.Name(File) & " ("
                        & Line_n(Line_n'First + 1 .. Line_n'Last) & ":" & Col(Col'First + 1 .. Col'Last) & ") : """
                        & Line & """";
                  end;
               end if;
            end;
         elsif c = '-' then -- we are describing a transition
            Get_Character(File, '-', c, eol);
            Get_Character(File, '>', c, eol);
            Get_Character(File, ' ', c, eol);
            UI_IO.Get(File, to);
            Get_Character(File, ' ', c, eol);
            declare
               Line : constant string := TIO.Get_Line(File);
            begin
               A.Make_Transition
                 (from,
                  Automata.Value(line(line'First + 8 .. line'Last - 3)), -- [label="symbol"]; ==> " have to be trimmed also
                  to);
            exception
               when Constraint_Error =>
                  -- in this case, exception message is the faulty input
                  declare
                     Line_n : constant String := TIO.Count'Image(TIO.Line(File) - 1);
                     Col    : constant String := Positive'Image(4 + Universal_Integer'Image(from)'Length + Universal_Integer'Image(to)'Length);
                  begin
                     raise Bad_Format with
                        "Unexpected input in " & TIO.Name(File) & " ("
                        & Line_n(Line_n'First + 1 .. Line_n'Last) & ":" & Col(Col'First + 1 .. Col'Last) & ") : """
                        & Line & """";
                  end;
            end;
         else
            raise Program_Error;
         end if;

         eof := TIO.End_Of_File(File);
         if not eof then
            TIO.Look_Ahead(File, c, eol);
            if c = '}' then
               eof := True;
            end if;
         end if;
      end loop;
   end Dot_Read;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (In_File   : in Ada.Text_IO.File_Type;
      Out_File  : in Ada.Text_IO.File_Type;
      A         : in Automata.Automaton'Class;
      Strategy  : in Matching_Strategy;
      Appendice : in String := "")
   is
      sentence_buffer : Buffers.Vector := Buffers.Empty_Vector;
      after_buffer    : Buffers.Vector := Buffers.Empty_Vector;
      word_buffer     : USW.Vector     := USW.Empty_Vector;
      c               : Character      := ASCII.NUL;
      eol             : Boolean        := False;
      sigma_token     : Sigma          := Sigma'First;
      skipping        : Boolean        := True;
      not_first       : Boolean        := False;

      procedure Output
      is
         w       : constant Sigma_Word := To_Word(word_buffer);
         matches : constant String     := A.Matching_Sequences(w, Strategy);
      begin
         for I in sentence_buffer.First_Index .. sentence_buffer.Last_Index loop
            TIO.Put(Out_File, ASU.To_String(sentence_buffer.Element(I)));
            TIO.Put(Out_File, ASCII.HT);
            Sigma_IO.Put(Out_File, word_buffer.Element(word_buffer.First_Index + Gaal.Long_Positive(I) - 1));

            if not after_buffer.Is_Empty then
               TIO.Put(Out_File, ASU.To_String(after_buffer.Element(after_buffer.First_Index + I - 1)));
            end if;

            TIO.Put(Out_File, ASCII.HT);
            TIO.Put(Out_File, matches(matches'First + I - 1));
            if Appendice'Length > 0 and matches(matches'First + I - 1) /= 'O' then
               TIO.Put(Out_File, Appendice);
            end if;
            TIO.New_Line(Out_File);
         end loop;
      end Output;
   begin
      loop
         TIO.Look_Ahead(In_File, c, eol);

         if eol then -- empty line <=> sentence end <=> writing
            if not_first then
               TIO.New_Line(Out_File);
            else
               not_first := True;
            end if;

            Output;

            sentence_buffer.Clear;
            word_buffer.Clear;
            after_buffer.Clear;
         else -- reading
            sentence_buffer.Append(ASU.To_Unbounded_String(IO.Get_Token(In_File))); -- word
            IO.Get_Character(In_File, ASCII.HT, c, eol);
            Sigma_IO.Get(In_File, sigma_token); -- POS
            word_buffer.Append(sigma_token);

            if TIO.End_Of_Line(In_File) then
               skipping := True;
            else
               after_buffer.Append(ASU.To_Unbounded_String(TIO.Get_Line(In_File))); -- other informations
               skipping := False;
            end if;
         end if;

         if TIO.End_Of_File(In_File) then
            if not word_buffer.Is_Empty then
               TIO.New_Line(Out_File);
               Output;
            end if;
            exit;
         else
            if skipping then
               TIO.Skip_Line(In_File);
            end if;
            skipping := True;
         end if;
      end loop;

      sentence_buffer.Clear;
      word_buffer.Clear;
   end Apply;

end Gaal.Automata.IO;
