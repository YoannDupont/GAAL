--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                         file : file_processing.ads                         --
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

with Chunks.IO;
pragma Elaborate_All(Chunks.IO);

with Ada.Exceptions,
     Ada.Wide_Characters.Handling;

package body File_Processing is
   package TIO renames Ada.Text_IO;
   package LIO renames Gaal.IO;
   package Chunk_IO is new Chunking.IO;

   package PP renames POS_Presentations;
   package Pr renames Presentations;
   package WPr renames Weighted_Presentations;

   package ASU renames Ada.Strings.Unbounded;
   package AUSW renames Automata.Unbounded_Sigma_Words;
   package AE renames Ada.Exceptions;

   ---------------
   -- Increment --
   ---------------

   procedure Increment(WPV : in out PP.Weighted_Vector) is
   begin
      WPV.n_occur := WPV.n_occur + 1;
   end Increment;

   ----------------------
   -- Get_Presentation --
   ----------------------

   procedure Get_Presentation
     (File : in Ada.Text_IO.File_Type;
      P    : out Presentation)
   is
      c : Character;
      eol : Boolean;
      tag : POS;
      chk : Chunking.Chunk;
      current : Chunking.Identifier := Chunking.Identifier'First;
      cursor : Pr.Cursor;
      vector : USW.Vector;

      use type Pr.Cursor;
      use type Chunking.Identifier;
      use type Ada.Text_IO.Count;
   begin
      loop
         TIO.Look_Ahead(File, c, eol);

         if not eol then
            -- word token is ignored
            declare
               buffer : String := Gaal.IO.Get_Token(File);
               pragma Unreferenced(buffer);
            begin
               null;
            end;

            Gaal.IO.Get_Character(File, ASCII.HT, c, eol);
            POS_IO.Get(File, tag);
            Gaal.IO.Get_Character(File, ASCII.HT, c, eol);
            Chunk_IO.Get(File, chk);

            case chk.prefix is
               when Chunking.B =>
                  if not USW.Is_Empty(vector) then
                     cursor := Pr.Find(P(current), vector);

                     if cursor = Pr.No_Element then
                        Pr.Append(P(current), vector);
                     end if;

                     USW.Clear(vector);
                  end if;

                  current := chk.id;
                  USW.Append(vector, tag);

               when Chunking.I =>
                  if current /= chk.id then
                     raise BAD_FORMAT;
                  end if;

                  current := chk.id;
                  USW.Append(vector, tag);

               when Chunking.O =>
                  if not USW.Is_Empty(vector) then
                     cursor := Pr.Find(P(current), vector);

                     if cursor = Pr.No_Element then
                        Pr.Append(P(current), vector);
                     end if;

                     USW.Clear(vector);
                  end if;
            end case;
         end if;

         if not TIO.End_Of_File(File) then
            TIO.Skip_Line(File);
         else
            cursor := Pr.Find(P(current), vector);

            if cursor = Pr.No_Element then
               Pr.Append(P(current), vector);
            end if;

            USW.Clear(vector);

            exit;
         end if;
      end loop;
   exception
      when e:BAD_FORMAT =>
         declare
            expected  : constant String := Chunking.Image(current);
            erroneous : constant String := Chunking.Image(chk.id);
            Line : constant String := TIO.Count'Image(TIO.Line(File));
            Col  : constant String := TIO.Count'Image(TIO.Col(File) - erroneous'Length);
         begin
            AE.Raise_Exception
              (AE.Exception_Identity(E),
               AE.Exception_Message(E) & ". " & TIO.Name(File) & " ("
               & Line(Line'First + 1 .. Line'Last) & ":"
               & Col(Col'First + 1 .. Col'Last) & ")."
               & " Expected: " & expected
               & " Erroneous: " & erroneous);
         end;
   end Get_Presentation;

   -------------------------------
   -- Get_Weighted_Presentation --
   -------------------------------

   procedure Get_Weighted_Presentation
     (File : in Ada.Text_IO.File_Type;
      WP   : out Weighted_Presentation)
   is
      c : Character;
      eol : Boolean;
      tag : POS;
      chk : Chunking.Chunk;
      current : Chunking.Identifier := Chunking.Identifier'First;
      cursor : WPr.Cursor;
      vector : PP.Weighted_Vector;

      use type WPr.Cursor;
      use type Chunking.Identifier;
      use type Ada.Text_IO.Count;
   begin
      loop
         TIO.Look_Ahead(File, c, eol);

         if not eol then
            -- word token is ignored
            declare
               buffer : String := Gaal.IO.Get_Token(File);
               pragma Unreferenced(buffer);
            begin
               null;
            end;

            Gaal.IO.Get_Character(File, ASCII.HT, c, eol);
            POS_IO.Get(File, tag);
            Gaal.IO.Get_Character(File, ASCII.HT, c, eol);
            Chunk_IO.Get(File, chk);

            case chk.prefix is
               when Chunking.B =>
                  if not USW.Is_Empty(vector.word) then
                     cursor := WPr.Find(WP(current), vector);

                     if cursor = WPr.No_Element then
                        WPr.Append(WP(current), (vector.word, 1));
                     else
                        WPr.Update_Element(WP(current), cursor, Increment'Access);
                     end if;

                     USW.Clear(vector.word);
                  end if;

                  current := chk.id;
                  USW.Append(vector.word, tag);

               when Chunking.I =>
                  if current /= chk.id then
                     raise BAD_FORMAT;
                  end if;

                  current := chk.id;
                  USW.Append(vector.word, tag);

               when Chunking.O =>
                  if not USW.Is_Empty(vector.word) then
                     cursor := WPr.Find(WP(current), vector);

                     if cursor = WPr.No_Element then
                        WPr.Append(WP(current), (vector.word, 1));
                     else
                        WPr.Update_Element(WP(current), cursor, Increment'Access);
                     end if;

                     USW.Clear(vector.word);
                  end if;
            end case;
         end if;

         if TIO.End_Of_File(File) then
            exit;
         else
            TIO.Skip_Line(File);
         end if;
      end loop;
   exception
      when e:BAD_FORMAT =>
         declare
            expected  : constant String := Chunking.Image(current);
            erroneous : constant String := Chunking.Image(chk.id);
            Line : constant String := TIO.Count'Image(TIO.Line(File));
            Col  : constant String := TIO.Count'Image(TIO.Col(File) - erroneous'Length);
         begin
            AE.Raise_Exception
              (AE.Exception_Identity(E),
               AE.Exception_Message(E) & ". " & TIO.Name(File) & " ("
               & Line(Line'First + 1 .. Line'Last) & ":"
               & Col(Col'First + 1 .. Col'Last) & ")."
               & " Expected: " & expected
               & " Erroneous: " & erroneous);
         end;
   end Get_Weighted_Presentation;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (In_File  : in Ada.Text_IO.File_Type;
      Out_File : in Ada.Text_IO.File_Type;
      A        : in Automata.Automaton'Class;
      Strategy : in Gaal.Matching_Strategy)
   is
      sentence_buffer : Buffers.Vector := Buffers.Empty_Vector;
      chunk_buffer    : Buffers.Vector := Buffers.Empty_Vector;
      after_buffer    : Buffers.Vector := Buffers.Empty_Vector;
      word_buffer     : AUSW.Vector    := AUSW.Empty_Vector;
      c               : Character      := ASCII.NUL;
      eol             : Boolean        := False;
      pos_token       : POS            := POS'First;
      skipping        : Boolean        := True;
   begin
      loop
         TIO.Look_Ahead(In_File, c, eol);

         if eol then -- empty line <=> sentence end <=> writing
            declare
               w : constant Automata.Sigma_Word := Automata.To_Word(word_buffer);
               matches : constant String := A.Matching_Sequences(w, Strategy);
            begin
               for I in sentence_buffer.First_Index .. sentence_buffer.Last_Index loop
                  TIO.Put(Out_File, ASU.To_String(sentence_buffer.Element(I)));
                  TIO.Put(Out_File, ASCII.HT);
                  POS_IO.Put(Out_File, word_buffer.Element(word_buffer.First_Index + Gaal.Long_Positive(I) - 1));
                  TIO.Put(Out_File, ASCII.HT);
                  TIO.Put(Out_File, ASU.To_String(chunk_buffer.Element(chunk_buffer.First_Index + I - 1)));

                  if not after_buffer.Is_Empty then
                     TIO.Put(Out_File, ASU.To_String(after_buffer.Element(after_buffer.First_Index + I - 1)));
                  end if;

                  TIO.Put(Out_File, ASCII.HT);
                  TIO.Put(Out_File, matches(matches'First + I - 1));
                  TIO.New_Line(Out_File);
               end loop;
               TIO.New_Line(Out_File);
            end;

            sentence_buffer.Clear;
            chunk_buffer.Clear;
            word_buffer.Clear;
            after_buffer.Clear;
         else -- reading
            sentence_buffer.Append(ASU.To_Unbounded_String(LIO.Get_Token(In_File))); -- word
            LIO.Get_Character(In_File, ASCII.HT, c, eol);
            POS_IO.Get(In_File, pos_token); -- POS
            word_buffer.Append(pos_token);
            LIO.Get_Character(In_File, ASCII.HT, c, eol);
            chunk_buffer.Append(ASU.To_Unbounded_String(LIO.Get_Token(In_File))); -- chunk

            if TIO.End_Of_Line(In_File) then
               skipping := True;
            else
               after_buffer.Append(ASU.To_Unbounded_String(TIO.Get_Line(In_File))); -- other informations
               skipping := False;
            end if;
         end if;

         if TIO.End_Of_File(In_File) then
            exit;
         else
            if skipping then
               TIO.Skip_Line(In_File);
            end if;
            skipping := True;
         end if;
      end loop;

      sentence_buffer.Clear;
      chunk_buffer.Clear;
      word_buffer.Clear;
   end Apply;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (In_File  : in Ada.Text_IO.File_Type;
      Out_File : in Ada.Text_IO.File_Type;
      A        : in Automata_Array;
      Strategy : in Gaal.Matching_Strategy)
   is
      use type Chunking.Identifier;

      sentence_buffer : Buffers.Vector := Buffers.Empty_Vector;
      chunk_buffer    : Buffers.Vector := Buffers.Empty_Vector;
      word_buffer     : AUSW.Vector    := AUSW.Empty_Vector;
      after_buffer    : Buffers.Vector := Buffers.Empty_Vector;
      c               : Character      := ASCII.NUL;
      eol             : Boolean        := False;
      pos_token       : POS            := POS'First;
      token_number    : Natural        := 0; -- length of matching sequence. Bounded Strings can be put in an array
      skipping        : Boolean        := True;
   begin
      loop
         TIO.Look_Ahead(In_File, c, eol);

         if eol then -- empty line <=> sentence end <=> writing
            declare
               w : constant Automata.Sigma_Word := Automata.To_Word(word_buffer);
               matches : array(Chunking.Identifier'Range) of String(1 .. token_number);
            begin
               for ID in Chunking.Identifier loop
                  matches(ID) := A(ID).all.Matching_Sequences(w, Strategy);
               end loop;

               for I in sentence_buffer.First_Index .. sentence_buffer.Last_Index loop
                  TIO.Put(Out_File, ASU.To_String(sentence_buffer.Element(I)));
                  TIO.Put(Out_File, ASCII.HT);
                  POS_IO.Put(Out_File, word_buffer.Element(word_buffer.First_Index + Gaal.Long_Positive(I) - 1));
                  TIO.Put(Out_File, ASCII.HT);
                  TIO.Put(Out_File, ASU.To_String(chunk_buffer.Element(chunk_buffer.First_Index + I - 1)));

                  if not after_buffer.Is_Empty then
                     TIO.Put(Out_File, ASU.To_String(after_buffer.Element(after_buffer.First_Index + I - 1)));
                  end if;

                  for ID in Chunking.Identifier loop
                     if ID /= Chunking.None then
                        TIO.Put(Out_File, ASCII.HT);
                        TIO.Put(Out_File, matches(ID)(matches(ID)'First + I - 1));
                     end if;
                  end loop;

                  TIO.New_Line(Out_File);
               end loop;

               TIO.New_Line(Out_File);
            end;

            sentence_buffer.Clear;
            chunk_buffer.Clear;
            word_buffer.Clear;
            token_number := 0;
         else -- reading
            sentence_buffer.Append(ASU.To_Unbounded_String(LIO.Get_Token(In_File))); -- word
            LIO.Get_Character(In_File, ASCII.HT, c, eol);
            POS_IO.Get(In_File, pos_token); -- POS
            word_buffer.Append(pos_token);
            LIO.Get_Character(In_File, ASCII.HT, c, eol);
            chunk_buffer.Append(ASU.To_Unbounded_String(LIO.Get_Token(In_File))); -- chunk

            if TIO.End_Of_Line(In_File) then
               skipping := True;
            else
               after_buffer.Append(ASU.To_Unbounded_String(TIO.Get_Line(In_File))); -- other informations
               skipping := False;
            end if;

            token_number := token_number + 1;
         end if;

         if TIO.End_Of_File(In_File) then
            exit;
         else
            if skipping then
               TIO.Skip_Line(In_File);
            end if;
            skipping := True;
         end if;
      end loop;

      sentence_buffer.Clear;
      chunk_buffer.Clear;
      word_buffer.Clear;
   end Apply;
end File_Processing;
