--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                               file : ihm.adb                               --
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

with Gaal.Automata.Hash.Prefix,
     Gaal.Automata.IO,
     Gaal.IO.Enumeration_IO,
     Gaal.Inference;
use Gaal;

with Ada.Containers,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Pretreatment,
     String_Operations;

procedure TUI is
   package TIO renames Ada.Text_IO;
   package S_O renames String_Operations;

   type Command_Type is
     (QUIT, RESTART, -- arity "zero" commands
      HELP,          -- arity "zero to one" commands
      KRI,           -- arity "one" commands
      WRITE,         -- arity "one to two" commands
      READ,          -- arity "two" commands
      APPLY          -- arity "ThreeToFour" commands
     );
   Invalid_Command,
   Invalid_Count    : Exception;

   subtype Arity_Zero is Command_Type range QUIT .. RESTART;
   subtype Arity_ZeroToOne is Command_Type range HELP .. HELP;
   subtype Arity_One is Command_Type range KRI .. KRI;
   subtype Arity_OneToTwo is Command_Type range WRITE .. WRITE;
   subtype Arity_Two is Command_Type range READ .. READ;
   subtype Arity_ThreeToFour is Command_Type range APPLY .. APPLY;

   type Format_Type is (PLAIN, DOT, PTA);
   subtype Out_Format is Format_Type range PLAIN .. DOT;

   function Arity_Of(Command : in Command_Type) return String is separate;
   function From_String(input : in String) return Command_Type is separate;
   procedure Display_Help is separate;
   procedure Help_For(Command : Command_Type) is separate;

   buffer : Ada.Strings.Unbounded.Unbounded_String;
begin
   <<BEGINNING>>
   for i in 1 .. 24 loop
      TIO.New_Line;
   end loop;
   TIO.Put_Line("------------------------------ Automata Interface ------------------------------");
   TIO.New_Line;

   correspondance:loop
      TIO.Put("Enter correpondance file: ");
      declare
         Answer : constant String := TIO.Get_Line;
      begin
         if Ada.Directories.Exists(Answer) then
            buffer := Ada.Strings.Unbounded.To_Unbounded_String(Answer);
            exit correspondance;
         else
            TIO.Put_Line("File does not exists: " & Answer);
         end if;
      end;
   end loop correspondance;

   declare
      -- declaring generic POS tags

      package Pret is new Pretreatment(Ada.Containers.Count_Type);

      Correspondance : constant Pret.Correspondance := Pret.Load_Correspondances(Ada.Strings.Unbounded.To_String(buffer));

      subtype Tag_Range is Ada.Containers.Count_Type range 1 .. Correspondance.Map.Length;

      function Image(T : Tag_Range) return String is
      begin
         return Pret.Get_Correspondance(T, Correspondance);
      end Image;

      function Value(S : String) return Tag_Range is
      begin
         return Pret.Get_Key(S, Correspondance);
      exception
         when Pret.No_Key => return Tag_Range'Value(S);
      end Value;

      function Valid(C : Character) return Boolean is
      begin
         return Pret.Is_Valid(C, Correspondance) or C in '0' .. '9';
      end Valid;

      package Tag_IO is new Gaal.IO.Enumeration_IO(Tag_Range, Image, Value, Valid'Access);

      -- Automata instanciations

      package Automata is new Gaal.Automata(Tag_Range, Image, Value);
      package Hash is new Automata.Hash;
      package Prefix is new Hash.Prefix;
      package Automata_IO is new Automata.IO(Tag_IO);
      Automaton : Automata.Automaton'Class := Prefix.Empty_Automaton;
      package Inference is new Gaal.Inference(Automata);

      ---------------
      -- Processes --
      ---------------

      procedure Process_ZeroToOne(Command : in Arity_ZeroToOne; Input : in String)
      is
         Cmd_Length : constant Positive := Command_Type'Image(Command)'Length + 2;
      begin
         case Command is
            when HELP =>
               if input'Length = Cmd_Length then
                  Display_Help;
               else
                  begin
                     Help_For(Command_Type'Value(Input(Input'First + Cmd_Length .. Input'Last)));
                  exception
                     when Constraint_Error =>
                        TIO.Put_Line("cannot display help, not a command: " & Input(Input'First + Cmd_Length + 1 .. Input'Last));
                        TIO.Put_Line("--------------------------------------------------------------------------------");
                  end;
               end if;
         end case;
      end Process_ZeroToOne;

      procedure Process_One(Command : in Arity_One; Argument : in String) is
      begin
         case Command is
         when KRI =>
            Inference.K_Reversible_Inference(Automaton, Universal_Integer'Value(Argument), True);
         end case;
      exception
         when Constraint_Error =>
            TIO.Put_Line("Not a natural: """ & Argument & """");
      end Process_One;

      procedure Process_OneToTwo(Command : in Arity_OneToTwo; Arguments : in S_O.Slice_Vector)
      is
         use type Ada.Directories.File_Kind;
         use type Ada.Containers.Count_Type;
      begin
         if not (S_O.Length(Arguments) in 1 .. 2) then
            raise Invalid_Count with "Wrong number of arguments, expected: " & Arity_Of(Command) & " got:" & Natural'Image(S_O.Length(Arguments));
         end if;

         case Command is
         when WRITE =>
            if S_O.Length(Arguments) = 1 then
               declare
                  Format : constant Out_Format := Out_Format'Value(S_O.Nth(Arguments, 1));
               begin
                  case Format is
                  when PLAIN => Automata_IO.Write(TIO.Standard_Output, Automaton);
                  when DOT => Automata_IO.Dot_Write(TIO.Standard_Output, Automaton);
                  end case;
               end;
               TIO.New_Line(TIO.Standard_Output);
            else
               declare
                  Left   : constant Out_Format := Out_Format'Value(S_O.Nth(Arguments, 1));
                  Right  : constant String     := S_O.Nth(Arguments, 2);
                  output : TIO.File_Type;
               begin
                  if Ada.Directories.Exists(Right) then
                     TIO.Put_Line("File already exists: " & Right);
                  else
                     TIO.Put_Line("Creating file: " & Right);
                     TIO.Create(output, TIO.Out_File, Right);

                     case Left is
                        when PLAIN => Automata_IO.Write(output, Automaton);
                        when DOT   => Automata_IO.Dot_Write(output, Automaton);
                     end case;

                     TIO.Close(output);
                     TIO.Put_Line("Done writing in file: " & Right);
                  end if;
               end;
            end if;
         end case;
      exception
         when e:Invalid_Count =>
            TIO.Put_Line(Ada.Exceptions.Exception_Message(e));
      end Process_OneToTwo;

      procedure Process_Two(Command : in Arity_Two; Arguments : in S_O.Slice_Vector)
      is
         use type Ada.Containers.Count_Type;
      begin
         if S_O.Length(Arguments) /= 2 then
            raise Invalid_Count with "Wrong number of arguments, expected: " & Arity_Of(Command) & " got:" & Natural'Image(S_O.Length(Arguments));
         end if;

         declare
            Left  : constant String := S_O.Nth(Arguments, 1);
            Right : constant String := S_O.Nth(Arguments, 2);
         begin
            case Command is
            when READ =>
               declare
                  file   : TIO.File_Type;
                  Format : constant Format_Type := Format_Type'Value(Left);
               begin
                  TIO.Put_Line("Accessing file: " & Right);
                  TIO.Open(file, TIO.In_file, Right);

                  case Format is
                  when PLAIN => Automata_IO.Read(file, Automaton);
                  when DOT   => Automata_IO.Dot_Read(file, Automaton);
                  when PTA   => Automata_IO.PTA(file, Automaton);
                  end case;

                  TIO.Close(file);
                  TIO.Put_Line("Done reading: " & Right);
               exception
                  when e:TIO.Name_Error =>
                     TIO.Put_Line(Ada.Exceptions.Exception_Message(e));
               end;
            end case;
         end;
      exception
         when e:Invalid_Count =>
            TIO.Put_Line(Ada.Exceptions.Exception_Message(e));
      end Process_Two;

      procedure Process_ThreeToFour(Command : in Arity_ThreeToFour; Arguments : in S_O.Slice_Vector)
      is
         use type Ada.Containers.Count_Type;
      begin
         if not(S_O.Length(Arguments) in 3 .. 4) then
            raise Invalid_Count with "Wrong number of arguments, expected: " & Arity_Of(Command) & " got:" & Natural'Image(S_O.Length(Arguments));
         end if;

         declare
            Fst : constant String := S_O.Nth(Arguments, 1);
            Snd : constant String := S_O.Nth(Arguments, 2);
            Trd : constant String := S_O.Nth(Arguments, 3);
         begin
            case Command is
            when APPLY =>
               if not Ada.Directories.Exists(Fst) then
                  TIO.Put_Line("File does not exists: " & Fst);
               elsif Ada.Directories.Exists(Snd) then
                  TIO.Put_Line("File already exists: " & Snd);
               else
                  declare
                     infile, outfile : TIO.File_Type;
                  begin
                     TIO.Open(infile, TIO.In_File, Fst);
                     TIO.Create(outfile, TIO.Out_File, Snd);

                     if S_O.Length(Arguments) = 3 then
                        Automata_IO.Apply(infile, outfile, Automaton, Matching_Strategy'Value(Trd));
                     else
                        Automata_IO.Apply
                          (infile,
                           outfile,
                           Automaton,
                           Matching_Strategy'Value(Trd),
                           S_O.Nth(Arguments, 4));
                     end if;

                     TIO.Close(infile);
                     TIO.Close(outfile);
                  end;
               end if;
            end case;
         end;
      exception
         when e:Invalid_Count =>
            TIO.Put_Line(Ada.Exceptions.Exception_Message(e));
      end Process_ThreeToFour;
   begin
      TIO.New_Line;
      Display_Help;

      input:loop
         TIO.Put("Enter command: ");
         declare
            Answer : constant String := TIO.Get_Line;
            command : Command_Type;
         begin
            command := From_String(Answer);

            -- Controller
            case command is
            when Arity_Zero =>
               case Arity_Zero(Command) is
                  when QUIT =>
                     Automaton.Clear;
                     goto ENDING;
                  when RESTART =>
                     Automaton.Clear;
                     goto BEGINNING;
               end case;
            when Arity_ZeroToOne =>
               Process_ZeroToOne
                 (Arity_ZeroToOne(Command),
                  Answer
                 );
            when Arity_One =>
               Process_One
                 (Arity_One(Command),
                  Answer(Answer'First + Command_Type'Image(command)'Length + 3 .. Answer'Last)
                 );
            when Arity_OneToTwo =>
               Process_OneToTwo
                 (Arity_OneToTwo(Command),
                  S_O.Split(Answer(Answer'First + Command_Type'Image(command)'Length + 3 .. Answer'Last), " ")
                 );
            when Arity_Two =>
               Process_Two
                 (Arity_Two(Command),
                  S_O.Split(Answer(Answer'First + Command_Type'Image(command)'Length + 3 .. Answer'Last), " ")
                 );
            when Arity_ThreeToFour =>
               Process_ThreeToFour
                 (Arity_ThreeToFour(Command),
                  S_O.Split(Answer(Answer'First + Command_Type'Image(command)'Length + 3 .. Answer'Last), " ")
                 );
            end case;
         exception
            when Invalid_Command =>
               TIO.Put_Line("Invalid command: " & Answer);
         end;
      end loop input;
   end;

   <<ENDING>>
   TIO.Put_Line("Bye bye !");
end TUI;
