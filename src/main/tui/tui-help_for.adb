--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                          file : ihm-help_for.adb                           --
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

separate(tui)

procedure Help_For(Command : Command_Type) is
   procedure Header
   is
      Repr   : constant String  := Command_Type'Image(Command);
      Length : constant Natural := (80 - (Repr'Length + 2)) / 2;
      Add    : constant Natural := Repr'Length rem 2;
   begin
      TIO.Put(Ada.Strings.Fixed."*"(Length, '-'));
      TIO.Put(' ' & Repr & ' ');
      TIO.Put_Line(Ada.Strings.Fixed."*"(Length + Add, '-'));
   end;
begin
   Header;
   TIO.Put("Usage: --" & Command_Type'Image(Command));

   case Command is
      when Arity_Zero =>
         TIO.New_Line;

         case Arity_Zero(Command) is
            when QUIT =>
               TIO.Put_Line("quit the program.");
            when RESTART =>
               TIO.Put_Line("restart the program.");
         end case;
      when Arity_ZeroToOne =>
         case Arity_ZeroToOne(Command) is
         when HELP =>
            TIO.Put_Line(" [Command]");
            TIO.New_Line;
            TIO.Put_Line("Display help for given command (or command list if no command given)");
         end case;

      when Arity_One =>
         case Arity_One(Command) is
         when KRI =>
            TIO.Put_Line(" k");
            TIO.Put_Line(ASCII.HT & "k : Natural;");
            TIO.New_Line;
            TIO.Put_Line("Computes k-Reversible Inference for current automaton.");
            TIO.Put_Line("An automaton is k-Reversible when it is a DFA and its reverse is deterministic with a look-ahead of k.");
         end case;

      when Arity_OneToTwo =>
         case Arity_OneToTwo(Command) is
         when WRITE =>
            TIO.Put_Line(" <Output_Format> [File]");
            TIO.New_Line;
            TIO.Put_Line("Allowed values for <Output_Format>:");
            for F in Out_Format loop
               TIO.Put_Line(ASCII.HT & Out_Format'Image(F));
            end loop;
            TIO.New_Line;
            TIO.Put_Line("Outputs the current automaton in File according to <Output_Format>. If File is not given, the automaton is outputed on Standard Output.");
         end case;

      when Arity_Two =>
         case Arity_Two(Command) is
         when READ =>
            TIO.Put_Line(" <File_Format> File");
            TIO.New_Line;
            TIO.Put_Line("Allowed values for <File_Format>:");
            for F in Format_Type loop
               TIO.Put_Line(ASCII.HT & Format_Type'Image(F));
            end loop;
            TIO.New_Line;
            TIO.Put_Line("Reads the automaton contained in File formatted according to <File_Format>.");
         end case;

      when Arity_ThreeToFour =>
         case Arity_ThreeToFour(Command) is
         when APPLY =>
            TIO.Put_Line(" In_File Out_File <Matching_Stategy> [Appendice]");
            TIO.New_Line;
            TIO.Put_Line("Allowed values for <Matching_Stategy>");
            for MS in Matching_Strategy loop
               TIO.Put_Line(ASCII.HT & Matching_Strategy'Image(MS));
            end loop;
            TIO.New_Line;
            TIO.Put("Applyies the current automaton on In_File and outputs result in Out_File according to <Matching_Strategy>.");
            TIO.Put_Line(" Appendice may be added at the end of each matching sequence character. Let Appendice=""-A"", output will be ""B-A"" and ""I-A"" instead of ""B"" and ""I"".");
            TIO.New_Line;
            TIO.Put_Line("In_File Format:");
            TIO.Put_Line("<Word><tabulation><Part-of-Speech><other_informations>");
            TIO.Put_Line("Where <other_informations> is a set of columns seperated by a <tabulation>");
         end case;
   end case;

   TIO.Put_Line("--------------------------------------------------------------------------------");
   TIO.New_Line;
end Help_For;
