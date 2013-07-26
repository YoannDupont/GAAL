--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                        file : ihm.display_help.adb                         --
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

procedure Display_Help
is
   function Description(Command : in Command_Type) return String is
   begin
      case Command is
         when QUIT    => return "quit the program.";
         when RESTART => return "restart the program.";
         when HELP    => return "display help for a command (list commands if none given).";
         when WRITE   => return "write current automaton (stdout if no file given).";
         when READ    => return "read automaton from given file.";
         when KRI     => return "launch K-Reversible Inference on automaton.";
         when APPLY   => return "apply automaton on file.";
      end case;
   end Description;
begin
   TIO.Put_Line("------------------------------------- Help -------------------------------------");
   TIO.Put_Line("Commands are case insensitive.");
   TIO.Put_Line("Display rule ::= --Command[Arity]: Description.");
   TIO.New_Line;
   for C in Command_Type loop
      TIO.Put_Line(ASCII.HT & "--" & Command_Type'Image(C) & Arity_Of(C) & ": " & Description(C));
   end loop;
   TIO.Put_Line("--------------------------------------------------------------------------------");
end Display_Help;
