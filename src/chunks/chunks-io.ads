--------------------------------------------------------------------------------
--                                    GAAL                                    --
--                       (Generic Ada Automata Library)                       --
--                                                                            --
--                            file : chunk-io.adb                             --
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

with Ada.Text_IO;

generic
package Chunks.IO is
   procedure Get(F : in Ada.Text_IO.File_Type; Item : out Identifier);

   procedure Put(F : in Ada.Text_IO.File_Type; Item : in Identifier);

   procedure Get(F : in Ada.Text_IO.File_Type; Item : out Position);

   procedure Put(F : in Ada.Text_IO.File_Type; Item : in Position);

   procedure Get(F : in Ada.Text_IO.File_Type; Item : out Chunk);

   procedure Put(F : in Ada.Text_IO.File_Type; Item : in Chunk);

end Chunks.IO;
