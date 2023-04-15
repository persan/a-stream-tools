-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2016 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with Ada.Streams;
package Stream_Tools.Stream_Debug_Readers is

   type Stream_Reader (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
     tagged limited private;
   --  Reads from a stream and does a HEX-dump (using GNAT.Memory_Dump)
   --  until the read operation return 0 Stream_Elements, then exist.

   type Text_Stream_Reader (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
     tagged limited private;
   --  Reads from a stream and does a text output (using GNAT.IO)
   --  until the read operation return 0 Stream_Elements, then exist.
   --  Not Printable will be displayed as '.'.

   Printable : constant array (Character) of Boolean
     := [ASCII.LF | ASCII.CR | ASCII.HT | ASCII.VT | ' ' .. '~' => True,
         others                                                 => False];

private

   task type Stream_Reader_Task
     (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
   end Stream_Reader_Task;

   type Stream_Reader (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
     tagged limited record
      Reader :  Stream_Reader_Task (S);
   end record;

   task type Text_Stream_Reader_Task
     (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
   end Text_Stream_Reader_Task;

   type Text_Stream_Reader (S :  not null access Ada.Streams.Root_Stream_Type'Class) is
     tagged limited record
      Reader :  Text_Stream_Reader_Task (S);
   end record;
   procedure Display (Item : Ada.Streams.Stream_Element_Array);
end Stream_Tools.Stream_Debug_Readers;
