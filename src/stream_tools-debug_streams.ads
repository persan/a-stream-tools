-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2015 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
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
with Ada.Text_IO;
package Stream_Tools.Debug_Streams is
   pragma Obsolescent ("Stream_Tools.Debug_Streams: is not implemented yet.");
   --  Note this unit is not implemented.
   type Debug_Stream is abstract new Ada.Streams.Root_Stream_Type with private;

   not overriding procedure Create
     (Stream      : in out Debug_Stream;
      Path        : String);

   not overriding procedure Open
     (Stream      : in out Debug_Stream;
      Path        : String);

   not overriding procedure Put_Line
     (Stream  : in out Debug_Stream;
      Item    : String);

   not overriding procedure New_Line
     (Stream  : in out Debug_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1);

   not overriding procedure Close
     (Stream  : in out Debug_Stream);

   not overriding procedure Write
     (Stream : in out Debug_Stream;
      Item   : Ada.Streams.Stream_Element);

   not overriding procedure Read
     (Stream : in out Debug_Stream;
      Item   : out Ada.Streams.Stream_Element);

   not overriding procedure Put_Header
     (Stream  : in out Debug_Stream);

   not overriding procedure Put_Footer
     (Stream  : in out Debug_Stream);

   overriding procedure Read
     (Stream : in out Debug_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Debug_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

private

   type Debug_Stream is abstract new Ada.Streams.Root_Stream_Type with record
      Target       : Ada.Text_IO.File_Type;
      With_Header  : Boolean := False;
      Mode         : Ada.Text_IO.File_Mode;
      Count        : Natural;
   end record;

   MAX_ELEMENTS_PER_LINE : constant := 16;

end Stream_Tools.Debug_Streams;
