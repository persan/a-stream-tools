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
with Ada.Real_Time;
with Ada.Text_IO;
package Stream_Tools.Timed_Output_Streams is

   type Timed_Output_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is new Ada.Streams.Root_Stream_Type with private;

   not overriding procedure Create
     (Stream      : in out Timed_Output_Stream;
      Path        : String;
      Output_Base : Ada.Text_IO.Number_Base := 16;
      With_Header : Boolean := True;
      Append      : Boolean := False;
      Spacing     : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.1));

   not overriding procedure Set_Output_Base
     (Stream  : in out Timed_Output_Stream;
      To      : Ada.Text_IO.Number_Base);

   not overriding procedure Put_Line
     (Stream  : in out Timed_Output_Stream;
      Item    : String);

   not overriding procedure New_Line
     (Stream  : in out Timed_Output_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1);

   not overriding procedure Close
     (Stream  : in out Timed_Output_Stream);

   overriding procedure Read
     (Stream : in out Timed_Output_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is null;

   overriding procedure Write
     (Stream : in out Timed_Output_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   not overriding procedure Flush
     (Stream : in out Timed_Output_Stream);

   not overriding procedure Tick
     (Stream : in out Timed_Output_Stream);

   not overriding procedure Put_Header
     (Stream  : in out Timed_Output_Stream);

   not overriding procedure Put_Footer
     (Stream  : in out Timed_Output_Stream);

private
   type Timed_Output_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is  new Ada.Streams.Root_Stream_Type with record
      Buffer       : Ada.Streams.Stream_Element_Array (1 .. Size);
      Cursor       : Ada.Streams.Stream_Element_Offset;
      Last         : Ada.Streams.Stream_Element_Offset;
      Target       : Ada.Text_IO.File_Type;
      Spacing      : Ada.Real_Time.Time_Span;
      Message_Time : Ada.Real_Time.Time;
      Start_Time   : Ada.Real_Time.Time;
      Output_Base  : Ada.Text_IO.Number_Base;
      With_Header  : Boolean;
   end record;
   not overriding procedure Write
     (Stream : in out Timed_Output_Stream);

end Stream_Tools.Timed_Output_Streams;
