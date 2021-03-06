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
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Finalization;
package Stream_Tools.Timed_Streams is

   type Timed_Stream is abstract new Ada.Streams.Root_Stream_Type with private;

   not overriding procedure Create
     (Stream      : in out Timed_Stream;
      Path        : String;
      With_Header : Boolean := True);

   not overriding procedure Open
     (Stream      : in out Timed_Stream;
      Path        : String;             --  Path to input file
      Ignore_EOF  : Boolean := False    --  Every read past input data will return 0
      );

   not overriding procedure Put_Line
     (Stream  : in out Timed_Stream;
      Item    : String);

   not overriding procedure New_Line
     (Stream  : in out Timed_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1);

   not overriding procedure Close
     (Stream  : in out Timed_Stream);

   not overriding procedure Write
     (Stream : in out Timed_Stream;
      Item   : Ada.Streams.Stream_Element);

   not overriding procedure Read
     (Stream : in out Timed_Stream;
      Item   : out Ada.Streams.Stream_Element);

   not overriding procedure Put_Header
     (Stream  : in out Timed_Stream);

   not overriding procedure Put_Footer
     (Stream  : in out Timed_Stream);

   overriding procedure Read
     (Stream : in out Timed_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is abstract;

   overriding procedure Write
     (Stream : in out Timed_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is abstract;

private
   type Simple_Event is record
      Time : Ada.Real_Time.Time_Span;
      Data : Ada.Streams.Stream_Element;
   end record;
   type Controler_Type (Controled : not null access Timed_Stream) is new Ada.Finalization.Limited_Controlled with null record;
   package Event_Vectors is new Ada.Containers.Vectors (Positive, Simple_Event);
   type Timed_Stream is abstract new Ada.Streams.Root_Stream_Type with record
      Buffer       : Event_Vectors.Vector;
      Start_Time   : Ada.Real_Time.Time;
      C            : Event_Vectors.Cursor;
      Target       : Ada.Text_IO.File_Type;
      With_Header  : Boolean := False;
      Mode         : Ada.Text_IO.File_Mode;
      Controler    : Controler_Type (Timed_Stream'Access);
      Null_EOF     : Boolean := False;
   end record;
   overriding procedure Initialize (Object : in out Controler_Type);
   overriding procedure Finalize   (Object : in out Controler_Type);

   not overriding procedure Write
     (Stream : in out Timed_Stream);

end Stream_Tools.Timed_Streams;
