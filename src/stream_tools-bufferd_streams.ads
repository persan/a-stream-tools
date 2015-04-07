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
package Stream_Tools.Bufferd_Streams is

   --  This package provides a syncronized FIFO Buffer with a stream interface.
   --  Note that the maximim buffer size is 64K (16#FFFF#).
   use type Ada.Streams.Stream_Element_Count;

   type Bufferd_Stream (Size : Ada.Streams.Stream_Element_Offset) is
     new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) with
     Pre => Item'Length <= Stream.Size;
   --  Read the full Stream_Element_Array, if the buffer does not contain enogh
   --  Elements then wait until enogh space is avalible
   --  Will raise constraint error if the buffer is to small to contain the
   --  total amount of elements.

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array) with
     Pre => Item'Length <= Stream.Size;
   --  Writes the element array to the buffer
   --  If ther is'nt enogh space then wait uintil there is place.
   --  Will raise constraint error if the buffer is to small to contain the
   --  total amount of elements.

   not overriding function GetCount
     (Stream : in out Bufferd_Stream) return Ada.Streams.Stream_Element_Offset;
   --  Returns the number of Stream_Elements in the buffer

   not overriding function Image
     (Stream : in out Bufferd_Stream) return String;
   --  Returns a printable string representing the buffer.

private
   subtype My_Stream_Element_Count is Ada.Streams.Stream_Element_Count range 1 .. 2 ** 16;
   protected type Buffer_Type (Size : Ada.Streams.Stream_Element_Offset) is
      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      entry Write
        (Item   : Ada.Streams.Stream_Element_Array);

      function GetCount return Ada.Streams.Stream_Element_Offset;
      function Image return String;

   private
      procedure H_Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      procedure H_Write
        (Item   : Ada.Streams.Stream_Element_Array);

      entry I_Read (My_Stream_Element_Count)
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      entry I_Write (My_Stream_Element_Count)
        (Item   : Ada.Streams.Stream_Element_Array);

      Buffer         : Ada.Streams.Stream_Element_Array (1 .. Size);
      Read_Cursor    : Ada.Streams.Stream_Element_Offset := 1;
      Write_Cursor   : Ada.Streams.Stream_Element_Offset := 1;
      Count          : Ada.Streams.Stream_Element_Offset := 0;
   end Buffer_Type;

   type Bufferd_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is  new Ada.Streams.Root_Stream_Type with record
      Buffer : Buffer_Type (Size);
   end record;
end Stream_Tools.Bufferd_Streams;
